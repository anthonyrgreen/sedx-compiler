{-# LANGUAGE ExistentialQuantification #-}


module ReadProgramAst
  ( readProgramAst
  , hasSubstitute
  , ProgramAst(..)
  ) where


import ProgramAst
import Data.Map.Strict as Map
import Data.List as List
import Control.Monad.Trans.Free
import Control.Monad.State
import Data.Maybe
import Control.Arrow
import Data.Functor.Identity
import Control.Monad.Writer.Lazy
import Utils
import PrintProgram
import Control.Monad.Trans.Except


readProgramAst :: Program a -> Either String ProgramAst
readProgramAst program = runExcept $ execStateT (iterM readLine program) emptyState
  where
    readLine :: ProgramF (StateT ProgramAst (Except String) a) -> StateT ProgramAst (Except String) a
    readLine (LetDecl name def next) = readLetDef name def >> next
    readLine (Match match next) = readMatch match >> next
    readLine (Substitute substitute next) = readSubstitute substitute >> next


data ProgramAst = ProgramAst { letDecls :: Map.Map String (LetDef ())
                             , matchDef :: MatchDef ()
                             , subDef :: SubDef ()
                             }


emptyState = ProgramAst Map.empty (return ()) (return ())


instance Show ProgramAst where
  show (ProgramAst letDecls matchDef subDef) =
    "LetDecls:\n" ++ showLetDecls letDecls
      ++ "MatchDef:\n" ++ showMatchDef matchDef
      ++ "SubDef:\n" ++ showSubDef subDef
      ++ "\n"
    where
      showLetDecls letDecls =
        (letDecls |> (Map.map showLetDef >>> Map.toList >>> concatMap (\elem -> "\t" ++ show elem))) ++ "\n"


readLetDef :: String -> LetDef () -> StateT ProgramAst (Except String) ()
readLetDef name def = do
  defAlreadyExists <- gets (letDecls >>> member name)
  when defAlreadyExists $ lift . throwE $ "Cannot redefine let declaration '" ++ name ++ "' because it already exists."
  let letDirectDependencies = getLetDirectDependencies def
  let allDependenciesDefined decls = all (`member` decls) letDirectDependencies
  allDepsDefined <- gets (letDecls >>> allDependenciesDefined)
  unless allDepsDefined $ lift . throwE $ "Some dependencies are not defined!"
  modify (\state -> state { letDecls = Map.insert name def (letDecls state) })


readMatch :: MatchDef () -> StateT ProgramAst (Except String) ()
readMatch match = do
  matchDef <- gets matchDef
  unless (matchDef == return ()) $ lift . throwE $ "Cannot redefine match! It has already been defined."
  modify (\state -> state { matchDef = match })


readSubstitute :: SubDef () -> StateT ProgramAst (Except String) ()
readSubstitute substitute = do
  subDef <- gets subDef
  unless (subDef == return ()) $ lift . throwE $ "Cannot redefine substitute! It has already been defined."
  modify (\state -> state { subDef = substitute })


getLetDirectDependencies :: LetDef a -> [String]
getLetDirectDependencies letDef = iterM writeLet letDef |> execWriter |> List.nub
  where
    writeLet :: LetDefF (Writer [String] a) -> Writer [String] a
    writeLet line = case line of
      LetDefInvocation func next -> tell (getFuncInvocationDependencies func) >> next
      LetDefCaptureInvocation _ subexpression next -> tell (getLetDirectDependencies subexpression) >> next
      LetDefLiteral _ next -> next


getFuncInvocationDependencies :: FuncInvocation -> [String]
getFuncInvocationDependencies (BuiltInFuncInvocation _ args) = concatMap getArgDependencies args
getFuncInvocationDependencies (UserDefinedFuncInvocation func) = [func]


getArgDependencies :: FuncArg -> [String]
getArgDependencies (ArgLiteral _) = []
getArgDependencies (InvocationArg funcInvocation) = getFuncInvocationDependencies funcInvocation


hasSubstitute :: ProgramAst -> Bool
hasSubstitute ast = subDef ast == return ()
