module LinkMatch 
    ( linkMatch
    ) where


import ProgramAst
import ReadProgramAst
import Control.Monad.Writer.Lazy
import Control.Monad.Trans.Free
import Control.Monad.State
import Data.Map.Strict as Map
import Data.Maybe
import Data.Functor.Identity
import Utils
import Control.Monad.Trans
import Control.Monad.Morph
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe


linkMatch :: Map.Map String (LetDef ()) -> MatchDef () -> LinkedMatchT (Except String) ()
linkMatch letDeclsByName = iterM processMatchDef
  where
    processMatchDef :: MatchDefF (LinkedMatchT (Except String) ()) -> LinkedMatchT (Except String) ()
    processMatchDef matchStatement = case matchStatement of
      MatchLiteral literal next -> linkedMatchLiteral literal >> next
      MatchInvocation func next -> linkFuncInvocation letDeclsByName func >> next
      MatchCaptureInvocation name nestedDef next -> do
        let linkedCaptureInvocation = linkMatch letDeclsByName nestedDef
        applyExcept linkedCaptureInvocation (linkedMatchNamedCaptureGroup name)
        next


linkFuncInvocation :: Map.Map String (LetDef ()) -> FuncInvocation -> LinkedMatchT (Except String) ()
linkFuncInvocation letDeclsByName func = case func of
  UserDefinedFuncInvocation funcName -> do
    letDecl <- Map.lookup funcName letDeclsByName 
           |> hoistMaybeToExceptT ("During linking: could not find a definition for " ++ funcName)
           |> lift
    iterM (linkLet letDeclsByName) letDecl
  BuiltInFuncInvocation func args -> do
    linkedArgs <- lift $ linkArgs letDeclsByName args
    let concattedLinkedArgs = concatMonad linkedArgs
    -- TODO: Potential bug: what if the argument is itself a named capture group?
    let processedArgs = if isSingleCharLiteralOrBracket concattedLinkedArgs 
                        then concattedLinkedArgs
                        else linkedMatchUnnamedCaptureGroup concattedLinkedArgs 
    case func of
      AnyOf -> linkedMatchBuiltInFunc AnyOf concattedLinkedArgs
      NoneOf -> linkedMatchBuiltInFunc NoneOf concattedLinkedArgs
      Star -> linkedMatchBuiltInFunc Star processedArgs
      Maybe -> linkedMatchBuiltInFunc Maybe processedArgs


linkLet :: Map.Map String (LetDef ()) -> LetDefF (LinkedMatchT (Except String) ()) -> LinkedMatchT (Except String) ()
linkLet letDeclsByName letDefStatement = case letDefStatement of
  LetDefLiteral literal next -> linkedMatchLiteral literal >> next
  LetDefInvocation func next -> linkFuncInvocation letDeclsByName func >> next
  LetDefCaptureInvocation name nestedDef next -> do
    let linkedLet = iterM (linkLet letDeclsByName) nestedDef
    applyExcept linkedLet (linkedMatchNamedCaptureGroup name)
    next


linkArgs :: Map.Map String (LetDef ()) -> [FuncArg] -> Except String [LinkedMatch ()]
linkArgs letDeclsByName funcArgs = mapM linkArg funcArgs
  where
    linkArg :: FuncArg -> Except String (LinkedMatch ())
    linkArg (ArgLiteral string) = return $ linkedMatchLiteral string
    linkArg (InvocationArg func) = joinFreeT $ linkFuncInvocation letDeclsByName func

