-- module Lib
--     ( linkAndEscape
--     , escapeProgramMatch
--     , escapeOptimizedProgramMatch
--     , printMatchAndSub
--     , repErrorOrSuccess
--     ) where
module Lib where

import           Data.Set       as Set
import           EscapeMatch
import           EscapeSub
import           Flags
import           LinkMatch
import           LinkSub
import           OptimizeMatch
import           ProgramAst
import           ReadProgramAst
import           Utils

import Data.Functor.Identity
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Free
import Data.Functor ((<&>))


linkAndEscape :: SedFlavor -> ProgramAst -> String
linkAndEscape sedFlavor state = linkAstMatch state 
                                <&> optimizeLinkedMatch Set.empty 
                                <&> escapeLinkedMatch sedFlavor 
                                |> either ("Error: " ++) runIdentity

linkAstMatch :: ProgramAst -> Either String (LinkedMatch ())
linkAstMatch programAst = linkMatch0 (letDecls programAst) (matchDef programAst)


repErrorOrSuccess :: Show a => Either a String -> String
repErrorOrSuccess (Left err)      = "Error: " ++ show err
repErrorOrSuccess (Right success) = success

repStrErrorOrSuccess :: Either String String -> String
repStrErrorOrSuccess (Left err)      = "Error: " ++ err
repStrErrorOrSuccess (Right success) = success

escapeProgramMatch :: SedFlavor -> Program () -> String
escapeProgramMatch sedFlavor program = repErrorOrSuccess $ do
  ast <- readProgramAst program
  linkedMatch <- linkAstMatch ast
  return $ runIdentity $ escapeLinkedMatch sedFlavor linkedMatch


escapeOptimizedProgramMatch :: SedFlavor -> Program () -> String
escapeOptimizedProgramMatch sedFlavor program = repErrorOrSuccess $ do
  ast <- readProgramAst program
  linkedMatch <- linkAstMatch ast
  let optimizedLinkedMatch = optimizeLinkedMatch Set.empty linkedMatch
  return $ runIdentity $ escapeLinkedMatch sedFlavor optimizedLinkedMatch


-- slightly wrong. Capture groups will get screwed up because we decide whether
-- to capture built-in function args on the fly while escaping the match
printMatchAndSub :: SedFlavor -> Program () -> String
printMatchAndSub sedFlavor program = repStrErrorOrSuccess $ do
  ast <- readProgramAst program
  unoptimizedLinkedMatch <- linkAstMatch ast
  let optimizedLinkedMatch = optimizeLinkedMatch (listRefsInSub $ subDef ast) unoptimizedLinkedMatch
  linkedSub <- runFreeTExceptT $ linkSub ast optimizedLinkedMatch
  let escapedMatch = runIdentity $ escapeLinkedMatch sedFlavor optimizedLinkedMatch
  let escapedSub = escapeLinkedSub linkedSub
  return $ "s/" ++ escapedMatch ++ "/" ++ escapedSub ++ "/g"
