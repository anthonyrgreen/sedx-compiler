module Lib
    ( linkAndEscape
    , escapeProgramMatch
    , escapeOptimizedProgramMatch
    , printMatchAndSub
    ) where

import ProgramAst
import ReadProgramAst
import Data.Set as Set
import Utils
import OptimizeMatch
import LinkMatch
import EscapeMatch
import EscapeSub
import LinkSub


linkAndEscape :: ProgramAst -> String
linkAndEscape state = 
  let linkedMatch = linkAstMatch state
  in case linkedMatch of
    Left err -> "Error: " ++ err
    Right linkedMatch -> escapeLinkedMatch (optimizeLinkedMatch Set.empty linkedMatch)

linkAstMatch :: ProgramAst -> Either String (LinkedMatch ())
linkAstMatch programAst = runFreeTExceptT $ linkMatch (letDecls programAst) (matchDef programAst)


repErrorOrSuccess :: Either String String -> String
repErrorOrSuccess (Left err) = "Error: " ++ err
repErrorOrSuccess (Right success) = success

escapeProgramMatch :: Program () -> String
escapeProgramMatch program = repErrorOrSuccess $ do
  ast <- readProgramAst program
  linkedMatch <- linkAstMatch ast
  return $ escapeLinkedMatch linkedMatch
  -- ast 
  --   |> readProgramAst 
  --   |> linkAstMatch 
  --   |> fmap escapeLinkedMatch 
  --   |> repErrorOrSuccess

escapeOptimizedProgramMatch :: Program () -> String
escapeOptimizedProgramMatch program = repErrorOrSuccess $ do
  ast <- readProgramAst program
  linkedMatch <- linkAstMatch ast
  let optimizedLinkedMatch = optimizeLinkedMatch Set.empty linkedMatch
  return $ escapeLinkedMatch optimizedLinkedMatch
  -- ast 
  --   |> readProgramAst 
  --   |> linkAstMatch 
  --   |> fmap (optimizeLinkedMatch Set.empty) 
  --   |> fmap escapeLinkedMatch 
  --   |> repErrorOrSuccess


-- slightly wrong. Capture groups will get screwed up because we decide whether
-- to capture built-in function args on the fly while escaping the match
printMatchAndSub :: Program () -> String
printMatchAndSub program = repErrorOrSuccess $ do
  ast <- readProgramAst program
  unoptimizedLinkedMatch <- linkAstMatch ast
  let optimizedLinkedMatch = optimizeLinkedMatch (listRefsInSub $ subDef ast) unoptimizedLinkedMatch
  linkedSub <- runFreeTExceptT $ linkSub ast optimizedLinkedMatch
  let escapedMatch = escapeLinkedMatch optimizedLinkedMatch
  let escapedSub = escapeLinkedSub linkedSub
  return $ "s/" ++ escapedMatch ++ "/" ++ escapedSub ++ "/g\n"

