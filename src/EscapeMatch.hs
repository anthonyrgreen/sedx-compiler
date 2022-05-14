module EscapeMatch
    ( escapeLinkedMatch
    -- , escapeMatchLinked
    -- , stateToLinked
    -- , stateToLinkedOptimized
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
import LinkMatch
import OptimizeMatch
import Data.Set as Set
import Data.Foldable as Foldable


escapeLinkedMatch :: LinkedMatch () -> String
escapeLinkedMatch linkedMatch = execWriter $ iterM processLine linkedMatch
  where
    processLine :: LinkedMatchF (Writer String a) -> Writer String a
    processLine (LinkedMatchLiteral literal next) = tell (escapeLiteralNonBracket literal) >> next
    processLine (LinkedMatchUnnamedCaptureGroup nestedDef next) = mkGroup (iterM processLine nestedDef) >> next
    processLine (LinkedMatchNamedCaptureGroup name nestedDef next) = mkGroup (iterM processLine nestedDef) >> next
    processLine (LinkedMatchBuiltInFunc func args next) = processBuiltInFunc func args >> next

processBuiltInFunc :: BuiltInFunc -> LinkedMatch () -> Writer String ()
processBuiltInFunc func args =
  case func of
    Star ->
      let argRep = escapeLinkedMatch args
      -- in if isSingleCharLiteralOrBracket args then tell argRep >> tell "*" else mkGroup1 argRep >> tell "*"
      in tell argRep >> tell "*"
    Maybe ->
      let argRep = escapeLinkedMatch args
      -- in if isSingleCharLiteralOrBracket args then tell argRep >> tell "?" else mkGroup1 argRep >> tell "?"
      in tell argRep >> tell "\\?"
    AnyOf -> do
      tell "["
      expandAndEscapeArgBracket1 args
      tell "]"
    NoneOf -> do
      tell "[^"
      expandAndEscapeArgBracket1 args
      tell "]"

expandAndEscapeArgBracket1 :: LinkedMatch () -> Writer String ()
expandAndEscapeArgBracket1 = iterM processLine
  where
    processLine :: LinkedMatchF (Writer String a) -> Writer String a
    processLine (LinkedMatchLiteral str next) = tell str >> next
    processLine (LinkedMatchUnnamedCaptureGroup linkedMatch next) = tell "CANNOT PUT A LinkedMatchUnnamedCaptureGroup IN A BRACKET" >> next
    processLine (LinkedMatchNamedCaptureGroup name linkedMatch next) = tell "CANNOT PUT A LinkedMatchNamedCaptureGroup IN A BRACKET" >> next
    processLine (LinkedMatchBuiltInFunc func args next) = tell "CANNOT PUT A LinkedMatchBuiltInFunc IN A BRACKET" >> next


escapeFunc :: ProgramAst -> FuncInvocation -> Writer String ()
escapeFunc state func = case func of
  BuiltInFuncInvocation Star args -> do
    mkGroup $ forM args expandAndEscapeArgNonBracket
    tell "*"
  BuiltInFuncInvocation Maybe args -> do
    mkGroup $ forM args expandAndEscapeArgNonBracket
    tell "?"
  BuiltInFuncInvocation AnyOf args -> do
    tell "["
    forM_ args expandAndEscapeArgBracket
    tell "]"
  BuiltInFuncInvocation NoneOf args -> do
    tell "[^"
    forM_ args expandAndEscapeArgBracket
    tell "]"
  UserDefinedFuncInvocation func -> case Map.lookup func $ letDecls state of
    -- TODO: Clean this up
    Nothing -> tell "OOPS I GUESS I COULDN'T FIND THE LET DEF FOR " >> tell func
    Just letDef -> iterM processLine letDef
  where
      processLine :: LetDefF (Writer String a) -> Writer String a
      processLine (LetDefLiteral literal next) = tell (escapeLiteralNonBracket literal) >> next
      processLine (LetDefInvocation funcInvocation next) = escapeFunc state funcInvocation >> next
      processLine (LetDefCaptureInvocation _ def next) = mkGroup (iterM processLine def) >> next
      expandAndEscapeArgNonBracket :: FuncArg -> Writer String ()
      expandAndEscapeArgNonBracket (ArgLiteral literal) = tell $ escapeLiteralNonBracket literal
      expandAndEscapeArgNonBracket (InvocationArg funcInvocation) = escapeFunc state funcInvocation
      expandAndEscapeArgBracket :: FuncArg -> Writer String ()
      -- TODO: actually deal with literal escaping in brackets
      expandAndEscapeArgBracket (ArgLiteral literal) = tell literal
      -- TODO: Should this actually be possible?
      expandAndEscapeArgBracket (InvocationArg funcInvocation) = tell "THIS IS NOT SUPPORTED AT THIS TIME"

-- TODO: We're assuming '/' is the separator character
escapeLiteralNonBracket :: String -> String
escapeLiteralNonBracket = concatMap escapeLiteralNonBracket1
  where
    escapeLiteralNonBracket1 c
      | c `elem` ".[\\*^$/" = '\\':[c]
      | otherwise = [c]


mkGroup :: Writer String a -> Writer String a
mkGroup writer = do
    tell "\\("
    ret <- writer
    tell "\\)"
    return ret

mkGroup1 :: String -> Writer String ()
mkGroup1 string = do
    tell "\\("
    tell string
    tell "\\)"

-- -- NOTE: does not handle collating symbols or equivalence classes
-- charClassEscape :: String -> String
-- charClassEscape 
