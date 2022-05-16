module EscapeMatch
    ( escapeLinkedMatch
    -- , escapeMatchLinked
    -- , stateToLinked
    -- , stateToLinkedOptimized
    ) where


import           Control.Monad.Except
import           Control.Monad.Morph
import           Control.Monad.State
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer.Lazy
import           Data.Foldable              as Foldable
import           Data.Functor.Identity
import           Data.Map.Strict            as Map
import           Data.Maybe
import           Data.Set                   as Set
import           LinkMatch
import           OptimizeMatch
import           ProgramAst
import           ReadProgramAst
import           Utils
import Flags

escapeLinkedMatch :: SedFlavor -> LinkedMatch () -> String
escapeLinkedMatch sedFlavor linkedMatch = execWriter $ iterM processLine linkedMatch
  where
    processLine :: LinkedMatchF (Writer String a) -> Writer String a
    processLine (LinkedMatchLiteral literal next) = tell (escapeLiteralNonBracket sedFlavor literal) >> next
    processLine (LinkedMatchUnnamedCaptureGroup nestedDef next) = mkGroup sedFlavor (iterM processLine nestedDef) >> next
    processLine (LinkedMatchNamedCaptureGroup name nestedDef next) = mkGroup sedFlavor (iterM processLine nestedDef) >> next
    processLine (LinkedMatchBuiltInFunc func args next) = processBuiltInFunc sedFlavor func args >> next

processBuiltInFunc :: SedFlavor -> BuiltInFunc -> LinkedMatch () -> Writer String ()
processBuiltInFunc sedFlavor func args =
  case func of
    Star ->
      let argRep = escapeLinkedMatch sedFlavor args
      in tell argRep >> tell "*"
    Maybe ->
      let argRep = escapeLinkedMatch sedFlavor args
          opRep = case sedFlavor of
            GNU -> "\\?"
            GNUExtended -> "?"
            BSD -> "\\{0,1\\}"
            BSDExtended -> "?"
      in tell argRep >> tell opRep
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
    processLine (LinkedMatchLiteral str next) = tell (rearrangeStringForBracket str) >> next
    processLine (LinkedMatchUnnamedCaptureGroup linkedMatch next) = tell "CANNOT PUT A LinkedMatchUnnamedCaptureGroup IN A BRACKET" >> next
    processLine (LinkedMatchNamedCaptureGroup name linkedMatch next) = tell "CANNOT PUT A LinkedMatchNamedCaptureGroup IN A BRACKET" >> next
    processLine (LinkedMatchBuiltInFunc func args next) = tell "CANNOT PUT A LinkedMatchBuiltInFunc IN A BRACKET" >> next

rearrangeStringForBracket :: String -> String
rearrangeStringForBracket str = if ']' `elem` str then ']' : Prelude.filter (/= ']') str else str

-- TODO: We're assuming '/' is the separator character
escapeLiteralNonBracket :: SedFlavor -> String -> String
escapeLiteralNonBracket sedFlavor = concatMap escapeLiteralNonBracket1
  where
    escapeLiteralNonBracket1 c
      | c `elem` specialChars = '\\':[c]
      | otherwise = [c]
    specialChars = case sedFlavor of
      GNU -> ".[\\*^$/"
      GNUExtended -> ".[\\*^$/?+(){}|"
      BSD -> ".[\\*^$/"
      BSDExtended -> ".[\\*^$/?+(){}|"



mkGroup :: SedFlavor -> Writer String a -> Writer String a
mkGroup sedFlavor writer = do
    let (openParens, closeParens) = case sedFlavor of
            GNU -> ("\\(", "\\)")
            GNUExtended -> ("(", ")")
            BSD -> ("\\(", "\\)")
            BSDExtended -> ("(", ")")
    tell openParens
    ret <- writer
    tell closeParens
    return ret

-- -- NOTE: does not handle collating symbols or equivalence classes
-- charClassEscape :: String -> String
-- charClassEscape
