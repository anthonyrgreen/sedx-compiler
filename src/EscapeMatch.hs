module EscapeMatch
    ( escapeLinkedMatch
    -- , escapeMatchLinked
    -- , stateToLinked
    -- , stateToLinkedOptimized
    ) where


import Control.Monad.Free (foldFree)
import           Control.Monad.Except
import           Control.Monad.Morph
import           Control.Monad.State
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer.Lazy (Writer, WriterT, tell, execWriterT)
import           Data.Foldable              as Foldable
import           Data.Functor.Identity
import           Data.Map.Strict            as Map
import           Data.Maybe
import           Data.Set                   as Set
import           Flags
import           LinkMatch
import           OptimizeMatch
import           ProgramAst
import           ReadProgramAst
import           Utils


escapeLinkedMatch :: Monad m => SedFlavor -> LinkedMatchT m () -> m String
escapeLinkedMatch sedFlavor linkedMatch = execWriterT $ iterTM processLine linkedMatch
  where
    processLine :: Monad m => LinkedMatchF m (WriterT String m a) -> WriterT String m a
    processLine (LinkedMatchLiteral literal next) = tell (escapeLiteralNonBracket sedFlavor literal) >> next
    processLine (LinkedMatchUnnamedCaptureGroup nestedDef next) = mkGroup sedFlavor (iterTM processLine nestedDef) >> next
    processLine (LinkedMatchNamedCaptureGroup name nestedDef next) = mkGroup sedFlavor (iterTM processLine nestedDef) >> next
    processLine (LinkedMatchBuiltInFunc0Arg func next) = processBuiltInFunc0Arg sedFlavor func >> next
    processLine (LinkedMatchBuiltInFunc1Arg func arg0 next) = processBuiltInFunc1Arg sedFlavor func arg0 >> next
    processLine (LinkedMatchBuiltInFunc2Arg func arg0 arg1 next) = processBuiltInFunc2Arg sedFlavor func arg0 arg1 >> next
    processLine (LinkedMatchBuiltInFunc3Arg func arg0 arg1 arg2 next) = processBuiltInFunc3Arg sedFlavor func arg0 arg1 arg2 >> next


processBuiltInFunc0Arg :: Monad m => SedFlavor -> BuiltInFunc0Arg -> WriterT String m ()
processBuiltInFunc0Arg sedFlavor func = case func of
    Any -> tell "."
    StartLine -> tell "^"
    EndLine -> tell "$"


processBuiltInFunc1Arg :: Monad m => SedFlavor -> BuiltInFunc1Arg -> LinkedMatchT m () -> WriterT String m ()
processBuiltInFunc1Arg sedFlavor func arg0 =
  case func of
    Star -> do
      argRep <- lift $ escapeLinkedMatch sedFlavor arg0
      tell argRep
      tell "*"
    Plus -> do
      argRep <- lift $ escapeLinkedMatch sedFlavor arg0
      let opRep = case sedFlavor of
            GNU         -> "\\+"
            GNUExtended -> "+"
            BSD         -> "\\{1,\\}"
            BSDExtended -> "+"
      tell argRep
      tell opRep
    Maybe -> do
      argRep <- lift $ escapeLinkedMatch sedFlavor arg0
      let opRep = case sedFlavor of
            GNU         -> "\\?"
            GNUExtended -> "?"
            BSD         -> "\\{0,1\\}"
            BSDExtended -> "?"
      tell argRep
      tell opRep
    AnyOf -> do
      tell "["
      expandAndEscapeArgBracket1 arg0
      tell "]"
    NoneOf -> do
      tell "[^"
      expandAndEscapeArgBracket1 arg0
      tell "]"


processBuiltInFunc2Arg :: Monad m => SedFlavor -> BuiltInFunc2Arg -> LinkedMatchT m () -> LinkedMatchT m () -> WriterT String m ()
processBuiltInFunc2Arg sedFlavor func arg0 arg1 =
  case func of
    AtMost -> do
      let opRepOpen = case sedFlavor of
            GNU         -> "\\{,"
            GNUExtended -> "{,"
            BSD         -> "\\{0,"
            BSDExtended -> "{0,"
      let opRepClose = case sedFlavor of
            GNU         -> "\\}"
            GNUExtended -> "}"
            BSD         -> "\\}"
            BSDExtended -> "}"
      arg0Rep <- lift $ escapeLinkedMatch sedFlavor arg0
      arg1Rep <- lift $ escapeLinkedMatch sedFlavor arg1
      tell arg1Rep
      tell opRepOpen
      tell arg0Rep
      tell opRepClose
    AtLeast -> do
      let opRepOpen = case sedFlavor of
            GNU         -> "\\{"
            GNUExtended -> "{"
            BSD         -> "\\{"
            BSDExtended -> "{"
      let opRepClose = case sedFlavor of
            GNU         -> ",\\}"
            GNUExtended -> ",}"
            BSD         -> ",\\}"
            BSDExtended -> ",}"
      arg0Rep <- lift $ escapeLinkedMatch sedFlavor arg0
      arg1Rep <- lift $ escapeLinkedMatch sedFlavor arg1
      tell arg1Rep
      tell opRepOpen
      tell arg0Rep
      tell opRepClose

processBuiltInFunc3Arg :: Monad m => SedFlavor -> BuiltInFunc3Arg -> LinkedMatchT m () -> LinkedMatchT m () -> LinkedMatchT m () -> WriterT String m ()
processBuiltInFunc3Arg sedFlavor func arg0 arg1 arg2 =
  case func of
    Between -> do
      let opRepOpen = case sedFlavor of
            GNU         -> "\\{"
            GNUExtended -> "{"
            BSD         -> "\\{"
            BSDExtended -> "{"
      let opRepClose = case sedFlavor of
            GNU         -> "\\}"
            GNUExtended -> "}"
            BSD         -> "\\}"
            BSDExtended -> "}"
      arg0Rep <- lift $ escapeLinkedMatch sedFlavor arg0
      arg1Rep <- lift $ escapeLinkedMatch sedFlavor arg1
      arg2Rep <- lift $ escapeLinkedMatch sedFlavor arg2
      tell arg2Rep
      tell opRepOpen
      tell arg0Rep
      tell ","
      tell arg1Rep
      tell opRepClose


expandAndEscapeArgBracket1 :: Monad m => LinkedMatchT m () -> WriterT String m ()
expandAndEscapeArgBracket1 = iterTM processLine
  where
    processLine :: Monad m => LinkedMatchF m (WriterT String m a) -> WriterT String m a
    processLine (LinkedMatchLiteral str next) = tell (rearrangeStringForBracket str) >> next
    processLine (LinkedMatchUnnamedCaptureGroup linkedMatch next) = tell "CANNOT PUT A LinkedMatchUnnamedCaptureGroup IN A BRACKET" >> next
    processLine (LinkedMatchNamedCaptureGroup name linkedMatch next) = tell "CANNOT PUT A LinkedMatchNamedCaptureGroup IN A BRACKET" >> next
    processLine (LinkedMatchBuiltInFunc0Arg _ next) = tell "CANNOT PUT A LinkedMatchBuiltInFunc IN A BRACKET" >> next
    processLine (LinkedMatchBuiltInFunc1Arg _ _ next) = tell "CANNOT PUT A LinkedMatchBuiltInFunc IN A BRACKET" >> next
    processLine (LinkedMatchBuiltInFunc2Arg _ _ _ next) = tell "CANNOT PUT A LinkedMatchBuiltInFunc IN A BRACKET" >> next
    processLine (LinkedMatchBuiltInFunc3Arg _ _ _ _ next) = tell "CANNOT PUT A LinkedMatchBuiltInFunc IN A BRACKET" >> next


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
      GNU         -> ".[\\*^$/"
      GNUExtended -> ".[\\*^$/?+(){}|"
      BSD         -> ".[\\*^$/"
      BSDExtended -> ".[\\*^$/?+(){}|"



mkGroup :: Monad m => SedFlavor -> WriterT String m a -> WriterT String m a
mkGroup sedFlavor writer = do
    let (openParens, closeParens) = case sedFlavor of
            GNU         -> ("\\(", "\\)")
            GNUExtended -> ("(", ")")
            BSD         -> ("\\(", "\\)")
            BSDExtended -> ("(", ")")
    tell openParens
    ret <- writer
    tell closeParens
    return ret


-- -- NOTE: does not handle collating symbols or equivalence classes
-- charClassEscape :: String -> String
-- charClassEscape
