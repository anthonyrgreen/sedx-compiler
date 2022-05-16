{-# LANGUAGE LambdaCase #-}

module OptimizeMatch
    ( optimizeLinkedMatch
    ) where


import           Control.Monad
import           Control.Monad.Trans.Free
import           Data.Set
import           ProgramAst
import           Utils

optimizeLinkedMatch :: Set [String] -> LinkedMatch () -> LinkedMatch ()
optimizeLinkedMatch namedPaths match =
    match
      |> removeUnusedNamesFromNamedCaptureGroups namedPaths
      |> removeUnneededUnnamedCaptureGroups


removeUnusedNamesFromNamedCaptureGroups :: Set [String] -> LinkedMatch () -> LinkedMatch ()
removeUnusedNamesFromNamedCaptureGroups namedPaths = iterM processLine
  where
    processLine = \case
      LinkedMatchNamedCaptureGroup path nestedDef next ->
        let nestedDefProcessed = iterM processLine nestedDef
            lineProcessed =
              if path `member` namedPaths
              then linkedMatchNamedCaptureGroup path nestedDefProcessed
              else linkedMatchUnnamedCaptureGroup nestedDefProcessed
        in lineProcessed >> next
      LinkedMatchUnnamedCaptureGroup nestedDef next -> 
        let nestedDefProcessed = iterM processLine nestedDef
        in linkedMatchUnnamedCaptureGroup nestedDefProcessed >> next
      LinkedMatchBuiltInFunc func nestedDefs next -> 
        let nestedDefsProcessed = iterM processLine nestedDefs
        in linkedMatchBuiltInFunc func nestedDefsProcessed >> next
      LinkedMatchLiteral literal next -> linkedMatchLiteral literal >> next


removeUnneededUnnamedCaptureGroups :: LinkedMatch () -> LinkedMatch ()
removeUnneededUnnamedCaptureGroups = iterM processLine
  where
    processLine = \case
      LinkedMatchNamedCaptureGroup path nestedDef next ->
        let nestedDefProcessed = removeUnneededUnnamedCaptureGroups nestedDef
        in linkedMatchNamedCaptureGroup path nestedDefProcessed >> next
      LinkedMatchUnnamedCaptureGroup nestedDef next ->
        removeUnneededUnnamedCaptureGroups nestedDef >> next
      LinkedMatchLiteral literal next -> linkedMatchLiteral literal >> next
      LinkedMatchBuiltInFunc func nestedDefs next ->
        let
          removeNestedUnnamedCaptureGroups = isSingleCharLiteralOrBracket nestedDefs
          builtInFuncUnprocessed = linkedMatchBuiltInFunc func nestedDefs
          builtInFuncProcessed = linkedMatchBuiltInFunc func (removeUnneededUnnamedCaptureGroups nestedDefs)
        in
          case func of
            Star -> (if removeNestedUnnamedCaptureGroups then builtInFuncProcessed else builtInFuncUnprocessed) >> next
            Maybe -> (if removeNestedUnnamedCaptureGroups then builtInFuncProcessed else builtInFuncUnprocessed) >> next
            AnyOf -> builtInFuncProcessed >> next
            NoneOf -> builtInFuncProcessed >> next
  
