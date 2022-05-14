module OptimizeMatch
    ( optimizeLinkedMatch
    ) where


import ProgramAst
import Data.Set
import Control.Monad
import Control.Monad.Trans.Free
import Utils

optimizeLinkedMatch :: Set [String] -> LinkedMatch () -> LinkedMatch ()
optimizeLinkedMatch namedPaths match =
    match
      |> removeUnusedNamesFromNamedCaptureGroups namedPaths
      |> removeUnneededUnnamedCaptureGroups


removeUnusedNamesFromNamedCaptureGroups :: Set [String] -> LinkedMatch () -> LinkedMatch ()
removeUnusedNamesFromNamedCaptureGroups namedPaths = helper []
  where
    helper existingPath = iterM (processLine existingPath)
    processLine existingPath (LinkedMatchNamedCaptureGroup name nestedDef next) =
      let newPath = existingPath ++ [name]
          nestedDefProcessed = helper newPath nestedDef
          lineProcessed =
            if newPath `member` namedPaths
            then linkedMatchNamedCaptureGroup name nestedDefProcessed
            else linkedMatchUnnamedCaptureGroup nestedDefProcessed
      in lineProcessed >> next
    processLine existingPath (LinkedMatchUnnamedCaptureGroup nestedDef next) =
      let nestedDefProcessed = helper ["This path will never exist"] nestedDef
      in linkedMatchUnnamedCaptureGroup nestedDefProcessed >> next
    processLine existingPath (LinkedMatchBuiltInFunc func nestedDefs next) =
      let nestedDefsProcessed = helper ["This path will never exist"] nestedDefs
      in linkedMatchBuiltInFunc func nestedDefsProcessed >> next
    processLine existingPath (LinkedMatchLiteral literal next) = linkedMatchLiteral literal >> next


removeUnneededUnnamedCaptureGroups :: LinkedMatch () -> LinkedMatch ()
removeUnneededUnnamedCaptureGroups = iterM processLine
  where
    processLine (LinkedMatchNamedCaptureGroup name nestedDef next) =
      let nestedDefProcessed = removeUnneededUnnamedCaptureGroups nestedDef
      in linkedMatchNamedCaptureGroup name nestedDefProcessed >> next
    processLine (LinkedMatchUnnamedCaptureGroup nestedDef next) =
      removeUnneededUnnamedCaptureGroups nestedDef >> next
    processLine (LinkedMatchLiteral literal next) = linkedMatchLiteral literal >> next
    processLine (LinkedMatchBuiltInFunc func nestedDefs next) =
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

