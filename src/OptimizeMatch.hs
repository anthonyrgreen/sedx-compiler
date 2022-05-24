{-# LANGUAGE LambdaCase #-}

module OptimizeMatch
    ( optimizeLinkedMatch
    ) where


import           Control.Monad
import           Control.Monad.Trans.Free
import           Data.Set
import           ProgramAst
import           Utils
import  Control.Monad.Trans (lift)

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
        in lineProcessed                                                                         >> next
      LinkedMatchUnnamedCaptureGroup nestedDef next ->
        let nestedDefProcessed = iterM processLine nestedDef
        in linkedMatchUnnamedCaptureGroup nestedDefProcessed                                     >> next
      LinkedMatchBuiltInFunc0Arg func next ->
        linkedMatchBuiltInFunc0Arg func                                                          >> next
      LinkedMatchBuiltInFunc1Arg func arg0Defs next ->
        let arg0DefsProcessed = iterM processLine arg0Defs
        in linkedMatchBuiltInFunc1Arg func arg0DefsProcessed                                     >> next
      LinkedMatchBuiltInFunc2Arg func arg0Defs arg1Defs next ->
        let arg0DefsProcessed = iterM processLine arg0Defs
            arg1DefsProcessed = iterM processLine arg1Defs
        in linkedMatchBuiltInFunc2Arg func arg0DefsProcessed arg1DefsProcessed                   >> next
      LinkedMatchBuiltInFunc3Arg func arg0Defs arg1Defs arg2Defs next ->
        let arg0DefsProcessed = iterM processLine arg0Defs
            arg1DefsProcessed = iterM processLine arg1Defs
            arg2DefsProcessed = iterM processLine arg2Defs
        in linkedMatchBuiltInFunc3Arg func arg0DefsProcessed arg1DefsProcessed arg2DefsProcessed >> next
      LinkedMatchLiteral literal next -> linkedMatchLiteral literal                              >> next


-- TODO: I'm not enforcing function arg types. E.G. "Between" accepts capture groups for its range inputs,
--   which makes no sense.
removeUnneededUnnamedCaptureGroups :: Monad m => LinkedMatchT m () -> LinkedMatchT m ()
removeUnneededUnnamedCaptureGroups = iterTM processLine
  where
    processLine = \case
      LinkedMatchNamedCaptureGroup path nestedDef next ->
        let nestedDefProcessed = removeUnneededUnnamedCaptureGroups nestedDef
        in linkedMatchNamedCaptureGroup path nestedDefProcessed >> next
      LinkedMatchUnnamedCaptureGroup nestedDef next ->
        removeUnneededUnnamedCaptureGroups nestedDef >> next
      LinkedMatchLiteral literal next -> linkedMatchLiteral literal >> next
      LinkedMatchBuiltInFunc0Arg func next -> linkedMatchBuiltInFunc0Arg func >> next
      LinkedMatchBuiltInFunc1Arg func arg0 next -> do
        doesNotNeedAGroup <- lift $ isSingleCharLiteralOrBracket arg0
        let
          funcWithGroups = linkedMatchBuiltInFunc1Arg func arg0
          funcWithoutGroups = linkedMatchBuiltInFunc1Arg func (removeUnneededUnnamedCaptureGroups arg0)
        case func of
          Star -> (if doesNotNeedAGroup then funcWithoutGroups else funcWithGroups) >> next
          Plus -> (if doesNotNeedAGroup then funcWithoutGroups else funcWithGroups) >> next
          Maybe -> (if doesNotNeedAGroup then funcWithoutGroups else funcWithGroups) >> next
          AnyOf -> funcWithoutGroups >> next
          NoneOf -> funcWithoutGroups >> next
      LinkedMatchBuiltInFunc2Arg func arg0 arg1 next -> do
        doesNotNeedAGroup <- lift $ isSingleCharLiteralOrBracket arg1
        let
          funcWithGroups = linkedMatchBuiltInFunc2Arg func arg0 arg1
          funcWithoutGroups = linkedMatchBuiltInFunc2Arg func (removeUnneededUnnamedCaptureGroups arg0) (removeUnneededUnnamedCaptureGroups arg1)
        case func of
          AtMost -> (if doesNotNeedAGroup then funcWithoutGroups else funcWithGroups) >> next
          AtLeast -> (if doesNotNeedAGroup then funcWithoutGroups else funcWithGroups) >> next
      LinkedMatchBuiltInFunc3Arg func arg0 arg1 arg2 next -> do
        doesNotNeedAGroup <- lift $ isSingleCharLiteralOrBracket arg2
        let
          funcWithGroups = linkedMatchBuiltInFunc3Arg func arg0 arg1 arg2
          funcWithoutGroups = linkedMatchBuiltInFunc3Arg func (removeUnneededUnnamedCaptureGroups arg0) (removeUnneededUnnamedCaptureGroups arg1) (removeUnneededUnnamedCaptureGroups arg2)
        case func of
          Between -> (if doesNotNeedAGroup then funcWithoutGroups else funcWithGroups) >> next

