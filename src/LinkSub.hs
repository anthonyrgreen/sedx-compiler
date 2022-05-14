module LinkSub
  ( linkSub
  , listRefsInSub
  ) where


import ProgramAst
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import ReadProgramAst
import Control.Monad.Trans.Free
import Control.Monad.Morph
import Data.Set as Set
import Control.Monad.Writer.Lazy
import Control.Monad.State.Lazy
import Data.List
import Data.Map.Strict as Map
import Utils


linkSub :: ProgramAst -> LinkedMatch () -> LinkedSubT (Except String) ()
linkSub programAst linkedMatch = iterM processLine (subDef programAst)
  where
    processLine :: SubDefF (LinkedSubT (Except String) ()) -> LinkedSubT (Except String) ()
    processLine (SubLiteral literal next) = linkedSubLiteral literal >> next
    processLine (SubCaptureReference refName next) = do
      captureGroupNum <- lift $ getCaptureGroupNum linkedMatch [refName]
      linkedSubBackReference captureGroupNum
      next
    processLine (SubScopedCaptureReference refNames next) = do
      captureGroupNum <- lift $ getCaptureGroupNum linkedMatch refNames
      linkedSubBackReference captureGroupNum
      next


getCaptureGroupNumsByPath :: LinkedMatch () -> Map.Map [String] Int
getCaptureGroupNumsByPath linkedMatch =
  linkedMatch
    |> iterM (processLine [] True)
    |> flip runStateT 1
    |> execWriter
    |> Map.fromList
    where
      processLine :: [String] -> Bool -> LinkedMatchF (StateT Int (Writer [([String], Int)]) a) -> StateT Int (Writer [([String], Int)]) a
      processLine currentPath recordCaptureGroups linkedMatch = case linkedMatch of
        LinkedMatchLiteral literal next  -> next
        LinkedMatchUnnamedCaptureGroup nestedDef next -> do
          modify (+1)
          iterM (processLine [] False) nestedDef
          next
        LinkedMatchNamedCaptureGroup refName nestedDef next -> do
          let newPath = currentPath ++ [refName]
          currentCaptureGroup <- get
          when recordCaptureGroups $ tell [(newPath, currentCaptureGroup)]
          modify (+1)
          iterM (processLine newPath recordCaptureGroups) nestedDef
          next
        LinkedMatchBuiltInFunc func funcArgs next -> do
          iterM (processLine currentPath recordCaptureGroups) funcArgs
          next

getCaptureGroupNum :: LinkedMatch () -> [String] -> Except String Int
getCaptureGroupNum linkedMatch path =
  let err = "Could not find referenced capture " ++ intercalate "." path ++ " in match!"
  in linkedMatch
    |> getCaptureGroupNumsByPath
    |> Map.lookup path
    |> hoistMaybeToExceptT err


listRefsInSub :: SubDef () -> Set.Set [String]
listRefsInSub subDef = Set.fromList . execWriter $ iterM processLine subDef
  where
    processLine :: SubDefF (Writer [[String]] a) -> Writer [[String]] a
    processLine (SubLiteral _ next) = next
    processLine (SubCaptureReference ref next) = tell [[ref]] >> next
    processLine (SubScopedCaptureReference refs next) = tell (inits refs) >> next
