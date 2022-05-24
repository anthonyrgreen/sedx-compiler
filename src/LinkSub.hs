{-# LANGUAGE LambdaCase #-}

module LinkSub
  ( linkSub
  , listRefsInSub
  ) where


import           Control.Monad.Except
import           Control.Monad.Morph
import           Control.Monad.State.Lazy
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer.Lazy
import           Data.List
import           Data.Map.Strict            as Map
import           Data.Set                   as Set
import           ProgramAst
import           ReadProgramAst
import           Utils
import Data.Functor.Identity

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
    |> iterM (processLine True)
    |> flip runStateT 1
    |> execWriter
    |> Map.fromList
    where
      processLine :: Bool -> LinkedMatchF Identity (StateT Int (Writer [([String], Int)]) a) -> StateT Int (Writer [([String], Int)]) a
      processLine recordCaptureGroups = \case
        LinkedMatchLiteral literal next -> next
        LinkedMatchUnnamedCaptureGroup nestedDef next -> do
          modify (+1)
          iterM (processLine False) nestedDef
          next
        LinkedMatchNamedCaptureGroup path nestedDef next -> do
          currentCaptureGroup <- get
          when recordCaptureGroups $ tell [(path, currentCaptureGroup)]
          modify (+1)
          iterM (processLine recordCaptureGroups) nestedDef
          next
        LinkedMatchBuiltInFunc0Arg func next -> next
        LinkedMatchBuiltInFunc1Arg func arg0 next -> do
          iterM (processLine recordCaptureGroups) arg0
          next
        -- I think that, for alternatives, this should be different, because they can actually be inside
        -- capture groups
        LinkedMatchBuiltInFunc2Arg func arg0 arg1 next -> do
          iterM (processLine recordCaptureGroups) arg0
          iterM (processLine recordCaptureGroups) arg1
          next
        LinkedMatchBuiltInFunc3Arg func arg0 arg1 arg2 next -> do
          iterM (processLine recordCaptureGroups) arg0
          iterM (processLine recordCaptureGroups) arg1
          iterM (processLine recordCaptureGroups) arg2
          next
          next


getCaptureGroupNum :: LinkedMatch () -> [String] -> Except String Int
getCaptureGroupNum linkedMatch path =
  let pathRep = intercalate "." path
      err = "Could not find referenced capture " ++ pathRep ++ " in match!"
  in linkedMatch
    |> getCaptureGroupNumsByPath
    |> Map.lookup path
    |> hoistMaybeToExceptT err
    >>= \num -> if num > 9
                then throwE ("Cannot reference capture group associated with path '"
                             ++ pathRep ++ "', because sed does not support capture "
                             ++ "groups over \\9, and its capture group would be \\" ++ show num)
                else return num


listRefsInSub :: SubDef () -> Set.Set [String]
listRefsInSub subDef = Set.fromList . execWriter $ iterM processLine subDef
  where
    processLine :: SubDefF (Writer [[String]] a) -> Writer [[String]] a
    processLine (SubLiteral _ next)                   = next
    processLine (SubCaptureReference ref next)        = tell [[ref]] >> next
    processLine (SubScopedCaptureReference refs next) = tell [refs] >> next
