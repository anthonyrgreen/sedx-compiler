{-# LANGUAGE LambdaCase, BlockArguments #-}

module LinkMatch
    ( linkMatch
    , linkMatch0
    ) where


import           Control.Monad.Except
import           Control.Monad.Morph
import           Control.Monad.State
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
-- import           Control.Monad.Writer.Lazy ()
import           Data.Functor.Identity
import           Data.Map.Strict            as Map
import           Data.Maybe
import           ProgramAst
import           ReadProgramAst
import           Utils


type ReaderStack = ReaderT Config (LinkedMatchT (Except String))
data Config = Config { getLetDeclsByName :: Map.Map String (LetDef ()), getPath :: [String] }


sequenceLinkedMatchTErrors :: LinkedMatchT (Except String) () -> Except String (LinkedMatch ())
sequenceLinkedMatchTErrors = joinFreeT . sequenceNestedExcepts
  where
    sequenceNestedExcepts :: LinkedMatchT (Except String) a -> FreeT (LinkedMatchF Identity) (Except String) a
    sequenceNestedExcepts = iterTM \case
      LinkedMatchNamedCaptureGroup path nestedDef next -> do
        pNestedDef <- lift $ sequenceLinkedMatchTErrors nestedDef
        hoistFreeT generalize $ linkedMatchNamedCaptureGroup path pNestedDef
        next
      LinkedMatchUnnamedCaptureGroup nestedDef next -> do
        pNestedDef <- lift $ sequenceLinkedMatchTErrors nestedDef
        hoistFreeT generalize $ linkedMatchUnnamedCaptureGroup pNestedDef
        next
      LinkedMatchBuiltInFunc0Arg func next -> do
        hoistFreeT generalize $ linkedMatchBuiltInFunc0Arg func
        next
      LinkedMatchBuiltInFunc1Arg func arg0 next -> do
        sequencedArg0 <- lift $ sequenceLinkedMatchTErrors arg0
        hoistFreeT generalize $ linkedMatchBuiltInFunc1Arg func sequencedArg0
        next
      LinkedMatchBuiltInFunc2Arg func arg0 arg1 next -> do
        sequencedArg0 <- lift $ sequenceLinkedMatchTErrors arg0
        sequencedArg1 <- lift $ sequenceLinkedMatchTErrors arg1
        hoistFreeT generalize $ linkedMatchBuiltInFunc2Arg func sequencedArg0 sequencedArg1
        next
      LinkedMatchBuiltInFunc3Arg func arg0 arg1 arg2 next -> do
        sequencedArg0 <- lift $ sequenceLinkedMatchTErrors arg0
        sequencedArg1 <- lift $ sequenceLinkedMatchTErrors arg1
        sequencedArg2 <- lift $ sequenceLinkedMatchTErrors arg2
        hoistFreeT generalize $ linkedMatchBuiltInFunc3Arg func sequencedArg0 sequencedArg1 sequencedArg2
        next
      LinkedMatchLiteral literal next -> do
        hoistFreeT generalize $ linkedMatchLiteral literal
        next


linkMatch0 :: Map.Map String (LetDef ()) -> MatchDef () -> Either String (LinkedMatch ())
linkMatch0 letDeclsByName matchDef = runExcept . sequenceLinkedMatchTErrors $ linkMatch letDeclsByName matchDef

linkMatch :: Map.Map String (LetDef ()) -> MatchDef () -> LinkedMatchT (Except String) ()
linkMatch letDeclsByName matchDef = runReaderT (linkMatch1 matchDef) config
  where
    config = Config letDeclsByName []


linkMatch1 :: MatchDef () -> ReaderStack ()
linkMatch1 = iterM processMatchDef
  where
    processMatchDef :: MatchDefF (ReaderStack ()) -> ReaderStack ()
    processMatchDef = \case
      MatchLiteral literal next -> lift (linkedMatchLiteral literal) >> next
      MatchInvocation func next -> linkFuncInvocation func >> next
      MatchCaptureInvocation name nestedDef next -> do
        path <- asks getPath
        let nestedPath = path ++ [name]
        local (\c -> c { getPath = nestedPath }) $ do
          linkedCaptureInvocation <- pullOutFree $ iterM processMatchDef nestedDef
          lift $ linkedMatchNamedCaptureGroup nestedPath linkedCaptureInvocation
        next
    pullOutFree :: ReaderStack () -> ReaderStack (LinkedMatchT (Except String) ())
    pullOutFree x = ReaderT $ \c -> return (runReaderT x c)



linkFuncInvocation :: FuncInvocation -> ReaderStack ()
linkFuncInvocation func = case func of
  UserDefinedFuncInvocation funcName -> do
    letDeclsByName <- asks getLetDeclsByName
    letDecl <- Map.lookup funcName letDeclsByName
           |> hoistMaybeToExceptT ("During linking: could not find a definition for " ++ funcName)
           |> lift
           |> lift
    iterM linkLet letDecl
  BuiltInFuncInvocation0Arg func -> lift $ linkedMatchBuiltInFunc0Arg func
  BuiltInFuncInvocation1Arg func arg0 -> do
    linkedArg0 <- linkArgs arg0
    let concattedLinkedArg0 = concatMonad linkedArg0
    -- TODO: Potential bug: what if the argument is itself a named capture group?
    arg0IsSingleCharLiteralOrBracket <- lift $ lift $ isSingleCharLiteralOrBracket concattedLinkedArg0
    let processedArg0 :: LinkedMatchT (Except String) ()
        processedArg0 = if arg0IsSingleCharLiteralOrBracket
                        then concattedLinkedArg0
                        else linkedMatchUnnamedCaptureGroup concattedLinkedArg0
    case func of
      AnyOf  -> lift $ linkedMatchBuiltInFunc1Arg AnyOf concattedLinkedArg0
      NoneOf -> lift $ linkedMatchBuiltInFunc1Arg NoneOf concattedLinkedArg0
      Star   -> lift $ linkedMatchBuiltInFunc1Arg Star processedArg0
      Plus   -> lift $ linkedMatchBuiltInFunc1Arg Star processedArg0
      Maybe  -> lift $ linkedMatchBuiltInFunc1Arg Maybe processedArg0
  BuiltInFuncInvocation2Arg func arg0 arg1 -> do
    linkedArg0 <- linkArg arg0
    linkedArg1 <- linkArgs arg1
    let concattedLinkedArg1 = concatMonad linkedArg1
    -- TODO: Potential bug: what if the argument is itself a named capture group?
    arg1IsSingleCharLiteralOrBracket <- lift $ lift $ isSingleCharLiteralOrBracket concattedLinkedArg1
    let processedArg1 :: LinkedMatchT (Except String) ()
        processedArg1 = if arg1IsSingleCharLiteralOrBracket
                        then concattedLinkedArg1
                        else linkedMatchUnnamedCaptureGroup concattedLinkedArg1
    case func of
      AtMost -> do
        arg0IsNumericCharLiteral <- lift $ lift $ isNumericCharLiteral linkedArg0
        unless arg0IsNumericCharLiteral do
          lift $ lift $ throwE "atMost requires a numeric literal as its 1st argument!"
        lift $ linkedMatchBuiltInFunc2Arg AtMost linkedArg0 processedArg1
      AtLeast -> do
        arg0IsNumericCharLiteral <- lift $ lift $ isNumericCharLiteral linkedArg0
        unless arg0IsNumericCharLiteral do
          lift $ lift $ throwE "atLeast requires a numeric literal as its 1st argument!"
        lift $ linkedMatchBuiltInFunc2Arg AtLeast linkedArg0 processedArg1
  BuiltInFuncInvocation3Arg func arg0 arg1 arg2 -> do
    linkedArg0 <- linkArg arg0
    linkedArg1 <- linkArg arg1
    linkedArg2 <- linkArgs arg2
    let concattedLinkedArg2 = concatMonad linkedArg2
    -- TODO: Potential bug: what if the argument is itself a named capture group?
    arg2IsSingleCharLiteralOrBracket <- lift $ lift $ isSingleCharLiteralOrBracket concattedLinkedArg2
    let processedArg2 :: LinkedMatchT (Except String) ()
        processedArg2 = if arg2IsSingleCharLiteralOrBracket
                        then concattedLinkedArg2
                        else linkedMatchUnnamedCaptureGroup concattedLinkedArg2
    case func of
      Between -> do
        arg0IsNumericCharLiteral <- lift $ lift $ isNumericCharLiteral linkedArg0
        arg1IsNumericCharLiteral <- lift $ lift $ isNumericCharLiteral linkedArg1
        unless arg0IsNumericCharLiteral do
          lift $ lift $ throwE "between requires a numeric literal as its 1st argument!"
        unless arg1IsNumericCharLiteral do
          lift $ lift $ throwE "between requires a numeric literal as its 2nd argument!"
        lift $ linkedMatchBuiltInFunc3Arg Between linkedArg0 linkedArg1 processedArg2


linkLet :: LetDefF (ReaderStack ()) -> ReaderStack ()
linkLet = \case
  LetDefLiteral literal next -> lift (linkedMatchLiteral literal) >> next
  LetDefInvocation func next -> linkFuncInvocation func >> next
  LetDefCaptureInvocation name nestedDef next -> do
    path <- asks getPath
    let nestedPath = path ++ [name]
        linkedLet :: ReaderStack ()
        linkedLet = local (\c -> c { getPath = nestedPath }) $ iterM linkLet nestedDef
    linkedLetSeq <- pullOutFree linkedLet
    lift $ linkedMatchNamedCaptureGroup nestedPath linkedLetSeq
    next
  where
    pullOutFree :: ReaderStack () -> ReaderStack (LinkedMatchT (Except String) ())
    -- pullOutFree x = ReaderT $ \c -> lift (joinFreeT (runReaderT x c))
    pullOutFree x = ReaderT $ \c -> return $ runReaderT x c


linkArgs :: [FuncArg] -> ReaderStack [LinkedMatchT (Except String) ()]
linkArgs = mapM linkArg

linkArg :: FuncArg -> ReaderStack (LinkedMatchT (Except String) ())
linkArg (ArgLiteral string)  = return $ linkedMatchLiteral string
linkArg (InvocationArg func) = pullOutFree $ linkFuncInvocation func
  where
    pullOutFree x = ReaderT $ \c -> return $ runReaderT x c
