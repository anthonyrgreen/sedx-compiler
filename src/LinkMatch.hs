{-# LANGUAGE LambdaCase #-}

module LinkMatch
    ( linkMatch
    ) where


import           Control.Monad.Except
import           Control.Monad.Morph
import           Control.Monad.State
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Control.Monad.Writer.Lazy
import           Data.Functor.Identity
import           Data.Map.Strict            as Map
import           Data.Maybe
import           ProgramAst
import           ReadProgramAst
import           Utils


type ReaderStack = ReaderT Config (LinkedMatchT (Except String))
data Config = Config { getLetDeclsByName :: Map.Map String (LetDef ()), getPath :: [String] }


linkMatch :: Map.Map String (LetDef ()) -> MatchDef () -> LinkedMatchT (Except String) ()
linkMatch letDeclsByName matchDef = runReaderT (linkMatch1 matchDef) config
  where
    config = Config letDeclsByName []


linkMatch1 :: MatchDef () -> ReaderT Config (LinkedMatchT (Except String)) ()
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
          -- TODO: IS THIS RIGHT? Aren't we adding an extra capture invocation?

        -- let linkedCaptureInvocation = local (\c -> c { getPath = path ++ [name] }) $ iterM processMatchDef nestedDef
        -- linkedCaptureInvocationSeq <- pullOutFree linkedCaptureInvocation
        -- local (\c -> c { getPath = path ++ [name] }) $ lift $ linkedMatchNamedCaptureGroup (path ++ [name]) linkedCaptureInvocationSeq
        next
    pullOutFree :: ReaderStack () -> ReaderStack (LinkedMatch ())
    pullOutFree x = ReaderT $ \c -> lift (joinFreeT (runReaderT x c))


linkFuncInvocation :: FuncInvocation -> ReaderStack ()
linkFuncInvocation func = case func of
  UserDefinedFuncInvocation funcName -> do
    letDeclsByName <- asks getLetDeclsByName
    letDecl <- Map.lookup funcName letDeclsByName
           |> hoistMaybeToExceptT ("During linking: could not find a definition for " ++ funcName)
           |> lift
           |> lift
    iterM linkLet letDecl
  BuiltInFuncInvocation func args -> do
    linkedArgs <- linkArgs args
    let concattedLinkedArgs = concatMonad linkedArgs
    -- TODO: Potential bug: what if the argument is itself a named capture group?
    let processedArgs = if isSingleCharLiteralOrBracket concattedLinkedArgs
                        then concattedLinkedArgs
                        else linkedMatchUnnamedCaptureGroup concattedLinkedArgs
    case func of
      AnyOf  -> lift $ linkedMatchBuiltInFunc AnyOf concattedLinkedArgs
      NoneOf -> lift $ linkedMatchBuiltInFunc NoneOf concattedLinkedArgs
      Star   -> lift $ linkedMatchBuiltInFunc Star processedArgs
      Maybe  -> lift $ linkedMatchBuiltInFunc Maybe processedArgs


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
    pullOutFree :: ReaderStack () -> ReaderStack (LinkedMatch ())
    pullOutFree x = ReaderT $ \c -> lift (joinFreeT (runReaderT x c))


linkArgs :: [FuncArg] -> ReaderStack [LinkedMatch ()]
linkArgs = mapM linkArg
  where
    linkArg :: FuncArg -> ReaderStack (LinkedMatch ())
    linkArg (ArgLiteral string)  = return $ linkedMatchLiteral string
    linkArg (InvocationArg func) = pullOutFree $ linkFuncInvocation func
    pullOutFree :: (Monad m, Traversable f) => ReaderT Config (FreeT f m) a -> ReaderT Config (FreeT f m) (Free f a)
    pullOutFree x = ReaderT $ \c -> lift (joinFreeT (runReaderT x c))
