module Utils
    ( (|>)
    , hoistMaybe
    , hoistMaybeToExceptT
    , concatMonad
    , applyExcept
    , runFreeTExceptT
    , isSingleCharLiteralOrBracket
    , isNumericCharLiteral
    ) where


import           Control.Monad.Morph
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Writer.Lazy
import           ProgramAst
import Data.Functor.Identity
import Data.Maybe
import Data.Function
import Text.Read

-- Reverse function application
infixl 1  |>
(|>) :: a -> (a -> b) -> b
(|>) x f = f x


-- fromEither :: (e -> a) -> Either e a -> a
-- fromEither f = either f id

-- Available in transformers-0.6.0.0 but we're currently at 0.5.6.2
-- | Convert a 'Maybe' computation to 'MaybeT'.
hoistMaybe :: (Applicative m) => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . pure


hoistMaybeToExceptT :: (Applicative m) => e -> Maybe a -> ExceptT e m a
hoistMaybeToExceptT err = maybeToExceptT err . hoistMaybe


concatMonad :: Monad m => [m ()] -> m ()
concatMonad = Prelude.foldl (>>) (return ())

applyExcept :: Traversable f => FreeT f (Except e) a -> (Free f a -> Free f a) -> FreeT f (Except e) a
applyExcept arg func = case runExcept (joinFreeT arg) of
  Left _        -> arg
  Right success -> hoistFreeT generalize $ func success


runFreeTExceptT :: Traversable f => FreeT f (Except s) a -> Either s (Free f a)
runFreeTExceptT = runExcept . joinFreeT


-- TODO: fix a potential bug where there are two literals next to each other and one of them is empty
isSingleCharLiteralOrBracket :: Monad m => LinkedMatchT m () -> m Bool
isSingleCharLiteralOrBracket args = go <$> getCommands args
  where
    go commands = case commands of
      [LinkedMatchLiteral str _]              -> length str == 1
      [LinkedMatchBuiltInFunc1Arg AnyOf _ _]  -> True
      [LinkedMatchBuiltInFunc1Arg NoneOf _ _] -> True
      _                                       -> False

isNumericCharLiteral :: Monad m => LinkedMatchT m () -> m Bool
isNumericCharLiteral args = go <$> getCommands args
  where
    go commands = case commands of
      [LinkedMatchLiteral str _] -> ((readMaybe str) :: Maybe Int) & isJust
      _ -> False

getCommands :: Monad m => LinkedMatchT m () -> m [LinkedMatchF m ()]
getCommands = execWriterT . iterTM processLine
  where
    processLine :: Monad m => LinkedMatchF m (WriterT [LinkedMatchF m ()] m  ()) -> WriterT [LinkedMatchF m ()] m ()
    processLine (LinkedMatchLiteral str next)                          = tell [LinkedMatchLiteral str ()]                         >> next
    processLine (LinkedMatchUnnamedCaptureGroup linkedMatch next)      = tell [LinkedMatchUnnamedCaptureGroup linkedMatch ()]     >> next
    processLine (LinkedMatchNamedCaptureGroup name linkedMatch next)   = tell [LinkedMatchNamedCaptureGroup name linkedMatch ()]  >> next
    processLine (LinkedMatchBuiltInFunc0Arg func next)                 = tell [LinkedMatchBuiltInFunc0Arg func ()]                >> next
    processLine (LinkedMatchBuiltInFunc1Arg func arg0 next)            = tell [LinkedMatchBuiltInFunc1Arg func arg0 ()]           >> next
    processLine (LinkedMatchBuiltInFunc2Arg func arg0 arg1 next)       = tell [LinkedMatchBuiltInFunc2Arg func arg0 arg1 ()]      >> next
    processLine (LinkedMatchBuiltInFunc3Arg func arg0 arg1 arg2 next)  = tell [LinkedMatchBuiltInFunc3Arg func arg0 arg1 arg2 ()] >> next

