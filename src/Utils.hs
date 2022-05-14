module Utils 
    ( (|>)
    , hoistMaybe
    , hoistMaybeToExceptT
    , concatMonad
    , applyExcept
    , runFreeTExceptT
    , isSingleCharLiteralOrBracket
    ) where


import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Trans.Free
import Control.Monad.Trans.Writer.Lazy
import Control.Monad.Morph
import ProgramAst

-- Reverse function application
infixl 1  |>
(|>) :: a -> (a -> b) -> b
(|>) x f = f x


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
  Left err -> arg
  Right success -> hoistFreeT generalize $ func success


runFreeTExceptT :: Traversable f => FreeT f (Except s) a -> Either s (Free f a)
runFreeTExceptT = runExcept . joinFreeT


-- TODO: fix a potential bug where there are two literals next to each other and one of them is empty
isSingleCharLiteralOrBracket :: LinkedMatch () -> Bool
isSingleCharLiteralOrBracket args = go (getCommands args)
  where
    go commands = case commands of
      [LinkedMatchLiteral str _] -> length str == 1
      [LinkedMatchBuiltInFunc AnyOf _ _] -> True
      [LinkedMatchBuiltInFunc NoneOf _ _] -> True
      _ -> False


getCommands :: LinkedMatch () -> [LinkedMatchF ()]
getCommands linkedMatch = execWriter $ iterM processLine linkedMatch
  where
    processLine :: LinkedMatchF (Writer [LinkedMatchF ()] ()) -> Writer [LinkedMatchF ()] ()
    -- processLine val = tell [val]
    processLine (LinkedMatchLiteral str next) = tell [LinkedMatchLiteral str ()] >> next
    processLine (LinkedMatchUnnamedCaptureGroup linkedMatch next) = tell [LinkedMatchUnnamedCaptureGroup linkedMatch ()] >> next
    processLine (LinkedMatchNamedCaptureGroup name linkedMatch next) = tell [LinkedMatchNamedCaptureGroup name linkedMatch ()] >> next
    processLine (LinkedMatchBuiltInFunc func args next) = tell [LinkedMatchBuiltInFunc func args ()] >> next

