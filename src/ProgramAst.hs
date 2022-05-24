{-# LANGUAGE DeriveTraversable, LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE GADTs #-}

module ProgramAst
    ( FuncInvocation(..)
    , FuncArg(..)
    , BuiltInFunc0Arg(..)
    , BuiltInFunc1Arg(..)
    , BuiltInFunc2Arg(..)
    , BuiltInFunc3Arg(..)
    , LetDefF(..)
    , MatchDefF(..)
    , LinkedMatchF(..)
    , SubDefF(..)
    , LinkedSubF(..)
    , ProgramF(..)
    , Program
    , LetDef
    , MatchDef
    , MatchDefT
    , LinkedMatch
    , LinkedMatchT
    , SubDef
    , SubDefT
    , LinkedSub
    , LinkedSubT
    , match
    , substitute
    , letDecl
    , letDefLiteral
    , letDefInvocation
    , letDefCaptureInvocation
    , matchLiteral
    , matchInvocation
    , matchCaptureInvocation
    , subLiteral
    , subCaptureReference
    , subScopedCaptureReference
    , linkedMatchLiteral
    , linkedMatchUnnamedCaptureGroup
    , linkedMatchNamedCaptureGroup
    , linkedMatchBuiltInFunc0Arg
    , linkedMatchBuiltInFunc1Arg
    , linkedMatchBuiltInFunc2Arg
    , linkedMatchBuiltInFunc3Arg
    , linkedSubLiteral
    , linkedSubBackReference
    ) where

import           Control.Monad.Morph
import           Control.Monad.Trans.Free
import           Data.Functor.Classes
import           Data.Functor ((<&>))
import Data.Functor.Identity

{- Program commands -}


letDecl :: Monad m => String -> LetDef () -> ProgramT m ()
letDecl name def = liftToFreeTStack $ LetDecl name def


match :: Monad m => MatchDef () -> ProgramT m ()
match match = liftToFreeTStack $ Match match


substitute :: Monad m => SubDef () -> ProgramT m ()
substitute substitute = liftToFreeTStack $ Substitute substitute


{- Let commands -}


letDefLiteral :: Monad m => String -> LetDefT m ()
letDefLiteral literal = liftToFreeTStack $ LetDefLiteral literal


letDefInvocation :: Monad m => FuncInvocation -> LetDefT m ()
letDefInvocation invocation = liftToFreeTStack $ LetDefInvocation invocation


letDefCaptureInvocation :: Monad m => String -> LetDef () -> LetDefT m ()
letDefCaptureInvocation captureName subExpression = liftToFreeTStack $ LetDefCaptureInvocation captureName subExpression


{- Match commands -}


matchLiteral :: Monad m => String -> MatchDefT m ()
matchLiteral literal = liftToFreeTStack $ MatchLiteral literal


matchInvocation :: Monad m => FuncInvocation -> MatchDefT m ()
matchInvocation invocation = liftToFreeTStack $ MatchInvocation invocation


matchCaptureInvocation :: Monad m => String -> MatchDef () -> MatchDefT m ()
matchCaptureInvocation captureName subExpression = liftToFreeTStack $ MatchCaptureInvocation captureName subExpression


{- Compilation commands -}


linkedMatchLiteral :: Monad m => String -> LinkedMatchT m ()
linkedMatchLiteral literal = liftToFreeTStack $ LinkedMatchLiteral literal


linkedMatchUnnamedCaptureGroup :: Monad m => LinkedMatchT m () -> LinkedMatchT m ()
linkedMatchUnnamedCaptureGroup match = liftToFreeTStack1 $ LinkedMatchUnnamedCaptureGroup match


linkedMatchNamedCaptureGroup :: Monad m => [String] -> LinkedMatchT m () -> LinkedMatchT m ()
linkedMatchNamedCaptureGroup path match = liftToFreeTStack1 $ LinkedMatchNamedCaptureGroup path match


linkedMatchBuiltInFunc0Arg :: Monad m => BuiltInFunc0Arg -> LinkedMatchT m ()
linkedMatchBuiltInFunc0Arg func = liftToFreeTStack1 $ LinkedMatchBuiltInFunc0Arg func


linkedMatchBuiltInFunc1Arg :: Monad m => BuiltInFunc1Arg -> LinkedMatchT m () -> LinkedMatchT m ()
linkedMatchBuiltInFunc1Arg func arg0 = liftToFreeTStack1 $ LinkedMatchBuiltInFunc1Arg func arg0


linkedMatchBuiltInFunc2Arg :: Monad m => BuiltInFunc2Arg -> LinkedMatchT m () -> LinkedMatchT m () -> LinkedMatchT m ()
linkedMatchBuiltInFunc2Arg func arg0 arg1 = liftToFreeTStack1 $ LinkedMatchBuiltInFunc2Arg func arg0 arg1


linkedMatchBuiltInFunc3Arg :: Monad m => BuiltInFunc3Arg -> LinkedMatchT m () -> LinkedMatchT m () -> LinkedMatchT m () -> LinkedMatchT m ()
linkedMatchBuiltInFunc3Arg func arg0 arg1 arg2 = liftToFreeTStack1 $ LinkedMatchBuiltInFunc3Arg func arg0 arg1 arg2


linkedSubLiteral :: Monad m => String -> LinkedSubT m ()
linkedSubLiteral literal = liftToFreeTStack $ LinkedSubLiteral literal


linkedSubBackReference :: Monad m => Int -> LinkedSubT m ()
linkedSubBackReference captureGroup = liftToFreeTStack $ LinkedSubBackReference captureGroup


{- Sub commands -}


subLiteral :: Monad m => String -> SubDefT m ()
subLiteral literal = liftToFreeTStack $ SubLiteral literal


subCaptureReference :: Monad m => String -> SubDefT m ()
subCaptureReference refName = liftToFreeTStack $ SubCaptureReference refName


subScopedCaptureReference :: Monad m => [String] -> SubDefT m ()
subScopedCaptureReference refPath = liftToFreeTStack $ SubScopedCaptureReference refPath


{- Program types -}


liftToFreeTStack :: (Monad m, Functor f) => (forall a. a -> f a) -> FreeT f m ()
liftToFreeTStack constructorFunc = hoistFreeT generalize $ liftF $ constructorFunc ()


liftToFreeTStack1 x = FreeT . return . Free $ x (pure ())


-- TODO: Should user-defined functions accept inputs?
data FuncInvocation = UserDefinedFuncInvocation { funcName :: String }
                    | BuiltInFuncInvocation0Arg BuiltInFunc0Arg
                    | BuiltInFuncInvocation1Arg BuiltInFunc1Arg [FuncArg]
                    | BuiltInFuncInvocation2Arg BuiltInFunc2Arg FuncArg [FuncArg]
                    | BuiltInFuncInvocation3Arg BuiltInFunc3Arg FuncArg FuncArg [FuncArg]
                    deriving (Eq)


data FuncArg = ArgLiteral String | InvocationArg FuncInvocation deriving (Eq)


data BuiltInFunc0Arg = Any
                     | StartLine
                     | EndLine
                     deriving (Show, Eq)
data BuiltInFunc1Arg = Star
                     | Plus
                     | NoneOf
                     | AnyOf
                     | Maybe
                     deriving (Show, Eq)
data BuiltInFunc2Arg = AtMost
                     | AtLeast
                     deriving (Show, Eq)
data BuiltInFunc3Arg = Between 
                     deriving (Show, Eq)


data ProgramF next = LetDecl { letName :: String,  letDef :: LetDef (), next :: next }
                   | Match (MatchDef ()) next
                   | Substitute (SubDef ()) next
                   deriving (Functor, Foldable)
type Program = Free ProgramF
type ProgramT m a = FreeT ProgramF m a


data LetDefF next = LetDefLiteral String next
                  | LetDefInvocation FuncInvocation next
                  | LetDefCaptureInvocation String (LetDef ()) next
                  deriving (Functor, Foldable)
type LetDef a = Free LetDefF a
type LetDefT m a = FreeT LetDefF m a


data MatchDefF next = MatchLiteral String next
                    | MatchInvocation FuncInvocation next
                    | MatchCaptureInvocation String (MatchDef ()) next
                    deriving (Functor, Foldable, Traversable)
type MatchDef a = Free MatchDefF a
type MatchDefT m a = FreeT MatchDefF m a

instance Eq1 MatchDefF where
  liftEq test (MatchLiteral string1 next1) (MatchLiteral string2 next2) =
    string1 == string2 && test next1 next2
  liftEq test (MatchInvocation func1 next1) (MatchInvocation func2 next2) =
    func1 == func2 && test next1 next2
  liftEq test (MatchCaptureInvocation name1 match1 next1) (MatchCaptureInvocation name2 match2 next2) =
    name1 == name2 && eq1 match1 match2 && test next1 next2
  liftEq _ _ _ = False


data LinkedMatchF m next = LinkedMatchLiteral String next
                         | LinkedMatchUnnamedCaptureGroup (LinkedMatchT m ()) next
                         | LinkedMatchNamedCaptureGroup [String] (LinkedMatchT m ()) next
                         | LinkedMatchBuiltInFunc0Arg BuiltInFunc0Arg next
                         | LinkedMatchBuiltInFunc1Arg BuiltInFunc1Arg (LinkedMatchT m ()) next
                         | LinkedMatchBuiltInFunc2Arg BuiltInFunc2Arg (LinkedMatchT m ()) (LinkedMatchT m ()) next
                         | LinkedMatchBuiltInFunc3Arg BuiltInFunc3Arg (LinkedMatchT m ()) (LinkedMatchT m ()) (LinkedMatchT m ()) next


instance Monad m => Functor (LinkedMatchF m) where
  fmap f = \case
    LinkedMatchLiteral x next                     -> LinkedMatchLiteral x (f next)
    LinkedMatchUnnamedCaptureGroup x next         -> LinkedMatchUnnamedCaptureGroup x (f next)
    LinkedMatchNamedCaptureGroup x y next         -> LinkedMatchNamedCaptureGroup x y (f next)
    LinkedMatchBuiltInFunc0Arg func next          -> LinkedMatchBuiltInFunc0Arg func (f next)
    LinkedMatchBuiltInFunc1Arg func a1 next       -> LinkedMatchBuiltInFunc1Arg func a1 (f next)
    LinkedMatchBuiltInFunc2Arg func a1 a2 next    -> LinkedMatchBuiltInFunc2Arg func a1 a2 (f next)
    LinkedMatchBuiltInFunc3Arg func a1 a2 a3 next -> LinkedMatchBuiltInFunc3Arg func a1 a2 a3  (f next)
instance Monad m => Foldable (LinkedMatchF m) where
  foldMap f = \case
    LinkedMatchLiteral x next                     -> f next
    LinkedMatchUnnamedCaptureGroup x next         -> f next
    LinkedMatchNamedCaptureGroup x y next         -> f next
    LinkedMatchBuiltInFunc0Arg func next          -> f next
    LinkedMatchBuiltInFunc1Arg func a1 next       -> f next
    LinkedMatchBuiltInFunc2Arg func a1 a2 next    -> f next
    LinkedMatchBuiltInFunc3Arg func a1 a2 a3 next -> f next
  foldr f acc = \case
    LinkedMatchLiteral x next -> f next acc
    LinkedMatchUnnamedCaptureGroup x next -> f next acc
    LinkedMatchNamedCaptureGroup x y next -> f next acc
    LinkedMatchBuiltInFunc0Arg func next          -> f next acc
    LinkedMatchBuiltInFunc1Arg func a1 next       -> f next acc
    LinkedMatchBuiltInFunc2Arg func a1 a2 next    -> f next acc
    LinkedMatchBuiltInFunc3Arg func a1 a2 a3 next -> f next acc
instance Monad m => Traversable (LinkedMatchF m) where
  sequenceA = \case
    LinkedMatchLiteral x next                     -> next <&> LinkedMatchLiteral x
    LinkedMatchUnnamedCaptureGroup x next         -> next <&> LinkedMatchUnnamedCaptureGroup x
    LinkedMatchNamedCaptureGroup x y next         -> next <&> LinkedMatchNamedCaptureGroup x y
    LinkedMatchBuiltInFunc0Arg func next          -> next <&> LinkedMatchBuiltInFunc0Arg func
    LinkedMatchBuiltInFunc1Arg func a1 next       -> next <&> LinkedMatchBuiltInFunc1Arg func a1
    LinkedMatchBuiltInFunc2Arg func a1 a2 next    -> next <&> LinkedMatchBuiltInFunc2Arg func a1 a2
    LinkedMatchBuiltInFunc3Arg func a1 a2 a3 next -> next <&> LinkedMatchBuiltInFunc3Arg func a1 a2 a3
  traverse f x = sequenceA (f <$> x)


type LinkedMatch a = Free (LinkedMatchF Identity) a
type LinkedMatchT m = FreeT (LinkedMatchF m) m


data SubDefF next = SubLiteral String next
                  | SubCaptureReference String next
                  | SubScopedCaptureReference [String] next
                  deriving (Eq, Functor)
type SubDef a = Free SubDefF a
type SubDefT m a = FreeT SubDefF m a


instance Eq1 SubDefF where
  liftEq test (SubLiteral string1 next1) (SubLiteral string2 next2) =
    string1 == string2 && test next1 next2
  liftEq test (SubCaptureReference name1 next1) (SubCaptureReference name2 next2) =
    name1 == name2 && test next1 next2
  liftEq test (SubScopedCaptureReference refPath1 next1) (SubScopedCaptureReference refPath2 next2) =
    refPath1 == refPath2 && test next1 next2
  liftEq _ _ _ = False


data LinkedSubF next = LinkedSubLiteral String next
                     | LinkedSubBackReference Int next
                     deriving (Functor, Foldable, Traversable)
type LinkedSub a = Free LinkedSubF a
type LinkedSubT m a = FreeT LinkedSubF m a

data AttemptF next = AttemptConstructor String next next deriving (Functor, Foldable, Traversable)
type Attempt a = Free AttemptF a
type AttemptT m a = FreeT AttemptF m a


attempt :: String -> Attempt () -> Attempt ()
attempt str nested1 = free $ Free $ AttemptConstructor str nested1 (pure ())

attempt1 :: Monad m => String -> AttemptT m () -> AttemptT m ()
attempt1 str nested1 = FreeT . return . Free $ AttemptConstructor str nested1 (pure ())
