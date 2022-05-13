{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, TypeSynonymInstances, FlexibleInstances, RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module ProgramAst
    ( FuncInvocation(..)
    , FuncArg(..)
    , BuiltInFunc(..)
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
    , linkedMatchBuiltInFunc
    , linkedSubLiteral
    , linkedSubBackReference
    ) where

import Text.Show.Deriving
import Data.Functor.Classes
import Control.Monad.Trans.Free
import Control.Monad.Morph

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


linkedMatchUnnamedCaptureGroup :: Monad m => LinkedMatch () -> LinkedMatchT m ()
linkedMatchUnnamedCaptureGroup match = liftToFreeTStack $ LinkedMatchUnnamedCaptureGroup match


linkedMatchNamedCaptureGroup :: Monad m => String -> LinkedMatch () -> LinkedMatchT m ()
linkedMatchNamedCaptureGroup name match = liftToFreeTStack $ LinkedMatchNamedCaptureGroup name match


linkedMatchBuiltInFunc :: Monad m => BuiltInFunc -> LinkedMatch () -> LinkedMatchT m ()
linkedMatchBuiltInFunc builtInFunc args = liftToFreeTStack $ LinkedMatchBuiltInFunc builtInFunc args


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


-- TODO: Should user-defined functions accept inputs?
data FuncInvocation = UserDefinedFuncInvocation { funcName :: String }
                    | BuiltInFuncInvocation { func :: BuiltInFunc, args :: [FuncArg] }
                    deriving (Eq)


data FuncArg = ArgLiteral String | InvocationArg FuncInvocation deriving (Eq)


data BuiltInFunc = Star
                 | NoneOf
                 | AnyOf
                 | Maybe
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
    name1 == name2 && (eq1 match1 match2) && test next1 next2
  liftEq _ _ _ = False


data LinkedMatchF next = LinkedMatchLiteral String next
                       | LinkedMatchUnnamedCaptureGroup (LinkedMatch ()) next
                       | LinkedMatchNamedCaptureGroup String (LinkedMatch ()) next
                       | LinkedMatchBuiltInFunc BuiltInFunc (LinkedMatch ()) next
                       deriving (Functor, Foldable, Traversable)
type LinkedMatch a = Free LinkedMatchF a
type LinkedMatchT m a = FreeT LinkedMatchF m a


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



