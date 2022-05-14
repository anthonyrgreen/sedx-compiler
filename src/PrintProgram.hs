{-# LANGUAGE TemplateHaskell #-}

module PrintProgram
  ( showProgram
  , showLetDef
  , showMatchDef
  , showSubDef
  , showLinkedMatchDef
  ) where


import Text.Show.Deriving
import ProgramAst
import Control.Monad.Trans.Free
import Data.List
import Utils

instance Show FuncInvocation where
  show (UserDefinedFuncInvocation name) = "UserFuncInvoc: name = " ++ name
  show (BuiltInFuncInvocation func args) = "BuiltinFuncInvoc: name = " ++ show func ++ ", args = " ++ show args


instance Show FuncArg where
  show (ArgLiteral string) = "Lit: \"" ++ string ++ "\""
  show (InvocationArg invoc) = "Invoc: " ++ show invoc


showProgram :: Program a -> String
showProgram program = "Program\n" ++ go program
  where
    go program = case runFree program of
      Pure _ -> ""
      Free (LetDecl name def next) ->
        "Name: " ++ name ++ " Def: " ++ showLetDef def ++ go next
      Free (Match match next) -> showMatchDef match ++ go next
      Free (Substitute sub next) -> showSubDef sub ++ go next


indent tabs str = replicate tabs '\t' ++ str

showLetDef :: LetDef a -> String
showLetDef = showLetDef1 0

showLetDef1 :: Int -> LetDef a -> String
showLetDef1 tabs letDef = replicate tabs '\t' ++ "Let\n" ++ go (tabs + 1) letDef
  where
    go tabs letDef = case runFree letDef of
      Pure _ -> ""
      Free (LetDefLiteral x next) ->
        indent tabs "Lit: \"" ++ x ++ "\"\n" ++ go tabs next
      Free (LetDefInvocation func next) ->
        indent tabs "Invoc: " ++ show func ++ "\n" ++ go tabs next
      Free (LetDefCaptureInvocation name func next) ->
        indent tabs "Capture name: " ++ name ++ ", Subexpression:\n" ++ showLetDef1 (tabs + 1) func ++ "\n" ++ go tabs next


showMatchDef :: MatchDef a -> String
showMatchDef = showMatchDef1 0


showMatchDef1 :: Int -> MatchDef a -> String
showMatchDef1 tabs matchDef = indent tabs "Match\n" ++ go (tabs + 1) matchDef
  where
    go tabs matchDef = case runFree matchDef of
      Pure _ -> ""
      Free (MatchLiteral x next) ->
        indent tabs "Lit: \"" ++ x ++ "\"\n" ++ go tabs next
      Free (MatchInvocation func next) ->
        indent tabs "Invoc: " ++ show func ++ "\n" ++ go tabs next
      Free (MatchCaptureInvocation name func next) ->
        indent tabs "Capture name: " ++ name ++ ", Subexpression: " ++ showMatchDef1 (tabs + 1) func ++ "\n" ++ go tabs next


showSubDef :: SubDef a -> String
showSubDef = showSubDef1 0


showSubDef1 :: Int -> SubDef a -> String
showSubDef1 tabs subDef = indent tabs "Sub\n" ++ go (tabs + 1) subDef
  where
    go tabs subDef = case runFree subDef of
      Pure _ -> ""
      Free (SubLiteral x next) ->
        indent tabs "Lit: \"" ++ x ++ "\"\n" ++ go tabs next
      Free (SubCaptureReference ref next) ->
        indent tabs "CaptureRef: " ++ ref ++ "\n" ++ go tabs next
      Free (SubScopedCaptureReference path next) ->
        indent tabs "Scoped CaptureRef: " ++ intercalate "." path ++ "\n" ++ go tabs next


showLinkedMatchDef :: LinkedMatch a -> String
showLinkedMatchDef linkedMatchDef = "LinkedMatch\n" ++ go 1 linkedMatchDef
  where
    go :: Int -> LinkedMatch a -> String
    go tabs linkedMatchDef = case runFree linkedMatchDef of
      Pure _ -> ""
      Free (LinkedMatchLiteral literal next) ->
        indent tabs "Lit: \"" ++ literal ++ "\"\n"
        ++ go tabs next
      Free (LinkedMatchUnnamedCaptureGroup nestedDef next) ->
        indent tabs "Unnamed capture group:\n"
        ++ indent tabs "{\n"
        ++ go (tabs + 1) nestedDef
        ++ indent tabs "}\n"
        ++ go tabs next
      Free (LinkedMatchNamedCaptureGroup name nestedDef next) ->
        indent tabs "Named capture group: <" ++ name ++ ">\n"
        ++ indent tabs "{\n"
        ++ go (tabs + 1) nestedDef
        ++ indent tabs "}\n"
        ++ go tabs next
      Free (LinkedMatchBuiltInFunc builtInFunc nestedDefs next) ->
        indent tabs "Built-in func:\n"
        ++ indent tabs "name: <" ++ show builtInFunc ++ ">\n"
        ++ indent tabs "{\n"
        ++ go (tabs + 1) nestedDefs
        ++ indent tabs "}\n"
        ++ go tabs next

deriveShow1 ''LetDefF
deriveShow1 ''SubDefF
deriveShow1 ''MatchDefF
deriveShow1 ''ProgramF
deriveShow1 ''LinkedMatchF
deriveShow1 ''LinkedSubF

