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


showLetDef :: LetDef a -> String
showLetDef letDef = "Let\n" ++ go letDef
  where
    go letDef = case runFree letDef of
      Pure _ -> ""
      Free (LetDefLiteral x next) -> 
        "\tLit: \"" ++ x ++ "\"\n" ++ go next
      Free (LetDefInvocation func next) -> 
        "\tInvoc: " ++ show func ++ "\n" ++ go next
      Free (LetDefCaptureInvocation name func next) -> 
        "\tCapt name: " ++ name ++ ", Subexpression: " ++ showLetDef func ++ "\n" ++ go next


showMatchDef :: MatchDef a -> String
showMatchDef matchDef = "Match\n" ++ go matchDef
  where
    go matchDef = case runFree matchDef of
      Pure _ -> ""
      Free (MatchLiteral x next) -> 
        "\tLit: \"" ++ x ++ "\"\n" ++ go next
      Free (MatchInvocation func next) -> 
        "\tInvoc: " ++ show func ++ "\n" ++ go next
      Free (MatchCaptureInvocation name func next) -> 
        "\tCapt name: " ++ name ++ ", Subexpression: " ++ showMatchDef func ++ "\n" ++ go next


showSubDef :: SubDef a -> String
showSubDef subDef = "Sub\n" ++ go subDef
  where
    go subDef = case runFree subDef of
      Pure _ -> ""
      Free (SubLiteral x next) -> 
        "\tLit: \"" ++ x ++ "\"\n" ++ go next
      Free (SubCaptureReference ref next) -> 
        "\tCaptureRef: " ++ ref ++ "\n" ++ go next
      Free (SubScopedCaptureReference path next) -> 
        "\tScoped CaptureRef: " ++ (concat $ intersperse "." path) ++ "\n" ++ go next


showLinkedMatchDef :: LinkedMatch a -> String
showLinkedMatchDef linkedMatchDef = "LinkedMatch\n" ++ go 1 linkedMatchDef
  where
    go :: Int -> LinkedMatch a -> String
    go tabs linkedMatchDef = case runFree linkedMatchDef of
      Pure _ -> ""
      Free (LinkedMatchLiteral literal next) -> 
        take tabs (repeat '\t') ++ "Lit: \"" ++ literal ++ "\"\n" 
        ++ go tabs next
      Free (LinkedMatchUnnamedCaptureGroup nestedDef next) -> 
        take tabs (repeat '\t') ++ "Unnamed capture group:\n"
        ++ take tabs (repeat '\t') ++ "{\n" 
        ++ go (tabs + 1) nestedDef 
        ++ take tabs (repeat '\t') ++ "}\n" 
        ++ go tabs next
      Free (LinkedMatchNamedCaptureGroup name nestedDef next) -> 
        take tabs (repeat '\t') ++ "Named capture group: <" ++ name ++ ">\n"
        ++ take tabs (repeat '\t') ++ "{\n" 
        ++ take tabs (repeat '\t') ++ go (tabs + 1) nestedDef 
        ++ take tabs (repeat '\t') ++ "}\n" 
        ++ go tabs next
      Free (LinkedMatchBuiltInFunc builtInFunc nestedDefs next) -> 
        take tabs (repeat '\t') ++ "Built-in func:\n" 
        ++ take tabs (repeat '\t') ++ "name: <" ++ show builtInFunc ++ ">\n"
        ++ take tabs (repeat '\t') ++ "{\n" 
        ++ go (tabs + 1) nestedDefs 
        ++ take tabs (repeat '\t') ++ "}\n" 
        ++ go tabs next

deriveShow1 ''LetDefF
deriveShow1 ''SubDefF
deriveShow1 ''MatchDefF
deriveShow1 ''ProgramF
deriveShow1 ''LinkedMatchF
deriveShow1 ''LinkedSubF

