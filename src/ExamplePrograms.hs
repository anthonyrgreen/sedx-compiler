module ExamplePrograms
    ( myProgram
    , single_import
    , many_imports
    , filename
    , whole_path
    , myProgramMatch
    , myProgramMatchOptimized
    , myProgramCompiledState
    ) where


import           BuiltInFunctions
import           Control.Monad.State
import           Control.Monad.Trans.Free
import           Data.Map.Strict          as Map
import           Data.Maybe
import           EscapeMatch
import           Flags
import           Lib
import           ProgramAst
import           ReadProgramAst

single_import :: LetDef ()
single_import = do
  let importChars = anyOfLiteral "A-Za-z0-9"
  letDefInvocation $ BuiltInFuncInvocation1Arg Star [InvocationArg importChars]
  letDefInvocation $ maybeLiteral ", "


many_imports :: LetDef ()
many_imports = do
    let single_import_invocation = UserDefinedFuncInvocation "single_import"
    letDefInvocation $ BuiltInFuncInvocation1Arg Star [InvocationArg single_import_invocation]


filename :: LetDef ()
filename = do
    letDefInvocation $ BuiltInFuncInvocation1Arg Star [InvocationArg $ noneOfLiteral "/ "]


whole_path :: LetDef ()
whole_path = do
    let filename_invocation = UserDefinedFuncInvocation "filename"
    letDefCaptureInvocation "root" (letDefInvocation filename_invocation)
    letDefLiteral "/"
    letDefInvocation $ BuiltInFuncInvocation1Arg Star [InvocationArg filename_invocation, ArgLiteral "/"]
    letDefCaptureInvocation "filename" (letDefInvocation filename_invocation)


myProgram :: Program ()
myProgram = do
  letDecl "single_import" $ do
    let importChars = anyOfLiteral "A-Za-z0-9"
    letDefInvocation $ BuiltInFuncInvocation1Arg Star [InvocationArg importChars]
    letDefInvocation $ maybeLiteral ", "
  letDecl "many_imports" $ do
    let single_import_invocation = UserDefinedFuncInvocation "single_import"
    letDefInvocation $ BuiltInFuncInvocation1Arg Star [InvocationArg single_import_invocation]
  letDecl "filename" $ do
    letDefInvocation $ BuiltInFuncInvocation1Arg Star [InvocationArg $ noneOfLiteral "/ "]
  letDecl "whole_path" $ do
    let filename_invocation = UserDefinedFuncInvocation "filename"
    letDefCaptureInvocation "root" (letDefInvocation filename_invocation)
    letDefLiteral "/"
    letDefInvocation $ BuiltInFuncInvocation1Arg Star [InvocationArg filename_invocation, ArgLiteral "/"]
    letDefCaptureInvocation "filename" (letDefInvocation filename_invocation)
  match $ do
    matchLiteral "import {"
    matchCaptureInvocation "imports" $ matchInvocation (UserDefinedFuncInvocation "many_imports")
    matchLiteral "} from '"
    matchCaptureInvocation "whole_path" $ matchInvocation (UserDefinedFuncInvocation "whole_path")
    matchLiteral "'"
  substitute $ do
    subLiteral "import {"
    subCaptureReference "imports"
    subLiteral "} from '"
    subScopedCaptureReference ["whole_path", "root"]
    subLiteral "/move/to/some/new/directory/"
    subScopedCaptureReference ["whole_path", "filename"]


myProgramCompiledState = readProgramAst myProgram
myProgramMatch = escapeProgramMatch GNU myProgram
myProgramMatchOptimized = escapeOptimizedProgramMatch GNU myProgram

