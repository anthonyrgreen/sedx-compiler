module ParseProgram where
-- module ParseProgram 
--     ( parseProgram
--     , pLetDef
--     , pLetName
--     , Parser
--     , single_import_str
--     , many_imports_str
--     , testpLetDef
--     ) where

import Data.Char
import ProgramAst
import PrintProgram
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Functor
import Control.Monad
import Text.Megaparsec.Debug
import Text.Megaparsec.Pos


type Parser = Parsec Void String
type LetDefP = Parser (LetDef ())
single_import_str = "  single_import := '${star(anyof('A-Za-z0-9')), maybe(', ')}'"
many_imports_str = "  many_imports := '${star(single_import())}'"
filename_str = "  filename := '${star(noneof('/ '))}'"
whole_path_str = "  whole_path := '${capture(filename()) as root, '/', star(filename(), '/'), capture(filename()) as filename}'"

file_str = "let\n\
\  single_import := '${star(anyof('A-Za-z0-9')), maybe(', ')}'\n\
\  many_imports := '${star(single_import())}'\n\
\  filename := '${star(noneof('/ '))}'\n\
\  whole_path := '${capture(filename()) as root, '/', star(filename(), '/'), capture(filename()) as filename}'\n\
\match\n"
test_pLetDef :: String -> IO ()
test_pLetDef s = parseTest (pLetDecl $ mkPos 2) s 

pProgram :: Parser (Program ())
pProgram = do
  L.symbol space "let"
  letIndent <- L.indentGuard space GT pos1
  letDecls <- many (pLetDecl letIndent) <&> sequence_
  L.symbol space "match"
  return letDecls

pLetDecl :: Pos -> Parser (Program ())
pLetDecl letIndent = do
  -- TODO: proper indentation check
  L.indentGuard space EQ letIndent
  name <- L.lexeme hspace1 pLetName
  L.symbol hspace1 ":="
  def <- L.lexeme space pLetDef
  return $ letDecl name def

pLetName :: Parser String
pLetName = do
  first <- satisfy (\s -> isAlpha s || s == '_')
  rest <- many $ satisfy (\s -> isAlphaNum s || s == '_')
  return $ first : rest

pLetDef :: LetDefP
pLetDef = try pLetString <|> pFunctionCapture <|> (letDefInvocation <$> pFunctionCall)

pLetString :: LetDefP
pLetString = between openQuote closeQuote $ do
  letStringTerms <- many pLetStringInteriorTerm
  return $ if null letStringTerms then letDefLiteral "" else sequence_ letStringTerms
  where
    openQuote = char '\''
    closeQuote = L.lexeme hspace $ char '\''
    pLetStringInteriorTerm = try pLetExpansion <|> try pLetStringLiteral -- nil?


pLetExpansion :: LetDefP
pLetExpansion = do
  letExpansionTerms <- between openBrace closeBrace $ sepEndBy1 pLetExpansionTerm pLetSeparator
  return $ sequence_ letExpansionTerms
  where
    openBrace = L.lexeme hspace $ string "${"
    closeBrace = L.lexeme hspace $ string "}"
    pLetSeparator = L.symbol hspace ","
    pLetExpansionTerm = L.lexeme hspace $ pLetString <|> try pFunctionCapture <|> (letDefInvocation <$> pFunctionCall)


pLetStringLiteral :: LetDefP
pLetStringLiteral = do
  -- TODO: we're using 'some' here: what about empty strings??? (If we don't use 'some' we get infinite recursion)
  literal <- some (try escapedCash <|> try escapedQuote <|> try escapedBackslash <|> regularChar)
  return $ letDefLiteral literal
  where
    escapedCash = string "\\$" $> '$'
    escapedQuote = string "\\'" $> '\''
    escapedBackslash = string "\\\\" $> '\\'
    regularChar = noneOf "$'"

foo :: Parser [String]
foo = between (string "{") (string "}") $ do
  let inner = between (char '\'') (char '\'') (many (try $ noneOf "'" <|> noneOf "'"))
  many (try $ noneOf "}")
  sepEndBy inner (char ',')

pFunctionCall :: Parser FuncInvocation
pFunctionCall = do
  funcName <- pLetName
  funcArgs <- pFuncArgs
  return $ case funcName of
    "star" -> BuiltInFuncInvocation Star funcArgs
    "anyof" -> BuiltInFuncInvocation AnyOf funcArgs
    "noneof" -> BuiltInFuncInvocation NoneOf funcArgs
    "maybe" -> BuiltInFuncInvocation Maybe funcArgs
    customName -> UserDefinedFuncInvocation funcName
  where
    argOpen = L.symbol hspace "("
    argClose = L.symbol hspace ")"
    argSep = L.symbol hspace ","
    pFuncArgs = L.lexeme hspace $ between argOpen argClose (sepEndBy functionArg argSep)
    functionArg = (ArgLiteral <$> try pStrictString) <|> (InvocationArg <$> try pFunctionCall)

pStrictString :: Parser String
pStrictString = between openQuote closeQuote chars
  where
    openQuote = char '\''
    closeQuote = L.lexeme hspace $ char '\''
    escapedQuote = string "\\'" $> '\''
    escapedBackslash = string "\\\\" $> '\\'
    regularChar = noneOf "'"
    chars = many $ try escapedQuote <|> try escapedBackslash <|> regularChar

pFunctionCapture :: LetDefP
pFunctionCapture = do
  captureTerms <- L.lexeme hspace $ between captureOpen captureClose (sepEndBy pCaptureTerm pSeparator)
  L.symbol hspace1 "as"
  captureName <- L.lexeme hspace $ pLetName
  return $ letDefCaptureInvocation captureName (sequence_ captureTerms)
  where
    captureOpen = L.symbol hspace "capture("
    captureClose = L.symbol hspace ")"
    pSeparator = L.symbol hspace ","
    pCaptureTerm = (letDefInvocation <$> pFunctionCall) <|> (letDefLiteral <$> pStrictString)

{-
<let-declaration-block-begin> := let
<let-declaration>             := <indent><let-name> = <let-definition>

<indent>                      := 2 spaces

<let-name>                    := [A-Za-z_]<let-name-rest>
<let-name-rest>               := [A-Za-z0-9_]<let-name-rest> | <nil>

<let-definition>              := <let-string> | <function-capture> | <function-call>

<let-string>                  := '<let-string-terms>'
<let-string-terms>            := <let-string-term><let-string-terms> | <nil>
<let-string-term>             := <let-expansion> | <let-string-literal>
<let-string-literal>          := [::anychar::, $'s and ''s escaped]<let-string-literal> | <nil>

<let-expansion>               := ${<let-expansion-terms>}
<let-expansion-terms>         := <let-expansion-term>, <let_expansion-terms> | <let-expansion-term> | <nil>
<let-expansion-term>          := <function-call> | <function-capture> | <let-string>

<function-call>               := <function-name>(<function-args>)
<function-name>               := <let-name>
<function-args>               := <function-arg>, <function-args> | <function-arg> | <nil>
<function-arg>                := <strict-string> | <function-call>

<strict-string>               := '<strict-string-chars>'
<strict-string-chars>         := <strict-string-char><strict-string-chars> | <nil>
<strict-string-char>          := [::anychar::, 's escaped]

<function-capture>            := capture(<function-capture-terms>) as <function-capture-name>
<function-capture-terms>      := <function-capture-term>, <function-capture-terms> | <function-capture-term>
<function-capture-term>       := <function-call> | <strict-string>
<function-capture-name>       := <let-name>
-}