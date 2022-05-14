{-# LANGUAGE LambdaCase #-}
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
import Control.Monad.Trans.Free

type Parser = Parsec Void String
type LetDefP = Parser (LetDef ())
single_import_str = "  single_import := '${star(anyof('A-Za-z0-9')), maybe(', ')}'"
many_imports_str = "  many_imports := '${star(single_import())}'"
filename_str = "  filename := '${star(noneof('/ '))}'"
whole_path_str = "  whole_path := '${capture(filename()) as root, '/', star(filename(), '/'), capture(filename()) as filename}'"
match_str = "import {${capture(many_imports()) as imports}} from '${capture(whole_path()) as whole_path}';"

file_str = "let\n\
\  single_import := '${star(anyof('A-Za-z0-9')), maybe(', ')}'\n\
\  many_imports := '${star(single_import())}'\n\
\  filename := '${star(noneof('/ '))}'\n\
\  whole_path := '${capture(filename()) as root, '/', star(filename(), '/'), capture(filename()) as filename}'\n\
\match\n\
\import {${capture(many_imports()) as imports}} from '${capture(whole_path()) as whole_path}';\n\
\substitute\n\
\import {${imports}} from '${whole_path.root}/some/directory/string/${whole_path.filename}';\n"
test_pLetDef :: String -> IO ()
test_pLetDef = parseTest (pLetDecl $ mkPos 2)


upToNextLine = (hspace *> eol) $> ()


pProgram :: Parser (Program ())
pProgram = do
  L.symbol upToNextLine "let"
  letIndent <- L.indentGuard space GT pos1
  letDecls <- many (pLetDecl letIndent) <&> sequence_
  L.symbol upToNextLine "match"
  matchDef <- L.lexeme upToNextLine $ pMatchDef <&> ProgramAst.match
  L.symbol upToNextLine "substitute"
  subDef <- L.lexeme upToNextLine $ pSubDef <&> ProgramAst.substitute
  return $ do
    letDecls
    matchDef
    subDef


pLetDecl :: Pos -> Parser (Program ())
pLetDecl letIndent = do
  -- TODO: proper indentation check
  L.indentGuard space EQ letIndent
  name <- L.lexeme hspace1 pLetName
  L.symbol hspace1 ":="
  def <- L.lexeme space pLetDef
  return $ letDecl name def


pLetName :: Parser String
pLetName = label "pLetName" $ do
  first <- satisfy (\s -> isAlpha s || s == '_')
  rest <- many $ satisfy (\s -> isAlphaNum s || s == '_')
  return $ first : rest


pLetDef :: LetDefP
pLetDef = label "pLetDef" $ try pLetString <|> pFunctionCapture <|> (letDefInvocation <$> pFunctionCall)


pLetString :: LetDefP
pLetString = label "pLetString" $ between openQuote closeQuote $ do
  letStringTerms <- many pLetStringInteriorTerm
  return $ if null letStringTerms then letDefLiteral "" else sequence_ letStringTerms
  where
    openQuote = char '\''
    closeQuote = L.lexeme hspace $ char '\''
    pLetStringInteriorTerm = try pLetExpansion <|> try pLetStringLiteral -- nil?


pLetExpansion :: LetDefP
pLetExpansion = label "pLetExpansion" $ sequence_ <$> pLetExpansionTerms
  where
    openBrace = L.lexeme hspace $ string "${"
    closeBrace = L.lexeme hspace $ string "}"
    pLetSeparator = L.symbol hspace ","
    pLetExpansionTerm = L.lexeme hspace $ pLetString <|> try pFunctionCapture <|> (letDefInvocation <$> pFunctionCall)
    pLetExpansionTerms = between openBrace closeBrace $ sepEndBy1 pLetExpansionTerm pLetSeparator


pLetStringLiteral :: LetDefP
pLetStringLiteral = label "pLetStringLiteral" $ letDefLiteral <$> literalChars
  -- TODO: we're using 'some' here: what about empty strings??? (If we don't use 'some' we get infinite recursion)
  -- TODO: escape close brace in the right context
  where
    escapedCash = string "\\$" $> '$'
    escapedQuote = string "\\'" $> '\''
    escapedBackslash = string "\\\\" $> '\\'
    regularChar = noneOf "$'"
    literalChars = some $ try escapedCash <|> try escapedQuote <|> try escapedBackslash <|> regularChar


pFunctionCall :: Parser FuncInvocation
pFunctionCall = label "pFunctionCall" $ do
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
pStrictString = label "pStrictString" $ between openQuote closeQuote chars
  where
    openQuote = char '\''
    closeQuote = L.lexeme hspace $ char '\''
    escapedQuote = string "\\'" $> '\''
    escapedBackslash = string "\\\\" $> '\\'
    regularChar = noneOf "'"
    chars = many $ try escapedQuote <|> try escapedBackslash <|> regularChar


pFunctionCapture :: LetDefP
-- pFunctionCapture = label "pFunctionCapture" $ letDefCaptureInvocation <$> captureName <*> captureTerms
pFunctionCapture = label "pFunctionCapture" $ do
  captureTerms <- sequence_ <$> L.lexeme hspace pCaptureTerms
  captureName <- L.symbol hspace1 "as" *> L.lexeme hspace pLetName
  return $ letDefCaptureInvocation captureName captureTerms
  where
    captureOpen = L.symbol hspace "capture("
    captureClose = L.symbol hspace ")"
    pSeparator = L.symbol hspace ","
    pCaptureTerm = (letDefInvocation <$> pFunctionCall) <|> (letDefLiteral <$> pStrictString)
    pCaptureTerms = between captureOpen captureClose (sepEndBy pCaptureTerm pSeparator)
    -- captureTerms = sequence_ <$> L.lexeme hspace pCaptureTerms
    -- captureName = L.symbol hspace1 "as" *> L.lexeme hspace pLetName


pMatchDef :: Parser (MatchDef ())
pMatchDef = label "pMatchDef" $ sequence_ <$> many matchTerms
  where
    matchTerms = (matchLiteral <$> pMatchLiteral) <|> pMatchExpansion
    -- TODO: allow unbackslashed if $ but not ${?
    escapedCashChar = string "\\$" $> '$'
    regularChar = noneOf "$\n"
    pMatchLiteral = some $ escapedCashChar <|> regularChar
    pMatchExpansion = letDefToMatchDef <$> pLetExpansion


pSubDef :: Parser (SubDef ())
pSubDef = label "pSubDef" $ sequence_ <$> many subTerms
  where
    subTerms = (subLiteral <$> try pSubLiteral) <|> pSubCaptureRef
    -- TODO: allow empty literals
    pSubLiteral = some $ escapedCashChar <|> regularChar
    escapedCashChar = string "\\$" $> '$'
    regularChar = noneOf "$\n"
    pSubCaptureRef = between openBrace closeBrace (sepEndBy1 pLetName pSeparator)
        <&> (\case
                [ref] -> subCaptureReference ref
                refs -> subScopedCaptureReference refs)
    openBrace = L.lexeme hspace $ string "${"
    closeBrace = L.lexeme hspace $ string "}"
    pSeparator = char '.'


letDefToMatchDef :: LetDef a -> MatchDef a
letDefToMatchDef  = iterM processLine
  where
    processLine letDef = case letDef of
      LetDefLiteral literal next -> matchLiteral literal >> next
      LetDefInvocation funcInvocation next -> matchInvocation funcInvocation >> next
      LetDefCaptureInvocation name def next -> matchCaptureInvocation name (letDefToMatchDef def) >> next
