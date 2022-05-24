{-# Language LambdaCase, BlockArguments#-}

module Main where

import           Flags (SedFlavor(..))
import           Lib (printMatchAndSub, repErrorOrSuccess)
import           ParseProgram (pProgram)
import           System.IO (IOMode( ReadMode ), withFile, hGetContents')
import           Text.Megaparsec (parse)
import Options.Applicative (Parser, ParserInfo, ReadM, execParser, strArgument)
import Options.Applicative.Builder (option, header, progDesc, fullDesc, help, short, metavar, long, info, eitherReader)
-- import Data.Either

data ProgramArgs = ProgramArgs String SedFlavor

pFileInput :: Parser String
pFileInput = strArgument
  ( metavar "SEDX_FILENAME"
    <> help ".sedx file" )

pSedFlavor :: Parser SedFlavor
pSedFlavor = option 
             strToSedFlavor 
             (short 's' 
              <> metavar "FLAVOR"
              <> long "sed_flavor"
              <> help "Which version of sed to produce output for.")

pProgramArgs :: Parser ProgramArgs
pProgramArgs = ProgramArgs <$> pFileInput <*> pSedFlavor

strToSedFlavor :: ReadM SedFlavor
strToSedFlavor = eitherReader \case
  "gnu-ere" -> Right GNUExtended
  "bsd-ere" -> Right BSDExtended
  "gnu-bre" -> Right GNU
  "bsd-bre" -> Right BSD
  str -> Left $ "unrecognized flavor " ++ str ++ ". <sed_flavor> must be one of {gnu-ere, bsd-ere, gnu-bre, bsd-bre}!"

programArgs :: ParserInfo ProgramArgs
programArgs = info 
              pProgramArgs 
              (fullDesc 
               <> progDesc "Compile a .sedx program into a sed search and replace expression"
               <> header "sedex: Make regexes easier.")

main' :: ProgramArgs -> IO ()
main' (ProgramArgs filename sedFlavor) = do
  programStr <- withFile filename ReadMode hGetContents'
  let output = do
                program <- parse pProgram filename programStr
                return $ printMatchAndSub sedFlavor program
  putStr $ repErrorOrSuccess output

main :: IO ()
main = execParser programArgs >>= main'

--main :: IO ()
--main = do
--  args <- getArgs
--  filename <- case args of
--    [filename] -> return filename
--    _          -> ioError (userError "Usage: sed-transpile <filename>")
--  programStr <- withFile filename ReadMode hGetContents'
--  let output = do
--                program <- parse pProgram filename programStr
--                return $ printMatchAndSub BSDExtended program
--  putStr $ repErrorOrSuccess output
--