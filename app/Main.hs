module Main where

import           Control.Monad
import           ExamplePrograms
import           Lib
import           ParseProgram
import           System.Environment
import           System.IO
import           System.IO.Error
import           Text.Megaparsec

main :: IO ()
main = do
  args <- getArgs
  filename <- case args of
    [filename] -> return filename
    _          -> ioError (userError "Usage: sed-transpile <filename>")
  programStr <- withFile filename ReadMode hGetContents'
  let output = do
                program <- parse pProgram filename programStr
                return $ printMatchAndSub program
  putStr $ repErrorOrSuccess output
