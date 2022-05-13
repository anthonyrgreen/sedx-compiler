module Main where

import Lib
import ParseProgram
import ExamplePrograms
import System.IO.Error
import Control.Monad
import System.Environment
import System.IO
import Text.Megaparsec

main :: IO ()
main = do
  args <- getArgs
  filename <- case args of 
    [filename] -> return filename
    _ -> ioError (userError "Usage: sed-transpile <filename>")
  programStr <- withFile filename ReadMode hGetContents'
  let output = do
                program <- parse pProgram filename programStr 
                return $ printMatchAndSub program
  putStr $ repErrorOrSuccess output
