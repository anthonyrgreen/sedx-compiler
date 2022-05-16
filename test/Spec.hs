{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.ByteString.Lazy            (ByteString)
import qualified Data.ByteString.Lazy.Char8      as BC
import           Lib
import           ParseProgram
import           System.FilePath                 (addExtension, dropExtension,
                                                  replaceExtension,
                                                  takeBaseName, takeDirectory,
                                                  takeFileName, (</>))
import           System.Process                  (readProcess)
import           Test.Tasty                      (TestTree, askOption,
                                                  defaultIngredients,
                                                  defaultMain,
                                                  defaultMainWithIngredients,
                                                  includingOptions, testGroup)
import           Test.Tasty.Golden               (findByExtension,
                                                  goldenVsStringDiff)
import           Text.Megaparsec
import           Text.Megaparsec.Error           (errorBundlePretty)
-- import Flags.Applicative (boolFlag, FlagsParser, parseSystemFlagsOrDie)
import           Control.Monad                   (when)
import           Data.Function                   ((&))
import           Data.Maybe                      (Maybe (Just))
import           Data.Proxy
import           Flags
import           Options.Applicative             (ParserInfo (ParserInfo),
                                                  execParser, help, long, short,
                                                  switch)
import           Options.Applicative.Help.Chunk  (stringChunk)
import           Options.Applicative.Help.Pretty (text)
import           Options.Applicative.Types       (ArgPolicy (Intersperse))
import           System.IO                       (hPutStrLn, stderr)
import           Test.Tasty.Options
import           Type.Reflection
import           Utils

verboseFlag = switch $ short 'v' <> long "verbose" <> help "If true, print compiled sed command."

newtype VerboseFlag = VerboseFlag Bool deriving Typeable
verboseFlagDescription :: OptionDescription
verboseFlagDescription = Option (Proxy :: Proxy VerboseFlag)

instance IsOption VerboseFlag where
  defaultValue = VerboseFlag False
  parseValue str = VerboseFlag <$> safeReadBool str
  optionName = "verbose"
  optionHelp = "If true, print compiled sed command."
  optionCLParser = flagCLParser (Just 'v') (VerboseFlag True)


sedFlavors = [BSD, BSDExtended, GNU, GNUExtended]
sedFlavorFileExt :: SedFlavor -> String
sedFlavorFileExt = \case
  BSD         -> "bsd"
  BSDExtended -> "bsdExt"
  GNU         -> "gnu"
  GNUExtended -> "gnuExt"

main :: IO ()
main = do
  searchAndReplaceSedxFiles <- findByExtension [".sedx"] "test/search-and-replace"
  sedxCompilationFiles <- findByExtension [".sedx"] "test/sedx-compilation"
  let ingredients = includingOptions [verboseFlagDescription] : defaultIngredients
      testTree = sedReplacementTests searchAndReplaceSedxFiles sedxCompilationFiles & askOption
  defaultMainWithIngredients ingredients testTree


sedReplacementTests :: [String] -> [String] -> VerboseFlag -> TestTree
sedReplacementTests searchAndReplaceSedxFiles sedxCompilationFiles (VerboseFlag verbose) = do
  testGroup "Sed tests" [searchAndReplaceTests, compilationTests]
  where
    searchAndReplaceParams = [(flavor, sedxFile) | flavor <- sedFlavors, sedxFile <- searchAndReplaceSedxFiles]
    compilationParams = [(flavor, sedxFile) | flavor <- sedFlavors, sedxFile <- sedxCompilationFiles]
    searchAndReplaceTests = testGroup "search and replace" (uncurry searchAndReplaceTest <$> searchAndReplaceParams)
    compilationTests = testGroup "compilation" (uncurry sedxCompilationTest <$> compilationParams)
    searchAndReplaceTest sedFlavor sedxFile = goldenVsStringDiff
                                              (testName sedFlavor sedxFile)
                                              diff
                                              (sedSearchAndReplaceGolden sedxFile)
                                              (runSedxAndReplace verbose sedFlavor sedxFile (sedInput sedxFile))
    sedxCompilationTest sedFlavor sedxFile = goldenVsStringDiff
                                             (testName sedFlavor sedxFile)
                                             diff
                                             (sedxCompilationGolden sedFlavor sedxFile)
                                             (runSedxCompilationTest sedFlavor sedxFile)
    testName sedFlavor = flip addExtension (sedFlavorFileExt sedFlavor) . dropExtension . takeFileName
    diff ref new = ["diff", "-u", ref, new]
    sedSearchAndReplaceGolden sedxFile = replaceExtension sedxFile "sedout.golden"
    sedxCompilationGolden sedFlavor sedxFile = replaceExtension sedxFile "sedxout" |> flip addExtension (sedFlavorFileExt sedFlavor) |> flip addExtension "golden"
    sedInput sedxFile = replaceExtension sedxFile "sedin"


runSedxAndReplace :: Bool -> SedFlavor -> String -> String -> IO BC.ByteString
runSedxAndReplace verbose sedFlavor sedxFile sedInput = do
  prog <- (parse pProgram sedxFile <$> readFile sedxFile) >>= either (fail . errorBundlePretty) pure
  let matchAndSubStr = printMatchAndSub sedFlavor prog
  when verbose $ hPutStrLn stderr matchAndSubStr
  input <- readFile sedInput
  let (sedCmd, extraArgs) = case sedFlavor of
                              BSD         -> ("sed", [])
                              BSDExtended -> ("sed", ["-E"])
                              GNU         -> ("gsed", [])
                              GNUExtended -> ("gsed", ["-E"])
  BC.pack <$> readProcess sedCmd (extraArgs ++ [matchAndSubStr]) input
  -- BC.pack <$> readProcess "sed" [matchAndSubStr] input


runSedxCompilationTest :: SedFlavor -> String -> IO BC.ByteString
runSedxCompilationTest sedFlavor sedxFile = do
  out <- (parse pProgram sedxFile <$> readFile sedxFile) >>=
          \parseOut -> pure (either errorBundlePretty (printMatchAndSub sedFlavor) parseOut)
  return $ BC.pack out
