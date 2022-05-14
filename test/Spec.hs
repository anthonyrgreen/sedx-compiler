
import System.Process (readProcess)
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BC
import           Lib
import           ParseProgram
import           System.FilePath            (takeBaseName, takeDirectory, (</>), replaceExtension, dropExtension, takeFileName, addExtension)
import           Test.Tasty                 (TestTree, defaultMain, testGroup)
import           Test.Tasty.Golden          (findByExtension, goldenVsStringDiff)
import           Text.Megaparsec
import           Text.Megaparsec.Error      (errorBundlePretty)

main :: IO ()
main = defaultMain =<< sedReplacementTests

-- sedReplacementTests :: IO TestTree
-- sedReplacementTests = do
--   sedxFiles <- findByExtension [".sedx"] "sedx-inputs"
--   return $ testGroup ".sedx golden tests"
--     [ goldenVsStringDiff
--         testName -- test name
--         sedOutputGolden -- golden file path
--         (runSedxAndReplace sedxFile sedInput) -- action whose result is tested
--     | sedxFile <- sedxFiles
--     , let testName = takeBaseName sedxFile
--     , let sedOutputGolden = replaceExtension sedxFile ".out"
--     , let sedInput = replaceExtension sedxFile ".in"
--     ]
sedReplacementTests :: IO TestTree
sedReplacementTests = do
  srcs <- findByExtension [".sedx"] "test/sedx-inputs"
  pure $ testGroup ".sedx golden tests" (aTest <$> srcs)
  where
    aTest sedxFile = goldenVsStringDiff
                       testName -- test name
                       diff
                       sedOutputGolden -- golden file path
                       (runSedxAndReplace sedxFile sedInput) -- action whose result is tested
      where
        testName = dropExtension $ takeFileName sedxFile
        sedOutputGolden = "test/sed-outputs" </> addExtension testName "out"
        sedInput = "test/sed-inputs" </> addExtension testName "in"
        diff ref new = ["diff", "-u", ref, new]



runSedxAndReplace :: String -> String -> IO BC.ByteString
runSedxAndReplace sedxFile sedInput = do
  prog <- (parse pProgram sedxFile <$> readFile sedxFile) >>= either (fail . errorBundlePretty) pure
  input <- readFile sedInput
  BC.pack <$> readProcess "gsed" ["-e", printMatchAndSub prog] input

