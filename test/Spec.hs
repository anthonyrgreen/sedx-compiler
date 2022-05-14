import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)
import System.FilePath (takeBaseName, replaceExtension)

main :: IO ()
main = defaultMain =<< sedReplacementTests

sedReplacementTests :: IO TestTree
sedReplacementTests = do
  sedxFiles <- findByExtension [".sedx"] "."
  return $ testGroup ".sedx golden tests"
    [ goldenVsString
        testName -- test name
        sedOutputGolden -- golden file path
        (runSedxAndReplace sedxFile sedInput) -- action whose result is tested
    | sedxFile <- sedxFiles
    , let testName = takeBaseName sedxFile
    , let sedOutputGolden = replaceExtension sedxFile ".out"
    , let sedInput = replaceExtension sedxFile ".in"
    ]



runSedxAndReplace :: String -> String -> IO Bytestring
runSedxAndReplace sedxFile sedInput = undefined
  
