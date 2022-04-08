module Test.Verify.Golden where

import Control.Monad (when)
import Data.Map qualified as Map ( fromList )
import Data.Text (Text)
import System.Directory (removeFile, doesFileExist)
import System.FilePath ((</>), (<.>))
import System.Info (os)

import Test.Tasty

import Vehicle
import Vehicle.Verify
import Vehicle.Verify.VerificationStatus hiding (version)
import Vehicle.Prelude
import Vehicle.Resource
import Vehicle.Backend.Prelude

import Test.GoldenUtils ( goldenFileTest )


--------------------------------------------------------------------------------
-- Tests

goldenTests :: TestTree
goldenTests = testGroup "GoldenTests"
  [ successTest
  ]

successTest :: TestTree
successTest = createTest "Marabou-success" inputFile networks
  where
  inputFile = "test/network/windController/windController.vcl"
  networks  = [("controller", "examples/network/windController/controller.onnx")]

--------------------------------------------------------------------------------
-- Utils

testDir :: FilePath
testDir = "test" </> "Test" </> "Verify" </> "Golden"

createTest :: String -> String -> [(Text, FilePath)] -> TestTree
createTest name inputFile networks =
  goldenFileTest name run ignoreList goldenFile outputFile
  where
  run = runTest name inputFile networks
  ignoreList = []
  goldenFile = testDir </> name <.> "txt"
  outputFile = testDir </> name <> "-output.txt"

runTest :: String -> String -> [(Text, FilePath)] -> IO ()
runTest name inputFile networks = do
  let outputFile   = testDir </> name <> "-output.txt"

  run $ Options
    { version       = False
    , outFile       = Nothing
    , errFile       = Nothing
    , logFile       = Nothing
    , commandOption = Verify $ VerifyOptions
      { verifier   = Marabou
      , inputFile  = inputFile
      , datasets   = mempty
      , networks   = Map.fromList networks
      , proofCache = Nothing
      }
    }

fixWindowsFilePaths :: FilePath -> IO ()
fixWindowsFilePaths outputFile = do
  contents <- readFile outputFile
  let newContents = fmap (\c -> if c == '\\' then '/' else c) contents
  writeFile outputFile newContents

mkStatus :: [(Text, PropertyStatus)] -> SpecificationStatus
mkStatus = SpecificationStatus . Map.fromList