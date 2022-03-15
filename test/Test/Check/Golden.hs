module Test.Check.Golden where

import Control.Monad (when)
import Data.Map qualified as Map ( fromList )
import Data.Text (Text)
import System.Directory (removeFile, doesFileExist)
import System.FilePath ((</>), (<.>))

import Test.Tasty

import Vehicle
import Vehicle.Check
import Vehicle.Verify.VerificationStatus hiding (version)
import Vehicle.Prelude
import Vehicle.NeuralNetwork ( hashNetwork )

import Test.GoldenUtils ( goldenFileTest )
import System.Info (os)

goldenTests :: TestTree
goldenTests = testGroup "GoldenTests"
  [ successTest
  , networkChangedTest
  , networkMissingTest
  ]

testDir :: FilePath
testDir = "test" </> "Test" </> "Check" </> "Golden"

successTest :: TestTree
successTest = createTest "success" status alterNetwork
  where
  status = mkStatus [("network1", Verified Nothing)]
  alterNetwork = const $ return ()

networkChangedTest :: TestTree
networkChangedTest = createTest "networkChanged" status alterNetwork
  where
  status = mkStatus [("network1", Verified Nothing)]
  alterNetwork f = writeFile f "alteredContents"

networkMissingTest :: TestTree
networkMissingTest = createTest "networkMissing" status alterNetwork
  where
  status = mkStatus [("network1", Verified Nothing)]
  alterNetwork = removeFile

createTest :: String -> SpecificationStatus -> (FilePath -> IO ()) -> TestTree
createTest name status alterNetwork =
  goldenFileTest name run ignoreList goldenFile outputFile
  where
  run = runTest name status alterNetwork
  -- Exclude any lines that contain ".onnx" as that signifies the line contains
  -- a filepath which won't be preserved across file systems.
  ignoreList   = [".onnx"]
  goldenFile   = testDir </> name <.> "txt"
  outputFile   = testDir </> name <> "-output.txt"

runTest :: String -> SpecificationStatus -> (FilePath -> IO ()) -> IO ()
runTest name status alterNetwork = do
  let outputFile   = testDir </> name <> "-output.txt"
  let proofCache   = testDir </> name <> "-proofFile"
  let networkFile  = testDir </> name <> "Network.onnx"

  writeFile networkFile "networkContents"
  networkHash <- hashNetwork networkFile
  let networkInfo =
        [ NetworkVerificationInfo "myNetwork" networkFile networkHash
        ]

  writeProofCache proofCache $ ProofCache
    { specVersion  = vehicleVersion
    , status       = status
    , networkInfo  = networkInfo
    , originalSpec = ""
    }

  alterNetwork networkFile

  run $ Options
    { version       = False
    , outFile       = Just outputFile
    , errFile       = Nothing
    , logFile       = Nothing
    , commandOption = Check $ CheckOptions
      { proofCache = proofCache
      }
    }

  removeFile proofCache
  removeFileIfExists networkFile

mkStatus :: [(Text, PropertyStatus)] -> SpecificationStatus
mkStatus = SpecificationStatus . Map.fromList