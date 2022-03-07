module Test.Check.Golden where

import Control.Monad (when)
import System.Directory (removeFile, doesFileExist)
import System.FilePath ((</>), (<.>))

import Test.Tasty

import Vehicle
import Vehicle.Check
import Vehicle.Verify.VerificationStatus hiding (version)
import Vehicle.Prelude
import Vehicle.NeuralNetwork ( hashNetwork )

import Test.GoldenUtils ( goldenFileTest )

goldenTests :: TestTree
goldenTests = testGroup "GoldenTests"
  [ successTest
  , networkChangedTest
  , networkMissingTest
  ]

testDir :: FilePath
testDir = "test" </> "Test" </> "Check" </> "Golden"

successTest :: TestTree
successTest = createTest "success" alterNetwork
  where
  alterNetwork = const $ return ()

networkChangedTest :: TestTree
networkChangedTest = createTest "networkChanged" alterNetwork
  where
  alterNetwork f = writeFile f "alteredContents"

networkMissingTest :: TestTree
networkMissingTest = createTest "networkMissing" alterNetwork
  where
  alterNetwork = removeFile

createTest :: String -> (FilePath -> IO ()) -> TestTree
createTest name alterNetwork = goldenFileTest name run goldenFile outputFile
  where
  goldenFile   = testDir </> name <.> "txt"
  outputFile   = testDir </> name <> "-output.txt"
  run = runTest name alterNetwork

runTest :: String -> (FilePath -> IO ()) -> IO ()
runTest name alterNetwork = do
  let outputFile   = testDir </> name <> "-output.txt"
  let databaseFile = testDir </> name <> "-proofFile"
  let networkFile  = testDir </> name <> "Network.onnx"

  writeFile networkFile "networkContents"
  networkHash <- hashNetwork networkFile
  let networkInfo =
        [ NetworkVerificationInfo "myNetwork" networkFile networkHash
        ]

  writeSpecificationStatus databaseFile $ SpecificationStatus
    { specVersion  = vehicleVersion
    , status       = Verified
    , networkInfo  = networkInfo
    , originalSpec = ""
    }

  alterNetwork networkFile

  run $ Options
    { version       = False
    , logFile       = Just $ Just outputFile
    , errFile       = Nothing
    , commandOption = Check $ CheckOptions
      { databaseFile = databaseFile
      }
    }

  removeFile databaseFile
  removeFileIfExists networkFile