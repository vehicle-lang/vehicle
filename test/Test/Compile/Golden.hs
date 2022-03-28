module Test.Compile.Golden
  ( goldenTests
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Bifunctor (first)
import Test.Tasty
import Test.Tasty.Golden.Advanced (goldenTest)
import System.Exit (exitFailure)
import System.FilePath (takeFileName, splitPath, (<.>), (</>))
import System.Directory (removeFile, removeDirectory)
import System.IO.Error (isDoesNotExistError)
import Control.Exception ( catch, throwIO )
import Debug.Trace (traceShowId)

import Vehicle
import Vehicle.Prelude
import Vehicle.Compile
import Vehicle.Backend.Prelude

import Test.GoldenUtils

--------------------------------------------------------------------------------
-- Tests

goldenTests :: TestTree
goldenTests = testGroup "GoldenTests" $
  map makeGoldenTests [
    -- Realistic tests
    ("acasXu-property6",       [VNNLibBackend, AgdaBackend, MarabouBackend]),
    ("andGate",                [VNNLibBackend, AgdaBackend]),
    ("autoencoderError",       [VNNLibBackend, AgdaBackend]),
    ("increasing",             [VNNLibBackend, AgdaBackend]),
    ("monotonicity",           [VNNLibBackend, AgdaBackend]),
    ("reachability",           [VNNLibBackend, AgdaBackend, MarabouBackend]),
    ("windController",         [VNNLibBackend, AgdaBackend, MarabouBackend]),

    -- Simple tests of Vehicle syntax
    ("simple-quantifierIn",    [AgdaBackend]),
    ("simple-let",             [AgdaBackend])
    ]

--------------------------------------------------------------------------------
-- Test infrastructure

specDir :: FilePath
specDir = "test" </> "specs"

goldenDir :: FilePath
goldenDir = "test" </> "Test" </> "Compile" </> "Golden"

getGoldenFilepathSuffix :: Backend -> String
getGoldenFilepathSuffix (Verifier Marabou) = "-marabou"
getGoldenFilepathSuffix (Verifier VNNLib)  = ".vnnlib"
getGoldenFilepathSuffix (ITP Agda)         = ".agda"

makeGoldenTests :: (String, [Backend]) -> TestTree
makeGoldenTests (name, outputTargets) = testGroup name tests
  where
    tests :: [TestTree]
    tests = map (makeIndividualTest name) outputTargets

makeIndividualTest :: String -> Backend -> TestTree
makeIndividualTest name backend = test
  where
  testName       = name <> "-" <> show backend
  filePathSuffix = getGoldenFilepathSuffix backend
  moduleName     = name <> "-output"
  inputFile      = specDir </> name </> name <.> ".vcl"
  outputFile     = goldenDir </> name </> name <> "-temp-output" <> filePathSuffix
  goldenFile     = goldenDir </> name </> name <> "-output"      <> filePathSuffix
  isFolderOutput = backend == MarabouBackend
  run            = runTest inputFile outputFile moduleName backend
  -- The proofCache field is a filepath and therefore does not transfer across
  -- systems, therefore ignore it when comparing the files.
  ignoreList     = ["proofCache" | backend == AgdaBackend]

  testFn = if isFolderOutput then goldenDirectoryTest else goldenFileTest
  test = testFn testName run ignoreList goldenFile outputFile

runTest :: FilePath -> FilePath -> String -> Backend -> IO ()
runTest inputFile outputFile modulePath backend = do
  run $ Options
    { version       = False
    , outFile       = Nothing
    , errFile       = Nothing
    , logFile       = Nothing -- Just Nothing
    , commandOption = Compile $ CompileOptions
      { target       = backend
      , inputFile    = inputFile
      , outputFile   = Just outputFile
      , networks     = mempty
      , modulePrefix = Nothing
      , proofCache   = Just "proofcache.vclp"
      }
    }