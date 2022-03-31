module Test.Compile.Golden
  ( goldenTests
  ) where

import Control.Exception ( catch, throwIO )
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

import Vehicle
import Vehicle.Prelude
import Vehicle.Compile
import Vehicle.Backend.Prelude

import Test.Utils
import Test.GoldenUtils

--------------------------------------------------------------------------------
-- Tests

goldenTests :: TestTree
goldenTests = testGroup "GoldenTests" $
  map makeGoldenTests [
    -- Examples
    (Examples, "windController",      [VNNLibBackend, AgdaBackend, MarabouBackend]),
    (Examples, "acasXu",              [AgdaBackend]),

    -- Realistic tests
    (Tests,    "acasXu-property6",    [VNNLibBackend, AgdaBackend, MarabouBackend]),
    (Tests,    "andGate",             [VNNLibBackend, AgdaBackend]),
    (Tests,    "autoencoderError",    [VNNLibBackend, AgdaBackend]),
    (Tests,    "increasing",          [VNNLibBackend, AgdaBackend]),
    (Tests,    "monotonicity",        [VNNLibBackend, AgdaBackend]),
    (Tests,    "reachability",        [VNNLibBackend, AgdaBackend, MarabouBackend]),

    -- Simple tests of Vehicle syntax
    (Tests,    "simple-quantifierIn", [AgdaBackend]),
    (Tests,    "simple-let",          [AgdaBackend]),
    (Tests,    "simple-defaultFin",   [AgdaBackend]),
    (Tests,    "simple-defaultInt",   [AgdaBackend]),
    (Tests,    "simple-defaultRat",   [AgdaBackend])
    ]

--------------------------------------------------------------------------------
-- Test infrastructure

goldenDir :: FilePath
goldenDir = "test" </> "Test" </> "Compile" </> "Golden"

getGoldenFilepathSuffix :: Backend -> String
getGoldenFilepathSuffix (Verifier Marabou) = "-marabou"
getGoldenFilepathSuffix (Verifier VNNLib)  = ".vnnlib"
getGoldenFilepathSuffix (ITP Agda)         = ".agda"

makeGoldenTests :: (SpecLocation, String, [Backend]) -> TestTree
makeGoldenTests (location, name, outputTargets) = testGroup name tests
  where
    tests :: [TestTree]
    tests = map (makeIndividualTest location name) outputTargets

makeIndividualTest :: SpecLocation -> String -> Backend -> TestTree
makeIndividualTest location name backend = test
  where
  testName       = name <> "-" <> show backend
  filePathSuffix = getGoldenFilepathSuffix backend
  moduleName     = name <> "-output"
  inputFile      = locationDir location </> name </> name <.> ".vcl"
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
    , logFile       = Nothing --Just Nothing
    , commandOption = Compile $ CompileOptions
      { target       = backend
      , inputFile    = inputFile
      , outputFile   = Just outputFile
      , networks     = mempty
      , modulePrefix = Nothing
      , proofCache   = Just "proofcache.vclp"
      }
    }