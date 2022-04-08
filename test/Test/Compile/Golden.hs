module Test.Compile.Golden
  ( goldenTests
  ) where

import Control.Exception ( catch, throwIO )
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Bifunctor (first)
import Data.Map (Map)
import Data.Map qualified as Map
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
  map makeGoldenTests
    -- Worked examples
    [ (Examples, "windController",
        [VNNLibBackend, AgdaBackend, MarabouBackend],
        [])

    , (Examples, "acasXu",
        [AgdaBackend, MarabouBackend],
        [])

    -- Realistic tests
    , (Tests, "andGate",
        [VNNLibBackend, AgdaBackend],
        [])

    , (Tests, "autoencoderError",
        [VNNLibBackend, AgdaBackend],
        [])

    , (Tests, "increasing",
        [VNNLibBackend, AgdaBackend],
        [])

    , (Tests, "monotonicity",
        [VNNLibBackend, AgdaBackend],
        [])

    , (Tests,    "reachability",
        [VNNLibBackend, AgdaBackend, MarabouBackend],
        [])

    -- Simple tests of Vehicle syntax
    , (Tests, "simple-quantifierIn",
        [AgdaBackend],
        [])

    , (Tests, "simple-let",
        [AgdaBackend],
        [])

    , (Tests, "simple-defaultFin",
        [AgdaBackend],
        [])

    , (Tests, "simple-defaultInt",
        [AgdaBackend],
        [])

    , (Tests, "simple-defaultRat",
        [AgdaBackend],
        [])

    , (Tests, "simple-constant-input",
        [AgdaBackend, MarabouBackend],
        [])

    -- , (Tests, "simple-dataset",
    --     [MarabouBackend],
    --     [("d", "dataset-nat-4.idx")])
    ]

--------------------------------------------------------------------------------
-- Test infrastructure

goldenDir :: FilePath
goldenDir = "test" </> "Test" </> "Compile" </> "Golden"

getGoldenFilepathSuffix :: Backend -> String
getGoldenFilepathSuffix (Verifier Marabou) = "-marabou"
getGoldenFilepathSuffix (Verifier VNNLib)  = ".vnnlib"
getGoldenFilepathSuffix (ITP Agda)         = ".agda"

makeGoldenTests :: (SpecLocation, String, [Backend], [(Text, FilePath)]) -> TestTree
makeGoldenTests (location, name, outputTargets, datasetLocations) =
  let datasetMap = Map.fromList datasetLocations in
  let datasets = fmap ((locationDir location </> name) </>) datasetMap in
  let makeTest = makeIndividualTest location name datasets in
  testGroup name (map makeTest outputTargets)

makeIndividualTest :: SpecLocation
                   -> String
                   -> Map Text FilePath
                   -> Backend
                   -> TestTree
makeIndividualTest location name datasets backend = test
  where
  testName       = name <> "-" <> show backend
  filePathSuffix = getGoldenFilepathSuffix backend
  moduleName     = name <> "-output"
  inputFile      = locationDir location </> name </> name <.> ".vcl"
  outputFile     = goldenDir </> name </> name <> "-temp-output" <> filePathSuffix
  goldenFile     = goldenDir </> name </> name <> "-output"      <> filePathSuffix
  isFolderOutput = backend == MarabouBackend
  run            = runTest inputFile outputFile moduleName backend datasets

  testFn = if isFolderOutput then goldenDirectoryTest else goldenFileTest
  test = testFn testName run windowsFilepathException goldenFile outputFile

runTest :: FilePath -> FilePath -> String -> Backend -> Map Text FilePath -> IO ()
runTest inputFile outputFile modulePath backend datasets = do
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
      , datasets     = datasets
      , modulePrefix = Nothing
      , proofCache   = Just "proofcache.vclp"
      }
    }