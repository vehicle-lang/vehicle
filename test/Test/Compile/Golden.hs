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
    [ TestSpec
      { testName     = "windController"
      , testLocation = Examples
      , testTargets  = [VNNLibBackend, AgdaBackend, MarabouBackend]
      , testDatasets = []
      }

    , TestSpec
      { testName     = "acasXu"
      , testLocation = Examples
      , testTargets  = [AgdaBackend, MarabouBackend]
      , testDatasets = []
      }

    -- Realistic tests
    , TestSpec
      { testName     = "andGate"
      , testLocation = Tests
      , testTargets  = [VNNLibBackend, AgdaBackend]
      , testDatasets = []
      }

    , TestSpec
      { testName     = "autoencoderError"
      , testLocation = Tests
      , testTargets  = [VNNLibBackend, AgdaBackend]
      , testDatasets = []
      }

    , TestSpec
      { testName     = "increasing"
      , testLocation = Tests
      , testTargets  = [VNNLibBackend, AgdaBackend]
      , testDatasets = []
      }

    , TestSpec
      { testName     = "monotonicity"
      , testLocation = Tests
      , testTargets  = [VNNLibBackend, AgdaBackend]
      , testDatasets = []
      }

    , TestSpec
      { testName     = "reachability"
      , testLocation = Tests
      , testTargets  = [VNNLibBackend, AgdaBackend, MarabouBackend]
      , testDatasets = []
      }

    -- Simple tests of Vehicle syntax
    , TestSpec
      { testName     = "simple-quantifierIn"
      , testLocation = Tests
      , testTargets  = [AgdaBackend]
      , testDatasets = []
      }

    , TestSpec
      { testName     = "simple-let"
      , testLocation = Tests
      , testTargets  = [AgdaBackend]
      , testDatasets = []
      }

    , TestSpec
      { testName     = "simple-defaultFin"
      , testLocation = Tests
      , testTargets  = [AgdaBackend]
      , testDatasets = []
      }

    , TestSpec
      { testName     = "simple-defaultInt"
      , testLocation = Tests
      , testTargets  = [AgdaBackend]
      , testDatasets = []
      }

    , TestSpec
      { testName     = "simple-defaultRat"
      , testLocation = Tests
      , testTargets  = [AgdaBackend]
      , testDatasets = []
      }

    , TestSpec
      { testName     = "simple-constant-input"
      , testLocation = Tests
      , testTargets  = [AgdaBackend, MarabouBackend]
      , testDatasets = []
      }

    , TestSpec
      { testName     = "bounded"
      , testLocation = Tests
      , testTargets  = [LossFunction]
      , testDatasets = []
      }
    ]

--------------------------------------------------------------------------------
-- Test infrastructure

data TestSpec = TestSpec
    { testName      :: String
    , testLocation  :: SpecLocation
    , testTargets   :: [Backend]
    , testDatasets  :: [(Text, FilePath)]
    }

goldenDir :: FilePath
goldenDir = "test" </> "Test" </> "Compile" </> "Golden"

getGoldenFilepathSuffix :: Backend -> String
getGoldenFilepathSuffix (Verifier Marabou) = "-marabou"
getGoldenFilepathSuffix (Verifier VNNLib)  = ".vnnlib"
getGoldenFilepathSuffix (ITP Agda)         = ".agda"
getGoldenFilepathSuffix LossFunction       = ".json"

makeGoldenTests :: TestSpec -> TestTree
makeGoldenTests TestSpec{..} =
  let datasetMap = Map.fromList testDatasets in
  let datasets = fmap ((locationDir testLocation </> testName) </>) datasetMap in
  let makeTest = makeIndividualTest testLocation testName datasets in
  testGroup testName (map makeTest testTargets)

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
  test = testFn testName run omitFilePaths goldenFile outputFile

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