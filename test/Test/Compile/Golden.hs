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

import Test.Compile.Utils
import Test.GoldenUtils

--------------------------------------------------------------------------------
-- Tests

goldenTests :: TestTree
goldenTests = testGroup "GoldenTests" $
  map makeGoldenTests
    -- Worked examples
    [ testSpec
      { testName       = "windController"
      , testLocation   = Examples
      , testTargets    = [VNNLibBackend, AgdaBackend, MarabouBackend]
      }

    , testSpec
      { testName       = "acasXu"
      , testLocation   = Examples
      , testTargets    = [AgdaBackend, MarabouBackend]
      }

    -- Realistic tests
    , testSpec
      { testName       = "andGate"
      , testLocation   = Tests
      , testTargets    = [VNNLibBackend, AgdaBackend]
      }

    , testSpec
      { testName       = "autoencoderError"
      , testLocation   = Tests
      , testTargets    = [VNNLibBackend, AgdaBackend]
      }

    , testSpec
      { testName       = "increasing"
      , testLocation   = Tests
      , testTargets    = [VNNLibBackend, AgdaBackend]
      }

    , testSpec
      { testName       = "monotonicity"
      , testLocation   = Tests
      , testTargets    = [VNNLibBackend, AgdaBackend]
      }

    , testSpec
      { testName       = "reachability"
      , testLocation   = Tests
      , testTargets    = [VNNLibBackend, AgdaBackend, MarabouBackend]
      }

    , testSpec
      { testName       = "bounded"
      , testLocation   = Tests
      , testTargets    = [LossFunction]
      }

    -- Simple tests of Vehicle syntax
    , testSpec
      { testName       = "simple-quantifierIn"
      , testLocation   = Tests
      , testTargets    = [AgdaBackend]
      }

    , testSpec
      { testName       = "simple-let"
      , testLocation   = Tests
      , testTargets    = [AgdaBackend]
      }

    , testSpec
      { testName       = "simple-if"
      , testLocation   = Tests
      , testTargets    = [MarabouBackend, AgdaBackend]
      }

    , testSpec
      { testName       = "simple-defaultFin"
      , testLocation   = Tests
      , testTargets    = [AgdaBackend]
      }

    , testSpec
      { testName       = "simple-defaultInt"
      , testLocation   = Tests
      , testTargets    = [AgdaBackend]
      }

    , testSpec
      { testName       = "simple-defaultRat"
      , testLocation   = Tests
      , testTargets    = [AgdaBackend]
      }

    , testSpec
      { testName       = "simple-constantInput"
      , testLocation   = Tests
      , testTargets    = [AgdaBackend, MarabouBackend]
      }

    -- , testSpec
    --   { testName       = "simple-vectorType"
    --   , testLocation   = Tests
    --   , testTargets    = [AgdaBackend]
    --   }
    ]

--------------------------------------------------------------------------------
-- Test infrastructure

goldenDir :: FilePath
goldenDir = "test" </> "Test" </> "Compile" </> "Golden"

getGoldenFilepathSuffix :: Backend -> String
getGoldenFilepathSuffix (Verifier Marabou) = "-marabou"
getGoldenFilepathSuffix (Verifier VNNLib)  = ".vnnlib"
getGoldenFilepathSuffix (ITP Agda)         = ".agda"
getGoldenFilepathSuffix LossFunction       = ".json"

makeGoldenTests :: TestSpec -> TestTree
makeGoldenTests spec@TestSpec{..} =
  let resources = testResources spec in
  let makeTest = makeIndividualTest testLocation testName resources in
  testGroup testName (map makeTest testTargets)

makeIndividualTest :: TestLocation
                   -> String
                   -> Resources
                   -> Backend
                   -> TestTree
makeIndividualTest location name datasets backend = test
  where
  testName       = name <> "-" <> show backend
  filePathSuffix = getGoldenFilepathSuffix backend
  moduleName     = name <> "-output"
  inputFile      = locationDir location name </> name <.> ".vcl"
  outputFile     = goldenDir </> name </> name <> "-temp-output" <> filePathSuffix
  goldenFile     = goldenDir </> name </> name <> "-output"      <> filePathSuffix
  isFolderOutput = backend == MarabouBackend
  run            = runTest inputFile outputFile moduleName backend datasets

  testFn = if isFolderOutput then goldenDirectoryTest else goldenFileTest
  test = testFn testName run omitFilePaths goldenFile outputFile

runTest :: FilePath -> FilePath -> String -> Backend -> Resources -> IO ()
runTest inputFile outputFile modulePath backend Resources{..} = do
  run $ Options
    { version       = False
    , outFile       = Nothing
    , errFile       = Nothing
    , logFile       = Nothing -- Just Nothing
    , debugLevel    = 2
    , commandOption = Compile $ CompileOptions
      { target           = backend
      , specification    = inputFile
      , outputFile       = Just outputFile
      , networkLocations = networks
      , datasetLocations = datasets
      , parameterValues  = parameters
      , modulePrefix     = Nothing
      , proofCache       = Just "proofcache.vclp"
      }
    }