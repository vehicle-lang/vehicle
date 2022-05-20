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

import Test.GoldenUtils
import Test.Compile.Utils

--------------------------------------------------------------------------------
-- Tests

goldenTests :: MonadTest m => m TestTree
goldenTests = testGroup "GoldenTests" <$>
  traverse makeGoldenTests
    -- Worked examples
    [ testSpec
      { testName       = "windController"
      , testLocation   = Examples
      , testTargets    = [AgdaBackend, MarabouBackend]
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
      , testTargets    = [AgdaBackend, MarabouBackend]
      }

    , testSpec
      { testName       = "autoencoderError"
      , testLocation   = Tests
      , testTargets    = [AgdaBackend] -- MarabouBackend
      }

    , testSpec
      { testName       = "increasing"
      , testLocation   = Tests
      , testTargets    = [AgdaBackend, MarabouBackend]
      }

    , testSpec
      { testName       = "monotonicity"
      , testLocation   = Tests
      , testTargets    = [AgdaBackend, MarabouBackend]
      }

    , testSpec
      { testName       = "reachability"
      , testLocation   = Tests
      , testTargets    = [AgdaBackend, MarabouBackend]
      }

    , testSpec
      { testName       = "bounded"
      , testLocation   = Tests
      , testTargets    = [LossFunction]
      }

    -- Simple tests of Vehicle syntax
    , testSpec
      { testName       = "simple-quantifier"
      , testLocation   = Tests
      , testTargets    = [AgdaBackend]
      }

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
      , testTargets    = [AgdaBackend, MarabouBackend]
      }

    , testSpec
      { testName       = "simple-defaultIndex"
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

    , testSpec
      { testName       = "simple-arithmetic"
      , testLocation   = Tests
      , testTargets    = [AgdaBackend]
      }

    , testSpec
      { testName       = "simple-tensor"
      , testLocation   = Tests
      , testTargets    = [AgdaBackend]
      }

    , testSpec
      { testName       = "simple-implicitParam"
      , testLocation   = Tests
      , testTargets    = [MarabouBackend]
      , testDatasets   = [("d", "dataset-rat-4.idx")]
      }

    , testSpec
      { testName       = "simple-gaussianElim"
      , testLocation   = Tests
      , testTargets    = [MarabouBackend]
      }

    , testSpec
      { testName       = "simple-foreach"
      , testLocation   = Tests
      , testTargets    = [MarabouBackend]
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
getGoldenFilepathSuffix (ITP Agda)         = ".agda"
getGoldenFilepathSuffix LossFunction       = ".json"

makeGoldenTests :: MonadTest m => TestSpec -> m TestTree
makeGoldenTests spec@TestSpec{..} = do
  let resources = testResources spec
  let makeTest = makeIndividualTest testLocation testName resources
  testGroup testName <$> traverse makeTest testTargets

makeIndividualTest :: MonadTest m
                   => TestLocation
                   -> String
                   -> Resources
                   -> Backend
                   -> m TestTree
makeIndividualTest location name datasets backend = do
  loggingSettings <- getTestLoggingSettings

  let testName       = name <> "-" <> show backend
  let filePathSuffix = getGoldenFilepathSuffix backend
  let moduleName     = name <> "-output"
  let inputFile      = locationDir location name </> name <.> ".vcl"
  let outputFile     = goldenDir </> name </> name <> "-temp-output" <> filePathSuffix
  let goldenFile     = goldenDir </> name </> name <> "-output"      <> filePathSuffix
  let isFolderOutput = backend == MarabouBackend

  let run = runTest loggingSettings inputFile outputFile moduleName backend datasets
  let testFn = if isFolderOutput then goldenDirectoryTest else goldenFileTest
  return $ testFn testName run omitFilePaths goldenFile outputFile

runTest :: TestLoggingSettings
        -> FilePath
        -> FilePath
        -> String
        -> Backend
        -> Resources
        -> IO ()
runTest (logFile, debugLevel) inputFile outputFile modulePath backend Resources{..} = do
  run $ Options
    { version     = False
    , outFile     = Nothing
    , errFile     = Nothing
    , logFile     = logFile
    , debugLevel  = debugLevel
    , modeOptions = Compile $ CompileOptions
      { target            = backend
      , specificationFile = inputFile
      , outputFile        = Just outputFile
      , networkLocations  = networks
      , datasetLocations  = datasets
      , parameterValues   = parameters
      , modulePrefix      = Nothing
      , proofCache        = Just "proofcache.vclp"
      }
    }