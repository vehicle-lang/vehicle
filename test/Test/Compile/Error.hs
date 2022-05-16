module Test.Compile.Error
  ( errorTests
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Data.Map ( Map )
import Data.Map qualified as Map
import System.Exit (exitFailure, ExitCode)
import System.FilePath (takeFileName, splitPath, (<.>), (</>), takeBaseName)
import System.Directory (removeFile, removeDirectory)
import System.IO.Error (isDoesNotExistError)
import System.IO (stderr)
import Control.Exception ( catch, throwIO, SomeException, Exception )
import Data.Maybe (fromMaybe)
import Control.Monad.RWS.Lazy (when)
import Test.Tasty
import Test.Tasty.Golden.Advanced (goldenTest)

import Vehicle
import Vehicle.Prelude
import Vehicle.Compile
import Vehicle.Backend.Prelude

import Test.GoldenUtils
import Test.Compile.Utils

--------------------------------------------------------------------------------
-- Tests

errorTests :: MonadTest m => m TestTree
errorTests = testGroup "ErrorTests" <$> sequence
  [ argumentErrors
  , typeCheckingErrors
  , networkErrors
  , datasetErrors
  , parameterErrors
  ]

argumentErrors :: MonadTest m => m TestTree
argumentErrors = failTestGroup "ArgumentErrors"
  [ testSpec
    { testName     = "missingInputFile"
    , testLocation = Tests
    , testTargets  = [MarabouBackend]
    }
  ]

typeCheckingErrors :: MonadTest m => m TestTree
typeCheckingErrors = failTestGroup "TypingErrors"
  [ testSpec
    { testName     = "intAsNat"
    , testLocation = Tests
    , testTargets  = [MarabouBackend]
    }

  , testSpec
    { testName     = "indexOutOfBounds"
    , testLocation = Tests
    , testTargets  = [MarabouBackend]
    }

  , testSpec
    { testName     = "indexOutOfBoundsUnknown"
    , testLocation = Tests
    , testTargets  = [MarabouBackend]
    }

  , testSpec
    { testName     = "incorrectTensorLength"
    , testLocation = Tests
    , testTargets  = [MarabouBackend]
    }

  , testSpec
    { testName     = "unsolvedMeta"
    , testLocation = Tests
    , testTargets  = [MarabouBackend]
    }
  ]

networkErrors :: MonadTest m => m TestTree
networkErrors = failTestGroup "NetworkErrors"
  [ testSpec
    { testName     = "notAFunction"
    , testLocation = Tests
    , testTargets  = [MarabouBackend]
    }

  , testSpec
    { testName     = "multidimInputTensor"
    , testLocation = Tests
    , testTargets  = [MarabouBackend]
    }
  ]

datasetErrors :: MonadTest m => m TestTree
datasetErrors = failTestGroup "DatasetErrors"
  [ testSpec
    { testName     = "notProvided"
    , testLocation = Tests
    , testTargets  = [MarabouBackend]
    , testDatasets = []
    }

  , testSpec
    { testName     = "missing"
    , testLocation = Tests
    , testTargets  = [MarabouBackend]
    , testDatasets = [("trainingDataset", "non-existent.idx")]
    }

  , testSpec
    { testName     = "unsupportedFormat"
    , testLocation = Tests
    , testTargets  = [MarabouBackend]
    , testDatasets = [("trainingDataset", "non-existent.fgt")]
    }

  , testSpec
    { testName     = "invalidContainerType"
    , testLocation = Tests
    , testTargets  = [MarabouBackend]
    , testDatasets = [("trainingDataset", "dataset-nat-4.idx")]
    }

  , testSpec
    { testName     = "invalidElementType"
    , testLocation = Tests
    , testTargets  = [MarabouBackend]
    , testDatasets = [("trainingDataset", "dataset-nat-4.idx")]
    }

  , testSpec
    { testName     = "variableDimensions"
    , testLocation = Tests
    , testTargets  = [MarabouBackend]
    , testDatasets = [("trainingDataset", "dataset-nat-4.idx")]
    }

  , testSpec
    { testName     = "mismatchedDimensions"
    , testLocation = Tests
    , testTargets  = [MarabouBackend]
    , testDatasets = [("trainingDataset", "dataset-nat-4.idx")]
    }

  , testSpec
    { testName     = "mismatchedType"
    , testLocation = Tests
    , testTargets  = [MarabouBackend]
    , testDatasets = [("trainingDataset", "dataset-rat-4.idx")]
    }

  , testSpec
    { testName     = "tooBigIndex"
    , testLocation = Tests
    , testTargets  = [MarabouBackend]
    , testDatasets = [("trainingDataset", "dataset-nat-4.idx")]
    }

  , testSpec
    { testName     = "negativeNat"
    , testLocation = Tests
    , testTargets  = [MarabouBackend]
    , testDatasets = [("trainingDataset", "dataset-int-4.idx")]
    }
  ]

parameterErrors :: MonadTest m => m TestTree
parameterErrors = failTestGroup "ParameterErrors"
  [ testSpec
    { testName       = "notProvided"
    , testLocation   = Tests
    , testTargets    = [MarabouBackend]
    , testParameters = []
    }

  , testSpec
    { testName       = "unparseable"
    , testLocation   = Tests
    , testTargets    = [MarabouBackend]
    , testParameters = [("n", "~`")]
    }
  ]

--------------------------------------------------------------------------------
-- Test infrastructure

testDir :: FilePath
testDir = "test" </> "Test" </> "Compile" </> "Error"

failTestGroup :: MonadTest m
              => FilePath
              -> [TestSpec]
              -> m TestTree
failTestGroup folder tests = testGroup folder <$> traverse mkTest tests
  where
  mkTest spec@TestSpec{..} = do
    let resources = testResources spec
    failTest (folder </> testName) (head testTargets) resources

failTest :: MonadTest m => FilePath -> Backend -> Resources -> m TestTree
failTest filepath backend resources = do
  loggingSettings <- getTestLoggingSettings

  let testName       = takeBaseName filepath
  let basePath       = testDir </> filepath
  let inputFile      = basePath <.> ".vcl"
  let logFile        = basePath <> "-temp" <.> "txt"
  let goldenFile     = basePath <.> "txt"
  let run            = runTest loggingSettings inputFile logFile backend resources

  return $ goldenFileTest testName run omitFilePaths goldenFile logFile

runTest :: TestLoggingSettings
        -> FilePath
        -> FilePath
        -> Backend
        -> Resources
        -> IO ()
runTest (logFile, debugLevel) inputFile outputFile backend Resources{..} = do
  run options `catch` handleExitCode
  where
  options = Options
    { version     = False
    , outFile     = Nothing
    , errFile     = Just outputFile
    , logFile     = logFile
    , debugLevel  = debugLevel
    , modeOptions = Compile $ CompileOptions
      { target            = backend
      , specificationFile = inputFile
      , outputFile        = Nothing
      , networkLocations  = networks
      , datasetLocations  = datasets
      , parameterValues   = parameters
      , modulePrefix      = Nothing
      , proofCache        = Nothing
      }
    }

handleExitCode :: ExitCode -> IO ()
handleExitCode e = return ()