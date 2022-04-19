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

errorTests :: TestTree
errorTests = testGroup "ErrorTests"
  [ argumentErrors
  , typeCheckingErrors
  , networkErrors
  , datasetErrors
  , parameterErrors
  ]

argumentErrors :: TestTree
argumentErrors = failTestGroup "ArgumentErrors"
  [ testSpec
    { testName     = "missingInputFile"
    , testLocation = Tests
    , testTargets  = [VNNLibBackend]
    }
  ]

typeCheckingErrors :: TestTree
typeCheckingErrors = failTestGroup "TypingErrors"
  [ testSpec
    { testName     = "intAsNat"
    , testLocation = Tests
    , testTargets  = [VNNLibBackend]
    }

  , testSpec
    { testName     = "indexOutOfBounds"
    , testLocation = Tests
    , testTargets  = [VNNLibBackend]
    }

  , testSpec
    { testName     = "indexOutOfBoundsUnknown"
    , testLocation = Tests
    , testTargets  = [VNNLibBackend]
    }

  , testSpec
    { testName     = "unsolvedMeta"
    , testLocation = Tests
    , testTargets  = [VNNLibBackend]
    }
  ]

networkErrors :: TestTree
networkErrors = failTestGroup "NetworkErrors"
  [ testSpec
    { testName     = "notAFunction"
    , testLocation = Tests
    , testTargets  = [VNNLibBackend]
    }

  , testSpec
    { testName     = "multidimInputTensor"
    , testLocation = Tests
    , testTargets  = [VNNLibBackend]
    }
  ]

datasetErrors :: TestTree
datasetErrors = failTestGroup "DatasetErrors"
  [ testSpec
    { testName     = "notProvided"
    , testLocation = Tests
    , testTargets  = [VNNLibBackend]
    , testDatasets = []
    }

  , testSpec
    { testName     = "missing"
    , testLocation = Tests
    , testTargets  = [VNNLibBackend]
    , testDatasets = [("trainingDataset", "non-existent.idx")]
    }

  , testSpec
    { testName     = "unsupportedFormat"
    , testLocation = Tests
    , testTargets  = [VNNLibBackend]
    , testDatasets = [("trainingDataset", "non-existent.fgt")]
    }

  , testSpec
    { testName     = "invalidContainerType"
    , testLocation = Tests
    , testTargets  = [VNNLibBackend]
    , testDatasets = [("trainingDataset", "dataset-nat-4.idx")]
    }

  , testSpec
    { testName     = "invalidElementType"
    , testLocation = Tests
    , testTargets  = [VNNLibBackend]
    , testDatasets = [("trainingDataset", "dataset-nat-4.idx")]
    }

  -- , testSpec
  --   { testName     = "variableDimensions"
  --   , testLocation = Tests
  --   , testTargets  = [VNNLibBackend]
  --   , testDatasets = [("trainingDataset", "dataset-nat-4.idx")]
  --   }

  -- , testSpec
  --   { testName     = "mismatchedDimensions"
  --   , testLocation = Tests
  --   , testTargets  = [VNNLibBackend]
  --   , testDatasets = [("trainingDataset", "dataset-nat-4.idx")]
  --   }

  , testSpec
    { testName     = "mismatchedType"
    , testLocation = Tests
    , testTargets  = [VNNLibBackend]
    , testDatasets = [("trainingDataset", "dataset-rat-4.idx")]
    }

  , testSpec
    { testName     = "tooBigFin"
    , testLocation = Tests
    , testTargets  = [VNNLibBackend]
    , testDatasets = [("trainingDataset", "dataset-nat-4.idx")]
    }

  , testSpec
    { testName     = "negativeNat"
    , testLocation = Tests
    , testTargets  = [VNNLibBackend]
    , testDatasets = [("trainingDataset", "dataset-int-4.idx")]
    }
  ]

parameterErrors :: TestTree
parameterErrors = failTestGroup "ParameterErrors"
  [ testSpec
    { testName       = "notProvided"
    , testLocation   = Tests
    , testTargets    = [VNNLibBackend]
    , testParameters = []
    }

  , testSpec
    { testName       = "unparseable"
    , testLocation   = Tests
    , testTargets    = [VNNLibBackend]
    , testParameters = [("n", "~`")]
    }
  ]

--------------------------------------------------------------------------------
-- Test infrastructure

testDir :: FilePath
testDir = "test" </> "Test" </> "Compile" </> "Error"

failTestGroup :: FilePath
              -> [TestSpec]
              -> TestTree
failTestGroup folder tests = testGroup folder (tests <&> \spec@TestSpec{..} ->
  let resources = testResources spec in
  failTest (folder </> testName) (head testTargets) resources)

failTest :: FilePath -> Backend -> Resources -> TestTree
failTest filepath backend resources = test
  where
  testName       = takeBaseName filepath
  basePath       = testDir </> filepath
  inputFile      = basePath <.> ".vcl"
  logFile        = basePath <> "-temp" <.> "txt"
  goldenFile     = basePath <.> "txt"
  run            = runTest inputFile logFile backend resources

  test = goldenFileTest testName run omitFilePaths goldenFile logFile

runTest :: FilePath -> FilePath -> Backend -> Resources -> IO ()
runTest inputFile outputFile backend Resources{..} = do
  run options `catch` handleExitCode
  where
  options = Options
    { version     = False
    , outFile     = Nothing
    , errFile     = Just outputFile
    , logFile     = Nothing -- Just Nothing
    , debugLevel  = 1
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