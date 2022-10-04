module Vehicle.Test.CompileMode.Error
  ( functionalityTests
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

import Vehicle.Test.Utils

--------------------------------------------------------------------------------
-- Tests

functionalityTests :: MonadTest m => m TestTree
functionalityTests = testGroup "ErrorTests" <$> sequence
  [ argumentErrors
  , parsingErrors
  , typeCheckingErrors
  , networkErrors
  , datasetErrors
  , parameterErrors
  , linearityErrors
  , polarityErrors
  , otherVerifierErrors
  ]

argumentErrors :: MonadTest m => m TestTree
argumentErrors = failTestGroup "ArgumentErrors" TypeCheck
  [ testSpec { testName = "missingInputFile" }
  ]

parsingErrors :: MonadTest m => m TestTree
parsingErrors = failTestGroup "ParsingErrors" TypeCheck
  [ testSpec { testName = "functionNotGivenBody"}
  , testSpec { testName = "propertyNotGivenBody"}
  , testSpec { testName = "resourceGivenBody"}
  , testSpec { testName = "annotationWithNoDeclaration"}
  , testSpec { testName = "invalidAnnotationParameter"}
  , testSpec { testName = "invalidAnnotationParameterValue"}
  , testSpec { testName = "functionWithMismatchedNames"}
  , testSpec { testName = "missingVariablesLambda"}
  , testSpec { testName = "unchainableOrdersEqualsEquals"}
  ]

typeCheckingErrors :: MonadTest m => m TestTree
typeCheckingErrors = failTestGroup "TypingErrors" TypeCheck
  [ testSpec { testName = "intAsNat"}
  , testSpec { testName = "indexOutOfBoundsConcrete"}
  , testSpec { testName = "indexOutOfBoundsUnknown"}
  , testSpec { testName = "incorrectTensorLength"}
  , testSpec { testName = "unsolvedMeta"}
  ]

networkErrors :: MonadTest m => m TestTree
networkErrors = failTestGroup "NetworkErrors" MarabouBackend
  [ testSpec
    { testName     = "notAFunction"
    }
  ]

datasetErrors :: MonadTest m => m TestTree
datasetErrors = failTestGroup "DatasetErrors" MarabouBackend
  [ testSpec
    { testName     = "notProvided"
    }

  , testSpec
    { testName     = "missingDataset"
    , testDatasets = [("trainingDataset", "non-existent.idx")]
    }

  , testSpec
    { testName     = "unsupportedFormat"
    , testDatasets = [("trainingDataset", "non-existent.fgt")]
    }

  , testSpec
    { testName     = "invalidContainerType"
    , testDatasets = [("trainingDataset", "dataset-nat-4.idx")]
    }

  , testSpec
    { testName     = "invalidElementType"
    , testDatasets = [("trainingDataset", "dataset-nat-4.idx")]
    }

  , testSpec
    { testName     = "variableDimensions"
    , testDatasets = [("trainingDataset", "dataset-nat-4.idx")]
    }

  , testSpec
    { testName     = "mismatchedDimensions"
    , testDatasets = [("trainingDataset", "dataset-nat-4.idx")]
    }

  , testSpec
    { testName     = "mismatchedDimensionSize"
    , testDatasets = [("trainingDataset", "dataset-nat-4.idx")]
    }

  , testSpec
    { testName     = "mismatchedType"
    , testDatasets = [("trainingDataset", "dataset-rat-4.idx")]
    }

  , testSpec
    { testName     = "tooBigIndex"
    , testDatasets = [("trainingDataset", "dataset-nat-4.idx")]
    }

  , testSpec
    { testName     = "negativeNat"
    , testDatasets = [("trainingDataset", "dataset-int-4.idx")]
    }
  ]

parameterErrors :: MonadTest m => m TestTree
parameterErrors = failTestGroup "ParameterErrors" MarabouBackend
  [ testSpec
    { testName       = "notProvided"
    }

  , testSpec
    { testName       = "unsupportedType"
    }

  , testSpec
    { testName       = "unparseableBool"
    , testParameters = [("b", "x")]
    }

  , testSpec
    { testName       = "unparseableIndex"
    , testParameters = [("n", "~`")]
    }

  , testSpec
    { testName       = "invalidIndex"
    , testParameters = [("n", "5")]
    }

  , testSpec
    { testName       = "invalidNat"
    , testParameters = [("n", "-5")]
    }

  , testSpec
    { testName       = "unparseableNat"
    , testParameters = [("n", "~`")]
    }

  , testSpec
    { testName       = "unparseableRat"
    , testParameters = [("r", "~`")]
    }
  ]

propertyErrors :: MonadTest m => m TestTree
propertyErrors = failTestGroup "PropertyErrors" TypeCheck
  [ testSpec { testName = "invalidType"}
  ]

polarityErrors :: MonadTest m => m TestTree
polarityErrors = failTestGroup "PolarityErrors" MarabouBackend
  [ testSpec { testName = "alternating"}
  , testSpec { testName = "alternatingNeg"}
  , testSpec { testName = "alternatingNegNeg"}
  , testSpec { testName = "alternatingImplies"}
  , testSpec { testName = "alternatingFun"}
  , testSpec { testName = "alternatingIndirectNegFun"}
  , testSpec { testName = "quantifiedIfCondition"}
  ]

linearityErrors :: MonadTest m => m TestTree
linearityErrors = failTestGroup "LinearityErrors" MarabouBackend
  [ testSpec { testName = "quadraticInput" }
  , testSpec { testName = "quadraticFunInput"}
  , testSpec { testName = "quadraticFunOutput"}
  , testSpec { testName = "quadraticInputOutput"}
  , testSpec { testName = "quadraticTensorInputLookup"}
  , testSpec { testName = "nonLinearIfCondition"}
  ]

otherVerifierErrors :: MonadTest m => m TestTree
otherVerifierErrors = failTestGroup "OtherVerifierErrors" MarabouBackend
  [ testSpec { testName = "quantifiedNat" }
  , testSpec { testName = "quantifiedInt" }
  , testSpec { testName = "quantifiedVectorNat" }
  ]

--------------------------------------------------------------------------------
-- Test infrastructure

testDir :: FilePath
testDir = baseTestDir </> "CompileMode" </> "Error"

failTestGroup :: MonadTest m
              => FilePath
              -> Backend
              -> [TestSpec]
              -> m TestTree
failTestGroup folder testTarget tests = testGroup folder <$> traverse mkTest tests
  where
  mkTest spec@TestSpec{..} = do
    let fullSpec = TestSpec{testLocation = Tests, testTargets = [testTarget], ..}
    let resources = testResources fullSpec
    failTest (folder </> testName) (head [testTarget]) resources

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
      { target                = backend
      , specification         = inputFile
      , declarationsToCompile = mempty
      , outputFile            = Nothing
      , networkLocations      = networks
      , datasetLocations      = datasets
      , parameterValues       = parameters
      , moduleName            = Nothing
      , proofCache            = Nothing
      }
    }

handleExitCode :: ExitCode -> IO ()
handleExitCode e = return ()