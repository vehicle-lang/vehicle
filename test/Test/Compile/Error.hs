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

--------------------------------------------------------------------------------
-- Tests

errorTests :: TestTree
errorTests = testGroup "ErrorTests"
  [ argumentErrors
  , typeCheckingErrors
  , networkErrors
  , datasetErrors
  ]

argumentErrors :: TestTree
argumentErrors = failTestGroup "ArgumentErrors"
  [ ("missingInputFile",        Nothing, [])
  ]

typeCheckingErrors :: TestTree
typeCheckingErrors = failTestGroup "TypingErrors"
  [ ("intAsNat",                Nothing, [])
  , ("indexOutOfBounds",        Nothing, [])
  , ("indexOutOfBoundsUnknown", Nothing, [])
  ]

networkErrors :: TestTree
networkErrors = failTestGroup "NetworkErrors"
  [ ("notAFunction",        Nothing, [])
  , ("multidimInputTensor", Nothing, [])
  ]

datasetErrors :: TestTree
datasetErrors = failTestGroup "DatasetErrors"
  [ ("notProvided",          Nothing, [])
  , ("missing",              Nothing, [("trainingDataset", "non-existent.idx")])
  , ("unsupportedFormat",    Nothing, [("trainingDataset", "non-existent.fgt")])
  , ("invalidContainerType", Nothing, [("trainingDataset", "dataset-nat-4.idx")])
  , ("invalidElementType",   Nothing, [("trainingDataset", "dataset-nat-4.idx")])
  , ("variableDimensions",   Nothing, [("trainingDataset", "dataset-nat-4.idx")])
  , ("mismatchedDimensions", Nothing, [("trainingDataset", "dataset-nat-4.idx")])
  , ("mismatchedType",       Nothing, [("trainingDataset", "dataset-rat-4.idx")])
  , ("tooBigFin",            Nothing, [("trainingDataset", "dataset-nat-4.idx")])
  , ("negativeNat",          Nothing, [("trainingDataset", "dataset-int-4.idx")])
  ]

--------------------------------------------------------------------------------
-- Test infrastructure

testDir :: FilePath
testDir = "test" </> "Test" </> "Compile" </> "Error"

failTestGroup :: FilePath
              -> [(FilePath, Maybe Backend, [(Text, FilePath)])]
              -> TestTree
failTestGroup folder tests = testGroup folder (tests <&>
  \(name, maybeBackend, datasetList) ->
    let datasets = fmap ((testDir </> folder) </>) (Map.fromList datasetList) in
    let backend = fromMaybe VNNLibBackend maybeBackend in
    failTest (folder </> name) backend datasets)

failTest :: FilePath -> Backend -> Map Text FilePath -> TestTree
failTest filepath backend datasets = test
  where
  testName       = takeBaseName filepath
  basePath       = testDir </> filepath
  inputFile      = basePath <.> ".vcl"
  logFile        = basePath <> "-temp" <.> "txt"
  goldenFile     = basePath <.> "txt"
  run            = runTest inputFile logFile backend datasets

  test = goldenFileTest testName run omitFilePaths goldenFile logFile

runTest :: FilePath -> FilePath -> Backend -> Map Text FilePath -> IO ()
runTest inputFile outputFile backend datasets = do
  run options `catch` handleExitCode
  where
    options = Options
      { version       = False
      , outFile       = Nothing
      , errFile       = Just outputFile
      , logFile       = Nothing -- Just Nothing
      , commandOption = Compile $ CompileOptions
        { target       = backend
        , inputFile    = inputFile
        , outputFile   = Nothing
        , networks     = mempty
        , datasets     = datasets
        , modulePrefix = Nothing
        , proofCache   = Nothing
        }
      }

handleExitCode :: ExitCode -> IO ()
handleExitCode e = return ()