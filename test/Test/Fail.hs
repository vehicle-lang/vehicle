module Test.Fail
  ( failTests
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Bifunctor (first)
import System.Exit (exitFailure)
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
import Test.Golden.Utils

--------------------------------------------------------------------------------
-- Tests

failTests :: TestTree
failTests = testGroup "FailTests"
  [ networkFailTests
  , typeCheckingFailTests
  ]

networkFailTests :: TestTree
networkFailTests = failTestGroup "NetworkTypeErrors"
  [ ("notAFunction", Nothing)
  , ("multidimInputTensor", Nothing)
  ]

typeCheckingFailTests :: TestTree
typeCheckingFailTests = failTestGroup "TypingErrors"
  [ ("intAsNat", Nothing)
  ]

failTestGroup :: FilePath -> [(FilePath, Maybe Backend)] -> TestTree
failTestGroup folder tests = testGroup folder (fmap
  (\(name, backend) -> failTest (folder </> name) backend) tests)

--------------------------------------------------------------------------------
-- Test infrastructure

failTest :: FilePath -> Maybe Backend -> TestTree
failTest filepath backend = test
  where
  testName       = takeBaseName filepath <> maybe "" (\x -> "-" <> show x) backend
  basePath       = "test" </> "Test" </> "Fail" </> filepath
  inputFile      = basePath <.> ".vcl"
  logFile        = basePath <> "-temp" <.> "txt"
  goldenFile     = basePath <.> "txt"
  run            = runTest inputFile logFile (fromMaybe VNNLibBackend backend)

  test = goldenFileTest testName run goldenFile logFile

runTest :: FilePath -> FilePath -> Backend -> IO ()
runTest inputFile outputFile backend = do
  run options `catch` handleExitCode
  where
    options = Options
      { version       = False
      , logFile       = Nothing --Just Nothing
      , errFile       = Just outputFile
      , commandOption = Compile $ CompileOptions
        { inputFile    = inputFile
        , outputFile   = Nothing
        , outputTarget = backend
        , moduleName   = "N/A"
        , networks     = []
        }
      }

handleExitCode :: SomeException -> IO ()
handleExitCode e = return ()