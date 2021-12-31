module Test.Golden
  ( GoldenTestSpec
  , goldenTests
  , goldenTestList
  ) where

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
import Control.Exception ( catch, throwIO )
import Debug.Trace (traceShowId)

import Vehicle
import Vehicle.Prelude
import Vehicle.Compile
import Vehicle.Backend
import Test.Golden.Utils

--------------------------------------------------------------------------------
-- Tests

goldenTests :: TestTree
goldenTests = testGroup "GoldenTests"
  [ testGroup "Networks" (map makeGoldenTestsFromSpec realisticTestList)
  , testGroup "Simple"   (map makeGoldenTestsFromSpec simpleTestList)
  , testGroup "Misc"     (map makeGoldenTestsFromSpec miscTestList)
  ]

goldenTestList :: [GoldenTestSpec]
goldenTestList = realisticTestList <> simpleTestList <> miscTestList

realisticTestList :: [GoldenTestSpec]
realisticTestList = map (addTestDirectory ("examples" </> "network")) [
  --("shortestPath",     [VNNLibBackend]),
  ("andGate",                [VNNLibBackend, AgdaBackend]),
  ("acasXu" </> "property6", [VNNLibBackend, AgdaBackend]),
  ("monotonicity",           [VNNLibBackend, AgdaBackend]),
  ("increasing",             [VNNLibBackend, AgdaBackend]),
  ("reachability",           [VNNLibBackend, AgdaBackend, MarabouBackend]),
  ("autoencoderError",       [VNNLibBackend, AgdaBackend]),
  ("windController",         [VNNLibBackend, AgdaBackend])
  ]

simpleTestList :: [GoldenTestSpec]
simpleTestList = map (addTestDirectory ("examples" </> "simple"))
  [ ("quantifierIn",   [AgdaBackend])
  , ("let",            [AgdaBackend])
  ]

miscTestList :: [GoldenTestSpec]
miscTestList = map (addTestDirectory ("examples" </> "misc"))
  [ --("dependent", [ITP (Vehicle Frontend)])
  ]

--------------------------------------------------------------------------------
-- Test infrastructure

type GoldenTestSpec = (FilePath, FilePath, [Backend])

addTestDirectory :: FilePath -> (FilePath, [Backend]) -> GoldenTestSpec
addTestDirectory folderPath (subfolder, targets) =
  ( folderPath </> subfolder
  , last (splitPath subfolder)
  , targets
  )

getGoldenFilepathSuffix :: Backend -> String
getGoldenFilepathSuffix (Verifier Marabou) = "-marabou"
getGoldenFilepathSuffix (Verifier VNNLib)  = ".vnnlib"
getGoldenFilepathSuffix (ITP Agda)         = ".agda"

makeGoldenTestsFromSpec :: GoldenTestSpec -> TestTree
makeGoldenTestsFromSpec (folderPath, testName, outputTargets) = testGroup testGroupName tests
  where
    testGroupName :: String
    testGroupName = takeFileName testName

    tests :: [TestTree]
    tests = map (makeIndividualTest folderPath testName) outputTargets

makeIndividualTest :: FilePath -> FilePath -> Backend -> TestTree
makeIndividualTest folderPath name backend = test
  where
  testName       = name <> "-" <> show backend
  filePathSuffix = getGoldenFilepathSuffix backend
  basePath       = folderPath </> name
  moduleName     = name <> "-output"
  inputFile      = basePath <.> ".vcl"
  isFolderOutput = backend == MarabouBackend
  outputFile     = basePath <> "-temp-output" <> filePathSuffix
  goldenFile     = basePath <> "-output"      <> filePathSuffix
  run            = runTest inputFile outputFile moduleName backend

  test = if backend == MarabouBackend
    then goldenDirectoryTest testName run goldenFile outputFile
    else goldenFileTest      testName run goldenFile outputFile

runTest :: FilePath -> FilePath -> String -> Backend -> IO ()
runTest inputFile outputFile modulePath backend = do
  run $ Options
    { version       = False
    , logFile       = Nothing --  Just Nothing
    , commandOption = Compile $ CompileOptions
      { inputFile    = inputFile
      , outputFile   = Just outputFile
      , outputTarget = backend
      , moduleName   = modulePath
      }
    }