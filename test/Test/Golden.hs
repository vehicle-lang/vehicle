module Test.Golden
  ( GoldenTestSpec
  , goldenTests
  , goldenTestList
  ) where

import Data.Algorithm.Diff (Diff, PolyDiff(..), getGroupedDiff)
import Data.Algorithm.DiffOutput (ppDiff)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Bifunctor (first)
import Test.Tasty
import Test.Tasty.Golden.Advanced (goldenTest)
import System.Exit (exitFailure)
import System.FilePath (takeFileName, splitPath, (<.>), (</>))
import System.Directory (removeFile)
import System.IO.Error (isDoesNotExistError)
import Control.Exception ( catch, throwIO )
import Debug.Trace (traceShowId)

import Vehicle
import Vehicle.Prelude
import Vehicle.Compile
import Vehicle.Backend

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
  ("reachability",           [VNNLibBackend, AgdaBackend]),
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

getFileExt :: Backend -> String
getFileExt MarabouBackend = ".txt"
getFileExt VNNLibBackend  = ".vnnlib"
getFileExt AgdaBackend         = ".agda"

makeGoldenTestsFromSpec :: GoldenTestSpec -> TestTree
makeGoldenTestsFromSpec (folderPath, testName, outputTargets) = testGroup testGroupName tests
  where
    testGroupName :: String
    testGroupName = takeFileName testName

    tests :: [TestTree]
    tests = map (makeIndividualTest folderPath testName) outputTargets

makeIndividualTest :: FilePath -> FilePath -> Backend -> TestTree
makeIndividualTest folderPath name target = testWithCleanup
  where
  testName   = name <> "-" <> show target
  extension  = getFileExt target
  basePath   = folderPath </> name
  moduleName = name <> "-output"
  inputFile  = basePath <.> ".vcl"
  outputFile = basePath <> "-temp-output" <.> extension
  goldenFile = basePath <> "-output" <.> extension
  readGolden = T.readFile goldenFile
  readOutput = do runTest inputFile outputFile moduleName target; T.readFile outputFile
  updateGolden = T.writeFile goldenFile

  test = goldenTest testName readGolden readOutput diffCommand updateGolden
  testWithCleanup = cleanupOutputFile outputFile test

diffCommand :: Text -> Text -> IO (Maybe String)
diffCommand golden output = do
  let goldenLines = map T.unpack $ T.lines golden
  let outputLines = map T.unpack $ T.lines output
  let diff = getGroupedDiff goldenLines outputLines
  return $ if all isBoth diff
    then Nothing
    else Just $ "Output differs:" <> ppDiff diff
  where
    isBoth :: Diff a -> Bool
    isBoth (Both a b) = True
    isBoth _ = False

cleanupOutputFile :: FilePath -> TestTree -> TestTree
cleanupOutputFile testFile test = withResource (return ()) (const cleanup) (const test)
  where
    cleanup = removeFile testFile `catch` handleExists

    handleExists e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e

runTest :: FilePath -> FilePath -> String -> Backend -> IO ()
runTest inputFile outputFile modulePath outputTarget = do
  run $ Options
    { version       = False
    , logFile       = Nothing --Just Nothing
    , commandOption = Compile $ CompileOptions
      { inputFile    = inputFile
      , outputFile   = Just outputFile
      , outputTarget = outputTarget
      , moduleName   = modulePath
      }
    }