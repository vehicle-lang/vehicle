module Test.Golden
  ( goldenTests
  ) where

import Control.Monad (void)
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

--------------------------------------------------------------------------------
-- Tests

goldenTests :: TestTree
goldenTests = testGroup "Golden"
  [ testGroup "Networks" (map makeGoldenTestsFromSpec realisticTestList)
  , testGroup "Simple"   (map makeGoldenTestsFromSpec simpleTestList)
  , testGroup "Misc"     (map makeGoldenTestsFromSpec miscTestList)
  ]

realisticTestList :: [GoldenTestSpec]
realisticTestList = map (addTestDirectory ("examples" </> "network")) [
  --("shortestPath",     [Verifier VNNLib]),
  ("andGate",                [Verifier VNNLib, ITP Agda]),
  ("acasXu" </> "property6", [Verifier VNNLib, ITP Agda]),
  ("monotonicity",           [Verifier VNNLib, ITP Agda]),
  ("increasing",             [Verifier VNNLib, ITP Agda]),
  ("reachability",           [Verifier VNNLib, ITP Agda])
  ]

simpleTestList :: [GoldenTestSpec]
simpleTestList = map (addTestDirectory ("examples" </> "simple"))
  [ --("quantifier",     [Verifier SMTLib])
    ("quantifierIn",   [Verifier SMTLib, ITP Agda])
  , ("let",            [Verifier SMTLib, ITP Agda])
  ]

miscTestList :: [GoldenTestSpec]
miscTestList = map (addTestDirectory ("examples" </> "misc"))
  [ --("dependent", [ITP (Vehicle Frontend)])
  ]

--------------------------------------------------------------------------------
-- Test infrastructure

type GoldenTestSpec = (FilePath, FilePath, [OutputTarget])

addTestDirectory :: FilePath -> (FilePath, [OutputTarget]) -> GoldenTestSpec
addTestDirectory folderPath (subfolder, targets) =
  ( folderPath </> subfolder
  , last (splitPath subfolder)
  , targets
  )

getFileExt :: OutputTarget -> String
getFileExt (Verifier VNNLib) = ".vnnlib"
getFileExt (Verifier SMTLib) = ".smtlib"
getFileExt (ITP Agda)        = ".agda"

makeGoldenTestsFromSpec :: GoldenTestSpec -> TestTree
makeGoldenTestsFromSpec (folderPath, testName, outputTargets) = testGroup testGroupName tests
  where
    testGroupName :: String
    testGroupName = takeFileName testName

    tests :: [TestTree]
    tests = map (makeIndividualTest folderPath testName) outputTargets

makeIndividualTest :: FilePath -> FilePath -> OutputTarget -> TestTree
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

runTest :: FilePath -> FilePath -> String -> OutputTarget -> IO ()
runTest inputFile outputFile modulePath outputTarget = do
  run $ Options
    { version       = False
    , logFile       = Nothing -- Just Nothing
    , commandOption = Compile $ CompileOptions
      { inputFile    = inputFile
      , outputFile   = Just outputFile
      , outputTarget = outputTarget
      , moduleName   = modulePath
      }
    }