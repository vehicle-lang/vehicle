module Test.Golden
  ( goldenTests
  ) where

import Control.Monad (void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Bifunctor (first)
import Test.Tasty
import Test.Tasty.Golden (goldenVsFileDiff)
import System.Exit (exitFailure)
import System.FilePath (takeFileName, splitPath, (<.>), (</>))
import System.Directory (removeFile)
import System.IO.Error (isDoesNotExistError)
import Control.Exception ( catch, throwIO )

import Vehicle

--------------------------------------------------------------------------------
-- Tests

goldenTests :: TestTree
goldenTests = testGroup "Golden"
  [ testGroup "Networks" (map makeGoldenTestsFromSpec realisticTestList)
  , testGroup "Simple"   (map makeGoldenTestsFromSpec simpleTestList)
  , testGroup "Misc"     (map makeGoldenTestsFromSpec miscTestList)
  ]

realisticTestList :: [GoldenTestSpec]
realisticTestList = map (addTestDirectory "./examples/network") [
  --("shortestPath",     [Verifier VNNLib]),
  ("andGate",          [Verifier VNNLib]),
  ("acasXu/property6", [Verifier VNNLib]),
  ("monotonicity",     [Verifier VNNLib])
  -- ("increasing",       [Verifier VNNLib, ITP Agda])
  ]

simpleTestList :: [GoldenTestSpec]
simpleTestList = map (addTestDirectory "./examples/simple")
  [ --("quantifier",     [Verifier SMTLib])
    ("quantifierIn",   [Verifier SMTLib, ITP Agda])
  , ("let",            [Verifier SMTLib, ITP Agda])
  ]

miscTestList :: [GoldenTestSpec]
miscTestList = map (addTestDirectory "./examples/misc")
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
getFileExt (ITP (Vehicle _)) = error "Vehicle targets not yet supported"

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
  inputFile  = basePath <.> ".vcl"
  outputFile = basePath <> "-temp-output" <.> extension
  goldenFile = basePath <> "-output" <.> extension
  action     = runTest inputFile outputFile target

  test = goldenVsFileDiff testName diffCommand goldenFile outputFile action
  testWithCleanup = cleanupTestFile outputFile test

diffCommand :: FilePath -> FilePath -> [String]
diffCommand ref new = ["diff", "--color=always", ref, new]

cleanupTestFile :: FilePath -> TestTree -> TestTree
cleanupTestFile testFile test = withResource (return ()) (const cleanup) (const test)
  where
    cleanup = removeFile testFile `catch` handleExists

    handleExists e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e

runTest :: FilePath -> FilePath -> OutputTarget -> IO ()
runTest inputFile outputFile outputTarget = do
  run $ defaultOptions
    { inputFile    = Just inputFile
    , outputTarget = Just outputTarget
    , outputFile   = Just outputFile
    , logFile      = Nothing -- Just Nothing -- Nothing
    }