{-# LANGUAGE ImportQualifiedPost #-}

module Test.Golden
  ( goldenTests
  ) where

import Control.Monad (void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Test.Tasty
import Test.Tasty.Golden (goldenVsFile)
import System.Exit (exitFailure)
import System.FilePath (takeFileName, (<.>))
import Vehicle

data Result
  = Success
  | Failure

type GoldenTestSpec = (FilePath, [(OutputTarget, Result)])

realisticTestList :: [GoldenTestSpec]
realisticTestList =
  [ ("./examples/realistic/shortestPath/shortestPath",
      [
        (Verifier VNNLib, Success)
        -- (Verifier SMTLib, Failure)
      ])
{-
  , ("examples/mnist",
      [ (Verifier VNNLib, Success)
      ])
-}
  ]

simpleTestList :: [GoldenTestSpec]
simpleTestList =
  [ ("./examples/simple/quantifier",
      [
        (Verifier VNNLib, Success)
      ])
  ]

--fileTests :: [FilePath]
--fileTests = fmap ("examples/" <>)
  --[ "simple/quantifier.vcl"
  -- "simple/quantifierIn.vcl"
  -- "simple/int.vcl"
  -- "simple/real.vcl"
  -- , "mnist.vcl"
  -- , "andGate.vcl"
  -- , "mnist.vcl"
  -- , "shortestPath.vcl"
  -- "AcasXu/property6.vcl"
  --"simple.vcl"
  --]

goldenTests :: TestTree
goldenTests = testGroup "Golden"
  [ testGroup "Realistic" (map makeGoldenTestsFromSpec realisticTestList)
  , testGroup "Simple"    (map makeGoldenTestsFromSpec simpleTestList)
  ]

getFileExt :: OutputTarget -> String
getFileExt (Verifier VNNLib) = ".vnnlib"
getFileExt (Verifier SMTLib) = ".smtlib"
getFileExt (ITP Agda)        = ".agda"

makeGoldenTestsFromSpec :: GoldenTestSpec -> TestTree
makeGoldenTestsFromSpec (name, outputTargets) = testGroup testGroupName tests
  where
    testGroupName :: String
    testGroupName = takeFileName name

    tests :: [TestTree]
    tests = map (makeIndividualTest name) outputTargets

makeIndividualTest :: FilePath -> (OutputTarget, Result) -> TestTree
makeIndividualTest baseFile (target, result) =
  let name       = show target in
  let extension  = getFileExt target in
  let inputFile  = baseFile <.> ".vcl" in
  let outputFile = baseFile <> "-output" <.> extension in
  let goldenFile = baseFile <.> extension in
  let action     = runTest inputFile outputFile target result in
  goldenVsFile (show target) goldenFile inputFile action

runTest :: FilePath -> FilePath -> OutputTarget -> Result -> IO ()
runTest inputFile outputFile outputTarget result = do
  run $ defaultOptions
    { inputFile    = Just inputFile
    , outputTarget = Just outputTarget
    , outputFile   = Just outputFile
    }