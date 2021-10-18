{-# LANGUAGE ImportQualifiedPost #-}

module Test.Golden
  ( goldenTests
  ) where

import Control.Monad (void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Bifunctor (first)
import Test.Tasty
import Test.Tasty.Golden (goldenVsFile)
import System.Exit (exitFailure)
import System.FilePath (takeFileName, (<.>), (</>))
import Vehicle

data Result
  = Success
  | Failure

type GoldenTestSpec = (FilePath, [OutputTarget])

realisticTestList :: [GoldenTestSpec]
realisticTestList = map (first ("./examples/network" </>))
  [ ("shortestPath/shortestPath",
      [ Verifier VNNLib
        -- (Verifier SMTLib)
      ])
  , ("andGate/andGate",
      [ Verifier VNNLib
      ])
  , ("acasXu/property6",
      [ Verifier VNNLib
      ])
  , ("monotonicity",
      [ Verifier VNNLib
      ])
  , ("increasing",
      [ Verifier VNNLib
      ])
  ]

simpleTestList :: [GoldenTestSpec]
simpleTestList = map (first ("./examples/simple" </>))
  [ ("quantifier",
      [
        Verifier SMTLib
      ])

  , ("quantifierIn",
      [
        Verifier SMTLib
      ])

  ,  ("let",
      [
        Verifier SMTLib
      ])

  ,  ("bool",
      [
        Verifier SMTLib
      ])
  ]

miscTestList :: [GoldenTestSpec]
miscTestList = map (first ("./examples/misc" </>))
  [ ("dependent/dependent",
      [ ITP (Vehicle Frontend)
      ])
  ]

goldenTests :: TestTree
goldenTests = testGroup "Golden"
  [ testGroup "Realistic" (map makeGoldenTestsFromSpec realisticTestList)
  , testGroup "Simple"    (map makeGoldenTestsFromSpec simpleTestList)
  , testGroup "Misc"      (map makeGoldenTestsFromSpec miscTestList)
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

makeIndividualTest :: FilePath -> OutputTarget -> TestTree
makeIndividualTest baseFile target =
  let name       = show target in
  let extension  = getFileExt target in
  let inputFile  = baseFile <.> ".vcl" in
  let outputFile = baseFile <> "-output" <.> extension in
  let goldenFile = baseFile <.> extension in
  let action     = runTest inputFile outputFile target in
  goldenVsFile (show target) goldenFile inputFile action

runTest :: FilePath -> FilePath -> OutputTarget -> IO ()
runTest inputFile outputFile outputTarget = do
  run $ defaultOptions
    { inputFile    = Just inputFile
    , outputTarget = Just outputTarget
    , outputFile   = Just outputFile
    , logFile      = Just Nothing
    }