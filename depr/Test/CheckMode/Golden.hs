module Vehicle.Test.CheckMode.Golden
  ( tests
  ) where

import Control.Monad (when)
import Data.Bifunctor
import Data.Map qualified as Map (fromList)
import Data.Text (Text)
import System.Directory (doesFileExist, removeFile)
import System.FilePath ((<.>), (</>))
import Test.Tasty
import Vehicle
import Vehicle.Check
import Vehicle.Prelude
import Vehicle.Resource
import Vehicle.Test.Utils (baseTestDir, goldenFileTest, omitFilePaths)
import Vehicle.Verify.ProofCache
import Vehicle.Verify.Specification
import Vehicle.Verify.Specification.Status

tests :: TestTree
tests = testGroup "GoldenTests"
  [ successTest
  , networkChangedTest
  , networkMissingTest
  ]

testDir :: FilePath
testDir = baseTestDir </> "CheckMode" </> "Golden"

successTest :: TestTree
successTest = createTest "success" status alterNetwork
  where
  status = mkStatus [("network1", SAT Nothing)]
  alterNetwork = const $ return ()

networkChangedTest :: TestTree
networkChangedTest = createTest "networkChanged" status alterNetwork
  where
  status = mkStatus [("network1", SAT Nothing)]
  alterNetwork f = writeFile f "alteredContents"

networkMissingTest :: TestTree
networkMissingTest = createTest "networkMissing" status alterNetwork
  where
  status = mkStatus [("network1", SAT Nothing)]
  alterNetwork = removeFile

createTest :: String -> SpecificationStatus -> (FilePath -> IO ()) -> TestTree
createTest name status alterNetwork =
  goldenFileTest name run omitFilePaths goldenFile outputFile
  where
  run = runTest name status alterNetwork
  goldenFile   = testDir </> name <.> "txt"
  outputFile   = testDir </> name <> "-output.txt"

runTest :: String -> SpecificationStatus -> (FilePath -> IO ()) -> IO ()
runTest name status alterNetwork = do
  let outputFile   = testDir </> name <> "-output.txt"
  let proofCache   = testDir </> name <> "-proofFile"
  let networkFile  = testDir </> name <> "Network.onnx"

  writeFile networkFile "networkContents"
  networkHash <- hashResource Network networkFile
  let resources =
        [ ResourceSummary "myNetwork" networkFile networkHash Network
        ]

  writeProofCache proofCache $ ProofCache
    { proofCacheVersion  = vehicleVersion
    , status             = status
    , resourceSummaries  = resources
    , originalSpec       = ""
    , originalProperties = mempty
    }

  alterNetwork networkFile

  run $ Options
    { globalOptions = defaultGlobalOptions
      { outFile      = Just outputFile
      }
    , modeOptions  = Just $ Check $ CheckOptions
      { proofCache = proofCache
      }
    }

  removeFile proofCache
  removeFileIfExists networkFile

mkStatus :: [(Text, SatisfiabilityStatus)] -> SpecificationStatus
mkStatus xs = do
  let satToProperty s = SinglePropertyStatus (False, NonTrivial s)
  let xs' = fmap (second satToProperty) xs :: [(Text, PropertyStatus)]
  SpecificationStatus (Map.fromList xs')
