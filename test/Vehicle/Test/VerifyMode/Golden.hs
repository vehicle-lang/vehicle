module Vehicle.Test.VerifyMode.Golden where

import Control.Monad (when)
import Data.Map qualified as Map (fromList)
import Data.Text (Text)
import System.Directory (doesFileExist, findExecutable, removeFile)
import System.FilePath ((<.>), (</>))
import System.Info (os)

import Test.Tasty

import Vehicle
import Vehicle.Backend.Prelude
import Vehicle.Prelude
import Vehicle.Resource
import Vehicle.Verify
import Vehicle.Verify.Specification.Status
import Vehicle.Verify.Verifier.Interface

import Vehicle.Test.Utils
import Vehicle.Test.Utils.Golden (goldenFileTest, omitFilePaths)

--------------------------------------------------------------------------------
-- Tests

goldenTests :: MonadTest m => m TestTree
goldenTests = testGroup "GoldenTests" <$> traverse makeTests
  [ testSpec
      { testName       = "simple-triviallyTrue"
      , testLocation   = Tests
      , testTargets    = [MarabouBackend]
      }
  ]

--------------------------------------------------------------------------------
-- Utils

testDir :: FilePath
testDir = baseTestDir </> "VerifyMode" </> "Golden"

makeTests :: MonadTest m => TestSpec -> m TestTree
makeTests spec@TestSpec{..} = do
  let resources = testResources spec
  let mkTest = makeTest testLocation testName resources
  testGroup testName <$> traverse mkTest testTargets

makeTest :: MonadTest m
         => TestLocation
         -> String
         -> Resources
         -> Backend
         -> m TestTree
makeTest location name resources backend = do
  loggingLevel <- getLoggingLevel

  let verifier    = getVerifier backend
  let verifierStr = layoutAsString (pretty verifier)
  let testName    = name <> "-" <> verifierStr
  let inputFile   = locationDir location name </> name <.> ".vcl"
  let outputFile  = testDir </> name </> name <> "-" <> verifierStr <> "-temp-output.txt"
  let goldenFile  = testDir </> name </> name <> "-" <> verifierStr <> "-output.txt"

  let run = runVehicle loggingLevel inputFile outputFile verifier resources
  return $ goldenFileTest testName run omitFilePaths goldenFile outputFile

runVehicle :: LoggingLevel -> FilePath -> FilePath -> VerifierIdentifier -> Resources -> IO ()
runVehicle loggingLevel inputFile outputFile verifier Resources{..} = do
  -- Total hack until we get Marabou installed properly
  verifierLocation <- findExecutable "which"
  run Options
    { globalOptions = defaultGlobalOptions
      { outFile      = Just outputFile
      , loggingLevel = loggingLevel
      }
    , modeOptions  = Just $ Verify $ VerifyOptions
      { specification    = inputFile
      , properties       = mempty
      , networkLocations = networks
      , datasetLocations = datasets
      , parameterValues  = parameters
      , verifier         = verifier
      , verifierLocation = verifierLocation
      , proofCache       = Nothing
      }
    }

mkStatus :: [(Text, PropertyStatus)] -> SpecificationStatus
mkStatus = SpecificationStatus . Map.fromList

getVerifier :: Backend -> VerifierIdentifier
getVerifier = \case
  VerifierBackend v -> v
  backend    -> error $ layoutAsString message
    where message = "Non-verifier backend" <+> quotePretty backend <+> "passed to verifier test."
