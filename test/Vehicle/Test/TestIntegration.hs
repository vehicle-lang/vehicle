
import Data.Maybe (mapMaybe)
import GHC.IO.Encoding
import System.Environment
import System.Directory (findExecutable)
import System.FilePath

import Vehicle.Backend.Prelude
import Vehicle.Prelude

import Test.Tasty

import Vehicle.Test.Utils.TestProgram (testProgram, CatchStderr(..))
import Vehicle.Test.CompileMode.Golden
import Vehicle.Test.Utils (MonadTest, filepathTests, TestSpec (..))
import Vehicle.Verify.VerificationStatus (writeProofCache, ProofCache (..), SpecificationStatus (SpecificationStatus))

-- Can't figure out how to get this passed in via the command-line *sadness*
testLogLevel :: Int
testLogLevel = 0

main :: IO ()
main = do
  vehicleExecutablePath <- findExecutable "vehicle"
  case vehicleExecutablePath of
    Nothing -> error "Please install Vehicle using `cabal install` before running the \"integration test suite\""
    Just{}  -> return ()

  print "WARNING: these tests use the version of Vehicle currently installed. \
      \ Run `cabal install` to install the latest version."

  defaultMain tests

tests :: TestTree
tests = do
  testGroup "IntegrationTests"
    [ agdaGoldenTests
    ]


--------------------------------------------------------------------------------
-- Integration tests

mockProofCacheLocation :: FilePath
mockProofCacheLocation = "./proofcache.vclp"

agdaGoldenTests :: TestTree
agdaGoldenTests = do
  let tests =  testGroup "AgdaIntegrationTests" $ mapMaybe makeAgdaTest goldenTestSpecifications
  let testsWithStderr = localOption (CatchStderr True) tests
  let testsWithProofCache = withResource agdaGoldenTestsSetup agdaGoldenTestsTeardown (const testsWithStderr)
  testsWithProofCache

agdaGoldenTestsSetup :: IO ()
agdaGoldenTestsSetup = do
  writeProofCache mockProofCacheLocation $
    ProofCache
      { proofCacheVersion  = vehicleVersion
      , status             = SpecificationStatus mempty
      , resourceSummaries  = []
      , originalSpec       = ""
      , originalProperties = mempty
      }

makeAgdaTest :: TestSpec -> Maybe TestTree
makeAgdaTest spec@TestSpec{..}
  | AgdaBackend `notElem` testTargets = Nothing
  | otherwise = Just $ do
    let backend         = AgdaBackend
    let name            = layoutAsString (pretty backend) <> "-integration" <> "-" <> testName
    let filePathSuffix  = goldenFilepathSuffix backend
    let goldenDirectory = goldenTestDirectory </> testName
    let goldenFile      = testName <> "-output" <> filePathSuffix

    testProgram name "agda" [goldenFile, "--library=vehicle", "--include-path=."] (Just goldenDirectory)

agdaGoldenTestsTeardown :: () -> IO ()
agdaGoldenTestsTeardown () = removeFileIfExists mockProofCacheLocation