module Vehicle.Test.Utils
  ( module FilePathUtils
  , module GoldenUtils
  , baseTestDir
  , MonadTest
  , TestLoggingSettings
  , getTestLoggingSettings
  , TestLocation(..)
  , TestSpec(..)
  , testSpec
  , testResources
  , locationDir
  , unitTestCase
  , normTypeClasses
  ) where

import Control.Monad.Reader (MonadReader(..), asks)
import Control.Monad.Except (MonadError(..), ExceptT, runExceptT)
import Data.Maybe (fromMaybe)
import Data.Text.Array
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Debug.Trace
import System.FilePath ((</>))

import Test.Tasty ( TestName, testGroup, after, DependencyType )
import Test.Tasty.Runners (TestTree(..))
import Test.Tasty.HUnit (testCase, Assertion)

import Vehicle.Backend.Prelude
import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Error.Message
import Vehicle.Compile.Normalise

import Vehicle.Test.Utils.FilePath as FilePathUtils
import Vehicle.Test.Utils.Golden as GoldenUtils

--------------------------------------------------------------------------------
-- Test locations

baseTestDir :: FilePath
baseTestDir = "test" </> "Vehicle" </> "Test"

--------------------------------------------------------------------------------
-- Test settings monad

type MonadTest m = MonadReader Int m

type TestLoggingSettings = (Maybe (Maybe FilePath), Int)

getTestLoggingSettings :: MonadTest m => m TestLoggingSettings
getTestLoggingSettings = do
  logLevel <- ask
  let logLocation = if logLevel == 0 then Nothing else Just Nothing
  return (logLocation, logLevel)

getDebugLevel :: MonadTest m => m DebugLevel
getDebugLevel = asks intToDebugLevel

--------------------------------------------------------------------------------
-- Test infrastructure

data TestLocation
  = Tests
  | Examples

locationDir :: TestLocation -> String -> FilePath
locationDir Tests    _    = "test" </> "specs"
locationDir Examples name = "examples" </> name

data TestSpec = TestSpec
    { testName         :: String
    , testLocation     :: TestLocation
    , testTargets      :: [Backend]
    , testNetworks     :: [(Text, FilePath)]
    , testDatasets     :: [(Text, FilePath)]
    , testParameters   :: [(Text, String)]
    , testDecls        :: [Text]
    }

testSpec :: TestSpec
testSpec = TestSpec
  { testName         = error "Must provide the 'testName' field"
  , testLocation     = error "Must provide the 'testLocation' field"
  , testTargets      = error "Must provide the 'testTargets' field"
  , testNetworks     = []
  , testDatasets     = []
  , testParameters   = []
  , testDecls        = []
  }

testResources :: TestSpec -> Resources
testResources TestSpec{..} =
  let datasetLocation = case testLocation of
        Tests    -> "test" </> "datasets"
        Examples -> "examples" </> testName in
  let datasets   = fmap (datasetLocation </>) (Map.fromList testDatasets) in

  let networkLocation = case testLocation of
        Tests    -> "test" </> "networks"
        Examples -> "examples" </> testName in
  let networks   = fmap (networkLocation </>) (Map.fromList testNetworks) in

  let parameters = Map.fromList testParameters in

  Resources networks datasets parameters

normTypeClasses :: MonadCompile m => CheckedExpr -> m CheckedExpr
normTypeClasses e = normalise e noNormalisationOptions
  { normaliseBuiltin = \case
      TypeClassOp{} -> True
      FromNat{}     -> True
      FromRat{}     -> True
      FromVec{}     -> True
      _             -> False
  }

--------------------------------------------------------------------------------
-- Other utilities

traceLogs :: Int -> ExceptT CompileError Logger a -> a
traceLogs logLevel e = do
  let debugLevel = intToDebugLevel logLevel
  let e' = logCompileError e
  let (v, logs) = runLogger debugLevel e'
  let result = if null logs then v else trace (showMessages logs) v
  case result of
    Left  x -> developerError $ pretty $ details x
    Right y -> y

unitTestCase :: MonadTest m => String -> ExceptT CompileError Logger Assertion -> m TestTree
unitTestCase testName e = do
  logLevel <- ask
  return $ testCase testName $ traceLogs logLevel e