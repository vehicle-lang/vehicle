module Test.Compile.Utils
  ( MonadTest
  , TestLoggingSettings
  , getTestLoggingSettings
  , TestLocation(..)
  , TestSpec(..)
  , testSpec
  , testResources
  , locationDir
  , textToCheckedExpr
  , retypeCheckExpr
  , traceLogs
  , discardLogs
  ) where

import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Except (MonadError(..), ExceptT, runExceptT)
import Data.Text.Array
import Data.Map
import Data.Map qualified as Map
import Data.Text
import Debug.Trace
import System.FilePath ((</>))

import Test.Tasty ( TestName, testGroup, after, DependencyType )
import Test.Tasty.Runners (TestTree(..))

import Vehicle.Compile (typeCheckExpr)
import Vehicle.Backend.Prelude
import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Error.Message
import Vehicle.Compile.Type (typeCheck)

--------------------------------------------------------------------------------
-- Test settings monad

type MonadTest m = MonadReader (Maybe Int) m

type TestLoggingSettings = (Maybe (Maybe FilePath), Int)

getTestLoggingSettings :: MonadTest m => m TestLoggingSettings
getTestLoggingSettings = do
  logLevel <- ask
  return $ case logLevel of
    Nothing -> (Nothing, 0)
    Just l  -> (Just Nothing, l)


--------------------------------------------------------------------------------
-- Test infrastructure

data TestLocation
  = Tests
  | Examples

locationDir :: TestLocation -> String -> FilePath
locationDir Tests    _    = "test" </> "specs"
locationDir Examples name = "examples" </> name

data TestSpec = TestSpec
    { testName       :: String
    , testLocation   :: TestLocation
    , testTargets    :: [Backend]
    , testNetworks   :: [(Text, FilePath)]
    , testDatasets   :: [(Text, FilePath)]
    , testParameters :: [(Text, String)]
    }

testSpec :: TestSpec
testSpec = TestSpec
  { testName       = error "Must provide the 'testName' field"
  , testLocation   = error "Must provide the 'testLocation' field"
  , testTargets    = error "Must provide the 'testTargets' field"
  , testNetworks   = []
  , testDatasets   = []
  , testParameters = []
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

--------------------------------------------------------------------------------
-- Other utilities

traceLogger :: Logger a -> a
traceLogger m =
  let (v, logs) = runLogger MaxDetail m in
  trace (showMessages logs) v

discardLogs :: ExceptT CompileError Logger a -> a
discardLogs e = case discardLogger $ logCompileError e of
  Left  x -> developerError $ pretty $ details x
  Right y -> y

traceLogs :: ExceptT CompileError Logger a -> a
traceLogs e = case traceLogger $ logCompileError e of
  Left  x -> developerError $ pretty $ details x
  Right y -> y

textToCheckedExpr :: Text -> CheckedExpr
textToCheckedExpr = discardLogs . typeCheckExpr

retypeCheckExpr :: CheckedExpr -> CheckedExpr
retypeCheckExpr = discardLogs . typeCheck