module Test.Compile.Utils
  ( TestLocation(..)
  , TestSpec(..)
  , testSpec
  , testResources
  , locationDir
  , textToCheckedExpr
  , retypeCheckExpr
  ) where

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
    , testParameters :: [(Text, Text)]
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

-- If you want to see the logs for tests, change `discardLogger` to
-- `traceLogger` here.
discardState :: ExceptT CompileError Logger a -> a
discardState e = case discardLogger $ logCompileError e of
  Left  x -> developerError $ pretty $ details x
  Right y -> y

traceLogger :: Logger a -> a
traceLogger m =
  let (v, logs) = runLogger MaxDetail m in
  trace (showMessages logs) v

textToCheckedExpr :: Text -> CheckedExpr
textToCheckedExpr = discardState . typeCheckExpr

retypeCheckExpr :: CheckedExpr -> CheckedExpr
retypeCheckExpr = discardState . typeCheck