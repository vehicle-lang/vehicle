module Vehicle.Test.Utils
  ( module FilePathUtils
  , module GoldenUtils
  , baseTestDir
  , MonadTest
  , TestLocation(..)
  , TestSpec(..)
  , getLoggingLevel
  , testSpec
  , testResources
  , locationDir
  , unitTestCase
  , normTypeClasses
  ) where

import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.Reader (MonadReader (..), asks)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Array
import Debug.Trace
import System.FilePath ((</>))

import Test.Tasty (DependencyType, TestName, after, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)
import Test.Tasty.Runners (TestTree (..))

import Vehicle.Backend.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Error.Message
import Vehicle.Compile.Normalise
import Vehicle.Compile.Prelude

import Data.Functor.Foldable (Recursive (..))
import Data.List.NonEmpty qualified as NonEmpty
import Vehicle.Test.Utils.FilePath as FilePathUtils
import Vehicle.Test.Utils.Golden as GoldenUtils

--------------------------------------------------------------------------------
-- Test locations

baseTestDir :: FilePath
baseTestDir = "test" </> "Vehicle" </> "Test"

--------------------------------------------------------------------------------
-- Test settings monad

type MonadTest m = MonadReader LoggingLevel m

getLoggingLevel :: MonadTest m => m LoggingLevel
getLoggingLevel = ask

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
    , testDecls      :: [Text]
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
normTypeClasses = cata $ \case
  AppF ann fun args -> do
    fun'  <- fun
    args' <- traverse sequenceA args
    case fun' of
      Builtin p (TypeClassOp op) -> case nfTypeClassOp p op (NonEmpty.toList args') of
        Nothing  -> error "No metas should be present"
        Just res -> do
          (fn, newArgs) <- res
          return $ normApp p fn newArgs

      _ -> return $ App ann fun' args'

  UniverseF ann l                 -> return $ Universe ann l
  HoleF     ann n                 -> return $ Hole ann n
  MetaF     ann m                 -> return $ Meta ann m
  LiteralF  ann l                 -> return $ Literal ann l
  BuiltinF  ann op                -> return $ Builtin ann op
  AnnF      ann e t               -> Ann ann <$> e <*> t
  PiF       ann binder result     -> Pi  ann <$> sequenceA binder <*> result
  LetF      ann bound binder body -> Let ann <$> bound <*> sequenceA binder <*> body
  LamF      ann binder body       -> Lam ann <$> sequenceA binder <*> body
  LVecF     ann xs                -> LVec ann <$> sequence xs
  VarF      ann v                 -> return $ Var ann v

--------------------------------------------------------------------------------
-- Other utilities

traceLogs :: LoggingLevel -> ExceptT CompileError Logger a -> a
traceLogs logLevel e = do
  let e' = logCompileError e
  let (v, logs) = runLogger logLevel e'
  let result = if null logs then v else trace (showMessages logs) v
  case result of
    Left  x -> developerError $ pretty $ details x
    Right y -> y

unitTestCase :: MonadTest m => String -> ExceptT CompileError Logger Assertion -> m TestTree
unitTestCase testName e = do
  logLevel <- ask
  return $ testCase testName $ traceLogs logLevel e
