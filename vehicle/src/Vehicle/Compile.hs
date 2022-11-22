module Vehicle.Compile
  ( module CompilePrelude
  , CompileOptions(..)
  , compile
  , compileToAgda
  , compileToVerifier
  , typeCheck
  , typeCheckExpr
  , parseAndTypeCheckExpr
  , readSpecification
  ) where

import Control.Exception (IOException, catch)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text as T (Text)
import Data.Text.IO qualified as TIO
import System.Exit (exitFailure)
import System.IO (hPutStrLn)

import Vehicle.Backend.Agda
import Vehicle.Backend.LossFunction (LDecl, writeLossFunctionFiles)
import Vehicle.Backend.LossFunction qualified as LossFunction
import Vehicle.Backend.Prelude
import Vehicle.Compile.DependencyAnalysis
import Vehicle.Compile.Elaborate.External as External (elaborate, elaborateExpr)
import Vehicle.Compile.Error
import Vehicle.Compile.Error.Message
import Vehicle.Compile.ExpandResources
import Vehicle.Compile.Parse
import Vehicle.Compile.Prelude as CompilePrelude
import Vehicle.Compile.Queries (QueryData, compileToQueries)
import Vehicle.Compile.Resource
import Vehicle.Compile.Scope (scopeCheck, scopeCheckClosedExpr)
import Vehicle.Compile.Type (typeCheck, typeCheckExpr)
import Vehicle.Verify.Specification
import Vehicle.Verify.Specification.IO
import Vehicle.Verify.Verifier (verifiers)
import Vehicle.Verify.Verifier.Interface
import Vehicle.Compile.Normalise.NormExpr

data CompileOptions = CompileOptions
  { target                :: Backend
  , specification         :: FilePath
  , declarationsToCompile :: DeclarationNames
  , networkLocations      :: NetworkLocations
  , datasetLocations      :: DatasetLocations
  , parameterValues       :: ParameterValues
  , outputFile            :: Maybe FilePath
  , moduleName            :: Maybe String
  , proofCache            :: Maybe FilePath
  } deriving (Eq, Show)

compile :: VehicleIOSettings -> CompileOptions -> IO ()
compile loggingOptions CompileOptions{..} = do
  let resources = Resources networkLocations datasetLocations parameterValues
  spec <- readSpecification loggingOptions specification
  case target of
    TypeCheck -> do
      _ <- fromLoggedEitherIO loggingOptions $ typeCheckProg spec declarationsToCompile
      return ()

    ITP Agda -> do
      let agdaOptions = AgdaOptions proofCache outputFile moduleName
      agdaCode <- compileToAgda loggingOptions agdaOptions spec declarationsToCompile resources
      writeAgdaFile outputFile agdaCode

    VerifierBackend verifierIdentifier -> do
      let verifier = verifiers verifierIdentifier
      compiledSpecification <- compileToVerifier loggingOptions spec declarationsToCompile resources verifier
      case outputFile of
        Nothing     -> outputSpecification loggingOptions compiledSpecification
        Just folder -> writeSpecificationFiles verifier folder compiledSpecification

    LossFunction differentiableLogic -> do
      lossFunction <- compileToLossFunction loggingOptions spec declarationsToCompile resources differentiableLogic
      writeLossFunctionFiles outputFile differentiableLogic lossFunction


--------------------------------------------------------------------------------
-- Backend-specific compilation functions

compileToVerifier :: VehicleIOSettings
                  -> SpecificationText
                  -> PropertyNames
                  -> Resources
                  -> Verifier
                  -> IO (Specification QueryData)
compileToVerifier loggingOptions spec properties resources verifier =
  fromLoggedEitherIO loggingOptions $ do
    (prog, propertyCtx, networkCtx, _) <- typeCheckProgAndLoadResources spec properties resources
    compileToQueries verifier prog propertyCtx networkCtx


compileToLossFunction :: VehicleIOSettings
                      -> SpecificationText
                      -> DeclarationNames
                      -> Resources
                      -> DifferentiableLogic
                      -> IO [LDecl]
compileToLossFunction loggingOptions spec declarationsToCompile resources differentiableLogic = do
  fromLoggedEitherIO loggingOptions $ do
    (prog, propertyCtx, networkCtx, _) <- typeCheckProgAndLoadResources spec declarationsToCompile resources
    LossFunction.compile differentiableLogic prog propertyCtx networkCtx

compileToAgda :: VehicleIOSettings
              -> AgdaOptions
              -> SpecificationText
              -> PropertyNames
              -> Resources
              -> IO (Doc a)
compileToAgda loggingOptions agdaOptions spec properties _resources =
  fromLoggedEitherIO loggingOptions $ do
    (prog, propertyCtx, _) <- typeCheckProg spec properties
    compileProgToAgda (fmap unnormalised prog) propertyCtx agdaOptions

--------------------------------------------------------------------------------
-- Useful functions that apply multiple compiler passes

readSpecification :: MonadIO m => VehicleIOSettings -> FilePath -> m SpecificationText
readSpecification VehicleIOSettings{..} inputFile = do
  liftIO $ TIO.readFile inputFile `catch` \ (e :: IOException) -> do
    hPutStrLn errorHandle $
      "Error occured while reading input file: \n  " <> show e
    exitFailure

parseAndTypeCheckExpr :: MonadCompile m => Text -> m CheckedExpr
parseAndTypeCheckExpr expr = do
  bnfcExpr    <- parseVehicle expr
  vehicleExpr <- elaborateExpr bnfcExpr
  scopedExpr  <- scopeCheckClosedExpr vehicleExpr
  typedExpr   <- typeCheckExpr scopedExpr
  return typedExpr

-- | Parses and type-checks the program but does
-- not load networks and datasets from disk.
typeCheckProg :: MonadCompile m
              => SpecificationText
              -> DeclarationNames
              -> m (GluedProg, PropertyContext, DependencyGraph)
typeCheckProg spec declarationsToCompile = do
  bnfcProg <- parseVehicle spec
  (vehicleProg, uncheckedPropertyCtx) <- elaborate bnfcProg
  (scopedProg, dependencyGraph) <- scopeCheck vehicleProg
  prunedProg <- analyseDependenciesAndPrune scopedProg uncheckedPropertyCtx dependencyGraph declarationsToCompile
  (typedProg, propertyContext) <- typeCheck prunedProg uncheckedPropertyCtx
  return (typedProg, propertyContext, dependencyGraph)

-- | Parses, expands parameters and datasets, type-checks and then
-- checks the network types from disk. Used during compilation to
-- verification queries.
typeCheckProgAndLoadResources :: (MonadIO m, MonadCompile m)
                              => SpecificationText
                              -> DeclarationNames
                              -> Resources
                              -> m (CheckedProg, PropertyContext, NetworkContext, DependencyGraph)
typeCheckProgAndLoadResources spec declarationsToCompile resources = do
  (typedProg, propertyCtx, depGraph) <- typeCheckProg spec declarationsToCompile
  (networkCtx, finalProg) <- expandResources resources True typedProg
  return (finalProg, propertyCtx, networkCtx, depGraph)
