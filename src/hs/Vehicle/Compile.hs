module Vehicle.Compile
  ( module CompilePrelude
  , CompileOptions(..)
  , compile
  , compileToMarabou
  , compileToAgda
  , typeCheck
  , typeCheckExpr
  , parseAndTypeCheckExpr
  , readSpecification
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Exception (IOException, catch)
import Data.Set (Set)
import Data.Text as T (Text)
import Data.Text.IO qualified as TIO
import System.IO (hPutStrLn)
import System.Exit (exitFailure)
import System.Directory (makeAbsolute)

import Vehicle.Backend.Prelude
import Vehicle.Compile.Prelude as CompilePrelude
import Vehicle.Compile.Error
import Vehicle.Compile.Error.Message
import Vehicle.Compile.Parse
import Vehicle.Compile.DependencyAnalysis
import Vehicle.Compile.Elaborate.External as External (elaborate, elaborateExpr)
import Vehicle.Compile.Scope (scopeCheck, scopeCheckClosedExpr)
import Vehicle.Compile.Type (typeCheck, typeCheckExpr)
import Vehicle.Backend.Marabou qualified as Marabou
import Vehicle.Backend.Marabou (MarabouSpec)
import Vehicle.Backend.Agda
import Vehicle.Backend.LossFunction qualified as LossFunction
import Vehicle.Backend.LossFunction ( LExpr, writeLossFunctionFiles)
import Vehicle.Compile.Resource
import Vehicle.Compile.ExpandResources

compile :: LoggingOptions -> CompileOptions -> IO ()
compile loggingOptions CompileOptions{..} = do
  let resources = Resources networkLocations datasetLocations parameterValues
  spec <- readSpecification loggingOptions specification
  case target of
    TypeCheck -> do
      _ <- fromLoggedEitherIO loggingOptions $ typeCheckProg spec declarationsToCompile
      return ()

    ITP Agda -> do
      proofCacheLocation <- maybe (return Nothing) (fmap Just . makeAbsolute) proofCache
      let agdaOptions = AgdaOptions proofCacheLocation outputFile moduleName
      agdaCode <- compileToAgda loggingOptions agdaOptions spec declarationsToCompile resources
      writeAgdaFile outputFile agdaCode

    Verifier Marabou -> do
      marabouProperties <- compileToMarabou loggingOptions spec declarationsToCompile resources
      Marabou.writeSpecFiles outputFile marabouProperties

    LossFunction -> do
      lossFunction <- compileToLossFunction loggingOptions spec declarationsToCompile resources
      writeLossFunctionFiles outputFile lossFunction


--------------------------------------------------------------------------------
-- Backend-specific compilation functions

compileToMarabou :: LoggingOptions
                 -> Specification
                 -> Properties
                 -> Resources
                 -> IO MarabouSpec
compileToMarabou loggingOptions spec properties resources =
  fromLoggedEitherIO loggingOptions $ do
    (prog, propertyCtx, networkCtx, _) <- typeCheckProgAndLoadResources spec properties resources
    Marabou.compile prog propertyCtx networkCtx

compileToLossFunction :: LoggingOptions
                      -> Specification
                      -> Set Symbol
                      -> Resources
                      -> IO [LExpr]
compileToLossFunction loggingOptions spec declarationsToCompile resources = do
  fromLoggedEitherIO loggingOptions $ do
    (prog, propertyCtx, networkCtx, _) <- typeCheckProgAndLoadResources spec declarationsToCompile resources
    LossFunction.compile prog propertyCtx networkCtx

compileToAgda :: LoggingOptions
              -> AgdaOptions
              -> Specification
              -> Properties
              -> Resources
              -> IO (Doc a)
compileToAgda loggingOptions agdaOptions spec properties _resources =
  fromLoggedEitherIO loggingOptions $ do
    (prog, propertyCtx, _) <- typeCheckProg spec properties
    compileProgToAgda prog propertyCtx agdaOptions

--------------------------------------------------------------------------------
-- Useful functions that apply multiple compiler passes

readSpecification :: MonadIO m => LoggingOptions -> FilePath -> m Specification
readSpecification LoggingOptions{..} inputFile = do
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
              => Specification
              -> Set Symbol
              -> m (CheckedProg, PropertyContext, DependencyGraph)
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
                              => Specification
                              -> Properties
                              -> Resources
                              -> m (CheckedProg, PropertyContext, NetworkContext, DependencyGraph)
typeCheckProgAndLoadResources spec properties resources = do
  (typedProg, propertyCtx, depGraph) <- typeCheckProg spec properties
  (networkCtx, finalProg) <- expandResources resources True typedProg
  return (finalProg, propertyCtx, networkCtx, depGraph)
