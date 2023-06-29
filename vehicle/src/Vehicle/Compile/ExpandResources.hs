module Vehicle.Compile.ExpandResources
  ( expandResources,
    splitResourceCtx,
  )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources.Core
import Vehicle.Compile.ExpandResources.Dataset
import Vehicle.Compile.ExpandResources.Network
import Vehicle.Compile.ExpandResources.Parameter
import Vehicle.Compile.Prelude
import Vehicle.Compile.Resource
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Compile.Warning (CompileWarning (..))
import Vehicle.Expr.Normalised (pattern VNatLiteral)

-- | Calculates the context for external resources, reading them from disk and
-- inferring the values of inferable parameters.
expandResources ::
  (MonadIO m, MonadCompile m) =>
  Resources ->
  StandardGluedProg ->
  m ResourceContext
expandResources resources prog =
  logCompilerPass MinDetail "expansion of external resources" $ do
    (intermediateResourcesCtx, inferableParameterCtx) <-
      execStateT (runReaderT (readResourcesInProg prog) resources) (emptyResourceCtx, mempty)

    checkForUnusedResources resources intermediateResourcesCtx

    fillInInferableParameters intermediateResourcesCtx inferableParameterCtx

splitResourceCtx :: ResourceContext -> (NetworkContext, StandardNormDeclCtx)
splitResourceCtx ResourceContext {..} = do
  let mkEntry expr =
        NormDeclCtxEntry
          { declExpr = expr,
            declAnns = [],
            declArity = 0
          }
  let declCtx = fmap mkEntry parameterContext <> fmap mkEntry datasetContext
  (networkContext, declCtx)

type MonadReadResources m =
  ( MonadIO m,
    MonadExpandResources m
  )

-- | Goes through the program finding all
-- the resources, comparing the data against the type in the spec, and making
-- note of the values for implicit parameters.
readResourcesInProg :: (MonadReadResources m) => StandardGluedProg -> m ()
readResourcesInProg (Main ds) = traverse_ readResourcesInDecl ds

readResourcesInDecl :: (MonadReadResources m) => StandardGluedDecl -> m ()
readResourcesInDecl decl = case decl of
  DefFunction {} -> return ()
  DefAbstract p ident defType declType ->
    case defType of
      ParameterDef sort -> case sort of
        Inferable ->
          noteInferableParameter p ident
        NonInferable -> do
          parameterValues <- asks parameters
          parameterExpr <- parseParameterValue parameterValues (ident, p) declType
          addParameter ident parameterExpr
      DatasetDef -> do
        datasetLocations <- asks datasets
        datasetExpr <- parseDataset datasetLocations (ident, p) declType
        addDataset ident datasetExpr
      NetworkDef -> do
        networkLocations <- asks networks
        networkDetails <- checkNetwork networkLocations (ident, p) declType
        addNetworkType ident networkDetails
      PostulateDef -> return ()

checkForUnusedResources ::
  (MonadLogger m) =>
  Resources ->
  ResourceContext ->
  m ()
checkForUnusedResources Resources {..} ResourceContext {..} = do
  warnIfUnusedResources Parameter parameters parameterContext
  warnIfUnusedResources Dataset datasets datasetContext
  warnIfUnusedResources Network networks networkContext

fillInInferableParameters :: (MonadCompile m) => ResourceContext -> InferableParameterContext -> m ResourceContext
fillInInferableParameters ResourceContext {..} inferableCtx = do
  newParameterCtx <- foldM insertInferableParameter parameterContext (Map.assocs inferableCtx)
  return $ ResourceContext {parameterContext = newParameterCtx, ..}
  where
    insertInferableParameter ::
      (MonadCompile m) =>
      ParameterContext ->
      (Identifier, Either Provenance InferableParameterEntry) ->
      m ParameterContext
    insertInferableParameter ctx (param, maybeValue) = case maybeValue of
      Left p -> throwError $ InferableParameterUninferrable (param, p)
      Right (_, _, v) -> return $ Map.insert param (VNatLiteral v) ctx

warnIfUnusedResources ::
  (MonadLogger m, HasName ident Name) =>
  ExternalResource ->
  Map Name a ->
  Map ident b ->
  m ()
warnIfUnusedResources resourceType given found = do
  when (null found) $
    logDebug MinDetail $
      "No" <+> pretty resourceType <> "s found in program"

  let givenNames = Map.keysSet given
  let foundNames = Set.map nameOf $ Map.keysSet found
  let unusedParams = givenNames `Set.difference` foundNames
  when (Set.size unusedParams > 0) $
    logWarning $
      pretty $
        UnusedResource resourceType unusedParams
