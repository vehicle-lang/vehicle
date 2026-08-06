module Vehicle.Compile.ExpandResources.Core where

import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Proxy (Proxy (..))
import Data.Void (Void)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Resource (DatasetElementType (..), DatasetType (..), NetworkName, ParameterType (..))
import Vehicle.Data.Builtin.Standard (Builtin)
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Code.Interface.Patterns
import Vehicle.Data.Variable.Free.Context (FreeContextT, MonadFreeContext, runFreshFreeContextT)
import Vehicle.Verify.Core

--------------------------------------------------------------------------------
-- Context

type NetworkContext = Map NetworkName NetworkContextInfo

lookupNetworkInfo :: NetworkName -> NetworkContext -> NetworkContextInfo
lookupNetworkInfo name ctx = do
  case Map.lookup name ctx of
    Nothing -> developerError $ "Expecting" <+> quotePretty name <+> "to be a @network"
    Just info -> info

--------------------------------------------------------------------------------
-- Resource contexts

type InferableParameterEntry = (DeclProvenance, ExternalResource, Int)

type InferableParameterContext = Map Identifier (Provenance, Type Builtin, Maybe InferableParameterEntry)

type ExplicitParameterContext = Map Identifier (Thunk Builtin)

--------------------------------------------------------------------------------
-- The resource monad

data ExpandResourcesState = ExpandResourcesState
  { networkCtx :: NetworkContext,
    inferableParamCtx :: InferableParameterContext,
    nonInferableParamCtx :: ExplicitParameterContext,
    missingResources :: [MissingResource],
    unusedResources :: Resources
  }

initialExpandResourcesState :: Resources -> ExpandResourcesState
initialExpandResourcesState resources =
  ExpandResourcesState
    { networkCtx = mempty,
      inferableParamCtx = mempty,
      nonInferableParamCtx = mempty,
      missingResources = mempty,
      unusedResources = resources
    }

type MonadExpandResources m =
  ( MonadCompile m,
    MonadState ExpandResourcesState m,
    MonadFreeContext Builtin m
  )

runExpandResourcesT ::
  (Monad m) =>
  Resources ->
  StateT ExpandResourcesState (FreeContextT Builtin m) a ->
  m (a, ExpandResourcesState)
runExpandResourcesT resources action =
  runFreshFreeContextT (Proxy @Builtin) $
    runStateT action (initialExpandResourcesState resources)

getExplicitParameterContext ::
  (MonadExpandResources m) =>
  m ExplicitParameterContext
getExplicitParameterContext = gets nonInferableParamCtx

getInferableParameterContext ::
  (MonadExpandResources m) =>
  m InferableParameterContext
getInferableParameterContext = gets inferableParamCtx

isInferableParameter ::
  (MonadExpandResources m) =>
  Identifier ->
  m Bool
isInferableParameter ident =
  Map.member ident <$> getInferableParameterContext

noteInferableParameter ::
  (MonadExpandResources m) =>
  Provenance ->
  Identifier ->
  Type Builtin ->
  m ()
noteInferableParameter p ident paramType =
  modify $ \ExpandResourcesState {..} ->
    ExpandResourcesState
      { inferableParamCtx = Map.insert ident (p, paramType, Nothing) inferableParamCtx,
        ..
      }

findNonInferableParameterValue ::
  (MonadExpandResources m) =>
  Provenance ->
  Identifier ->
  m (Maybe String)
findNonInferableParameterValue p ident = do
  ExpandResourcesState {unusedResources = Resources {..}, ..} <- get
  let (maybeResult, unusedParameters) = deleteAndGet (nameOf ident) parameters
  case maybeResult of
    Nothing -> do
      put $
        ExpandResourcesState
          { unusedResources = Resources {..},
            missingResources = (Parameter, (ident, p)) : missingResources,
            ..
          }
      return Nothing
    Just result -> do
      put $
        ExpandResourcesState
          { unusedResources = Resources {parameters = unusedParameters, ..},
            ..
          }
      return $ Just result

noteNonInferableParameter ::
  (MonadExpandResources m) =>
  Identifier ->
  Thunk Builtin ->
  m ()
noteNonInferableParameter ident value =
  modify $ \ExpandResourcesState {..} ->
    ExpandResourcesState
      { nonInferableParamCtx = Map.insert ident value nonInferableParamCtx,
        ..
      }

findDatasetValue ::
  (MonadExpandResources m) =>
  Provenance ->
  Identifier ->
  m (Maybe FilePath)
findDatasetValue p ident = do
  ExpandResourcesState {unusedResources = Resources {..}, ..} <- get
  let (maybeResult, unusedDatasets) = deleteAndGet (nameOf ident) datasets
  case maybeResult of
    Nothing -> do
      put $
        ExpandResourcesState
          { unusedResources = Resources {..},
            missingResources = (Dataset, (ident, p)) : missingResources,
            ..
          }
      return Nothing
    Just result -> do
      put $
        ExpandResourcesState
          { unusedResources = Resources {datasets = unusedDatasets, ..},
            ..
          }
      return $ Just result

findNetworkValue ::
  (MonadExpandResources m) =>
  Provenance ->
  Identifier ->
  m (Maybe FilePath)
findNetworkValue p ident = do
  ExpandResourcesState {unusedResources = Resources {..}, ..} <- get
  let (maybeResult, unusedNetworks) = deleteAndGet (nameOf ident) networks
  case maybeResult of
    Nothing -> do
      put $
        ExpandResourcesState
          { unusedResources = Resources {..},
            missingResources = (Network, (ident, p)) : missingResources,
            ..
          }
      return Nothing
    Just result -> do
      put $
        ExpandResourcesState
          { unusedResources = Resources {networks = unusedNetworks, ..},
            ..
          }
      return $ Just result

addPossibleInferableParameterSolution ::
  (MonadExpandResources m) =>
  Identifier ->
  Provenance ->
  Type Builtin ->
  InferableParameterEntry ->
  m ()
addPossibleInferableParameterSolution ident p declType entry =
  modify $ \ExpandResourcesState {..} ->
    ExpandResourcesState
      { inferableParamCtx = Map.insert ident (p, declType, Just entry) inferableParamCtx,
        ..
      }

noteNetwork ::
  (MonadExpandResources m) =>
  Identifier ->
  NetworkContextInfo ->
  m ()
noteNetwork ident details =
  modify $ \ExpandResourcesState {..} ->
    ExpandResourcesState
      { networkCtx = Map.insert (nameOf ident) details networkCtx,
        ..
      }

resourceTypingError :: (MonadCompile m) => Doc Void -> ForcedType Builtin -> m b
resourceTypingError doc invalidType =
  compilerDeveloperError $
    "Invalid"
      <+> doc
      <+> "type"
      <+> squotes (prettyVerbose invalidType)
      <+> "should have been caught during type-checking"

fromParameterType :: ParameterType (Thunk Builtin) -> Thunk Builtin
fromParameterType typ = Forced $ case typ of
  ParameterBoolType -> IBoolType
  ParameterIndexType size -> IIndexType size
  ParameterRealType -> IRatType
  ParameterNatType -> INatType

fromDatasetType :: DatasetType (Thunk Builtin) -> Thunk Builtin
fromDatasetType typ = case typ of
  DatasetVectorType tElem n -> Forced $ IVectorType (fromDatasetType tElem) n
  DatasetListType tElem -> Forced $ IListType (fromDatasetType tElem)
  DatasetTensorType tElem sizeType -> Forced $ ITensorType (fromDatasetElementType tElem) sizeType
  DatasetRecordType ident _fields -> Forced $ VFreeVar ident []
  DatasetElementType t -> fromDatasetElementType t

fromDatasetElementType :: DatasetElementType (Thunk Builtin) -> Thunk Builtin
fromDatasetElementType typ = Forced $ case typ of
  DatasetRealType -> IRatType
  DatasetNatType -> INatType
  DatasetIndexType size -> IIndexType size
