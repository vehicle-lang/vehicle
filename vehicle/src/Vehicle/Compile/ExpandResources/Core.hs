module Vehicle.Compile.ExpandResources.Core where

import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor (Bifunctor (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Resource
import Vehicle.Compile.Type.Subsystem.Standard.Core

--------------------------------------------------------------------------------
-- Resource contexts

type InferableParameterEntry = (DeclProvenance, ExternalResource, Int)

type InferableParameterContext = Map Identifier (Either Provenance InferableParameterEntry)

--------------------------------------------------------------------------------
-- The resource monad

type MonadExpandResources m =
  ( MonadCompile m,
    MonadReader Resources m,
    MonadState (ResourceContext, InferableParameterContext) m
  )

getInferableParameterContext ::
  (MonadExpandResources m) =>
  m InferableParameterContext
getInferableParameterContext = gets snd

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
  m ()
noteInferableParameter p ident =
  modify (second $ Map.insert ident (Left p))

addPossibleInferableParameterSolution ::
  (MonadExpandResources m) =>
  Identifier ->
  InferableParameterEntry ->
  m ()
addPossibleInferableParameterSolution ident entry =
  modify (second $ Map.insert ident (Right entry))

emptyResourceCtx :: ResourceContext
emptyResourceCtx = ResourceContext mempty mempty mempty

addParameter ::
  (MonadExpandResources m) =>
  Identifier ->
  StandardNormExpr ->
  m ()
addParameter ident value = modify $ first $ \ResourceContext {..} ->
  ResourceContext
    { parameterContext = Map.insert ident value parameterContext,
      ..
    }

addDataset ::
  (MonadExpandResources m) =>
  Identifier ->
  StandardNormExpr ->
  m ()
addDataset ident value = modify $ first $ \ResourceContext {..} ->
  ResourceContext
    { datasetContext = Map.insert ident value datasetContext,
      ..
    }

addNetworkType ::
  (MonadExpandResources m) =>
  Identifier ->
  (FilePath, NetworkType) ->
  m ()
addNetworkType ident details = modify $ first $ \ResourceContext {..} ->
  ResourceContext
    { networkContext = Map.insert (nameOf ident) details networkContext,
      ..
    }
