module Vehicle.Compile.ExpandResources.Core where

import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as Map
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Resource

--------------------------------------------------------------------------------
-- The resource monad

type MonadExpandResources m =
  ( MonadCompile m,
    MonadReader Resources m,
    MonadState ResourceContext m
  )

isInferableParameter :: MonadExpandResources m => Identifier -> m Bool
isInferableParameter ident = do
  inferableCtx <- gets inferableParameterContext
  return $ Map.member (nameOf ident) inferableCtx

--------------------------------------------------------------------------------
-- Resource contexts

type InferableParameterEntry = (DeclProvenance, Resource, Int)

type InferableParameterContext = Map Name (Maybe InferableParameterEntry)

type ParameterContext = Map Name TypedExpr

type DatasetContext = Map Name TypedExpr

data ResourceContext = ResourceContext
  { inferableParameterContext :: InferableParameterContext,
    parameterContext :: ParameterContext,
    datasetContext :: DatasetContext,
    networkContext :: NetworkContext
  }

emptyResourceCtx :: ResourceContext
emptyResourceCtx = ResourceContext mempty mempty mempty mempty

noteInferableParameter :: Identifier -> ResourceContext -> ResourceContext
noteInferableParameter ident ResourceContext {..} =
  ResourceContext
    { inferableParameterContext = Map.insert (nameOf ident) Nothing inferableParameterContext,
      ..
    }

addPossibleInferableParameterSolution :: Identifier -> InferableParameterEntry -> ResourceContext -> ResourceContext
addPossibleInferableParameterSolution ident entry ResourceContext {..} =
  ResourceContext
    { inferableParameterContext = Map.insert (nameOf ident) (Just entry) inferableParameterContext,
      ..
    }

addParameter :: Identifier -> TypedExpr -> ResourceContext -> ResourceContext
addParameter ident value ResourceContext {..} =
  ResourceContext
    { parameterContext = Map.insert (nameOf ident) value parameterContext,
      ..
    }

addDataset :: Identifier -> TypedExpr -> ResourceContext -> ResourceContext
addDataset ident value ResourceContext {..} =
  ResourceContext
    { datasetContext = Map.insert (nameOf ident) value datasetContext,
      ..
    }

addNetworkType :: Identifier -> NetworkType -> ResourceContext -> ResourceContext
addNetworkType ident details ResourceContext {..} =
  ResourceContext
    { networkContext = Map.insert (nameOf ident) details networkContext,
      ..
    }
