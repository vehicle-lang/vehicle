
module Vehicle.Compile.ExpandResources.Core where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Map qualified as Map

import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Resource

--------------------------------------------------------------------------------
-- The resource monad

type MonadExpandResources m =
  ( MonadCompile m
  , MonadReader (Resources, Bool) m
  , MonadWriter ResourceContext m
  , MonadState InferableParameterContext m
  )

isInferableParameter :: MonadExpandResources m => Identifier -> m Bool
isInferableParameter ident = gets (Map.member (nameOf ident))

--------------------------------------------------------------------------------
-- Resource contexts

data ResourceContext = ResourceContext
  { parameterContext :: ParameterContext
  , datasetContext   :: DatasetContext
  , networkContext   :: NetworkContext
  }

instance Semigroup ResourceContext where
  c1 <> c2 = ResourceContext
    { parameterContext = parameterContext c1 <> parameterContext c2
    , datasetContext   = datasetContext   c1 <> datasetContext   c2
    , networkContext   = networkContext   c1 <> networkContext   c2
    }

instance Monoid ResourceContext where
  mempty = ResourceContext mempty mempty mempty
