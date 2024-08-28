module Vehicle.Compile.Context.Var
  ( MonadVarContext,
    runFreshVarContextT,
    module BoundCtx,
    module FreeContext,
  )
where

import Data.Data (Proxy (..))
import Vehicle.Compile.Context.Bound as BoundCtx
import Vehicle.Compile.Context.Free as FreeContext
import Vehicle.Compile.Prelude

type MonadVarContext builtin m =
  (MonadBoundContext (Type builtin) m, MonadFreeContext builtin m)

runFreshVarContextT ::
  forall m builtin a.
  (Monad m) =>
  Proxy builtin ->
  BoundContextT builtin (FreeContextT builtin m) a ->
  m a
runFreshVarContextT p = runFreshFreeContextT (Proxy @builtin) . runFreshBoundContextT p
