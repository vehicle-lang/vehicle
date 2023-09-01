module Vehicle.Compile.Context.Var
  ( MonadVarContext,
    runFreshVarContextT,
    module BoundCtx,
    module FreeContext,
    normalise,
  )
where

import Data.Data (Proxy (..))
import Vehicle.Compile.Context.Bound as BoundCtx
import Vehicle.Compile.Context.Free as FreeContext
import Vehicle.Compile.Prelude
import Vehicle.Data.NormalisedExpr

type MonadVarContext builtin m =
  (MonadBoundContext builtin m, MonadFreeContext builtin m)

runFreshVarContextT ::
  forall m builtin a.
  (Monad m) =>
  Proxy builtin ->
  BoundContextT builtin (FreeContextT builtin m) a ->
  m a
runFreshVarContextT p = runFreshFreeContextT p . runFreshBoundContextT p

normalise ::
  forall builtin m.
  (MonadVarContext builtin m) =>
  Expr Ix builtin ->
  m (Value builtin)
normalise e = do
  boundCtx <- getBoundCtx (Proxy @builtin)
  let env = boundContextToEnv boundCtx
  normaliseInEnv env e
