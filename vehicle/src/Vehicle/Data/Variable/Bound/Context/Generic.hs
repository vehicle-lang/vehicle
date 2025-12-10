module Vehicle.Data.Variable.Bound.Context.Generic
  ( module Export,
    getBoundVarByIx,
    getBoundVarByLv,
  )
where

import Data.Proxy (Proxy (..))
import GHC.Stack (HasCallStack)
import Vehicle.Compile.Prelude
import Vehicle.Data.Variable.Bound.Context.Generic.Class as Export
import Vehicle.Data.Variable.Bound.Context.Generic.Core as Export
import Vehicle.Data.Variable.Bound.Context.Generic.Instance as Export

getBoundVarByIx ::
  forall expr m.
  (MonadBoundContext expr m, HasCallStack) =>
  Proxy expr ->
  Ix ->
  m (GenericBinder expr)
getBoundVarByIx _ ix =
  lookupIxInBoundCtx ix <$> getBoundCtx (Proxy @expr)

getBoundVarByLv ::
  forall expr m.
  (MonadBoundContext expr m, HasCallStack) =>
  Proxy expr ->
  Lv ->
  m (GenericBinder expr)
getBoundVarByLv _ lv =
  lookupLvInBoundCtx lv <$> getBoundCtx (Proxy @expr)
