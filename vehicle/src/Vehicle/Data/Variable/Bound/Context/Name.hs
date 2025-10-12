module Vehicle.Data.Variable.Bound.Context.Name
  ( module Core,
    prettyExternalInCtx,
    prettyFriendlyInCtx,
  )
where

-- Simple module that specialises MonadBoundContext for the common occurence
-- where you only need to know the bound variable's names.

import Data.Proxy (Proxy (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Data.Variable.Bound.Context.Class
import Vehicle.Data.Variable.Bound.Context.Name.Core as Core

prettyFriendlyInCtx ::
  (MonadNameContext m, PrettyFriendly (Contextualised a NamedBoundCtx)) =>
  a ->
  m (Doc b)
prettyFriendlyInCtx value = do
  ctx <- getNamedBoundCtx (Proxy @())
  return $ prettyFriendly (WithContext value ctx)

prettyExternalInCtx ::
  (MonadNameContext m, PrettyExternal (Contextualised a NamedBoundCtx)) =>
  a ->
  m (Doc b)
prettyExternalInCtx e = do
  ctx <- getNamedBoundCtx (Proxy @())
  return $ prettyExternal $ WithContext e ctx
