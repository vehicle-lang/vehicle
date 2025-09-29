module Vehicle.Compile.Context.Name
  ( module Core,
    prettyExternalInCtx,
    prettyFriendlyInCtx,
  )
where

-- Simple module that specialises MonadBoundContext for the common occurence
-- where you only need to know the bound variable's names.

import Data.Proxy (Proxy (..))
import Vehicle.Compile.Context.Bound.Class
import Vehicle.Compile.Context.Bound.Core
import Vehicle.Compile.Context.Name.Core as Core
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print

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
