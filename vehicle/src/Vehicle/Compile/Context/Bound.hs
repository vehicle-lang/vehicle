module Vehicle.Compile.Context.Bound
  ( module X,
    prettyFriendlyInCtx,
  )
where

import Data.Proxy (Proxy)
import Vehicle.Compile.Context.Bound.Class as X
import Vehicle.Compile.Context.Bound.Core as X
import Vehicle.Compile.Context.Bound.Instance as X
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (PrettyFriendly, prettyFriendly)

prettyFriendlyInCtx ::
  (MonadBoundContext expr m, MonadLogger m, PrettyFriendly (Contextualised a NamedBoundCtx)) =>
  Proxy expr ->
  a ->
  m (Doc b)
prettyFriendlyInCtx p value = do
  ctx <- getNamedBoundCtx p
  return $ prettyFriendly (WithContext value ctx)
