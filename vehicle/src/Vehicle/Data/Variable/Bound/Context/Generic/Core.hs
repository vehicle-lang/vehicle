module Vehicle.Data.Variable.Bound.Context.Generic.Core where

import Vehicle.Data.Variable.Bound.Context.Core
import Vehicle.Data.Variable.Bound.Context.Name.Core
import Vehicle.Prelude

-- | The binders of the variables that are in currently in scope, indexed into
-- via De Bruijn expressions.
-- Therefore the variables at the start of the list are the most
-- recent variables introduced to the scope.
type BoundCtx expr = GenericBoundCtx (GenericBinder expr)

emptyBoundCtx :: BoundCtx expr
emptyBoundCtx = mempty

class HasBoundCtx a expr | a -> expr where
  boundContextOf :: a -> BoundCtx expr

toNamedBoundCtx :: BoundCtx expr -> NamedBoundCtx
toNamedBoundCtx = fmap nameOf

namedBoundCtxOf :: (HasBoundCtx a builtin) => a -> NamedBoundCtx
namedBoundCtxOf = toNamedBoundCtx . boundContextOf
