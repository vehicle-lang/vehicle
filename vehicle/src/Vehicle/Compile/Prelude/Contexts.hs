module Vehicle.Compile.Prelude.Contexts where

import Control.Monad.Reader (MonadReader (..))
import Data.Coerce (coerce)
import Data.Map (Map)
import Vehicle.Expr.DeBruijn
import Vehicle.Prelude ((!!?))
import Vehicle.Syntax.AST

--------------------------------------------------------------------------------
-- Bound variable context

-- | The names, types and values if known of the variables that are in
-- currently in scope, indexed into via De Bruijn expressions.
type BoundCtx a = [a]

type BoundDBCtx = BoundCtx (Maybe Name)

emptyDBCtx :: BoundDBCtx
emptyDBCtx = mempty

class HasBoundCtx a where
  boundContextOf :: a -> BoundDBCtx

instance HasBoundCtx (BoundCtx (Maybe Name)) where
  boundContextOf = id

instance HasBoundCtx (BoundCtx Name) where
  boundContextOf = map Just

addToBoundCtx :: (MonadReader (BoundCtx b) m) => b -> m c -> m c
addToBoundCtx v = local (v :)

getBoundCtx :: (MonadReader (BoundCtx b) m) => m (BoundCtx b)
getBoundCtx = ask

lookupVar :: BoundCtx b -> Ix -> Maybe b
lookupVar ctx i = ctx !!? coerce i

--------------------------------------------------------------------------------
-- Declaration contexts

-- | Stores information associated with the declarations that are currently in
-- scope, indexed into via their names.
type DeclCtx a = Map Identifier a
