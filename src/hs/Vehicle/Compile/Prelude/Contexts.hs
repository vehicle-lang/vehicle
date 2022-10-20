
module Vehicle.Compile.Prelude.Contexts where

import Control.Monad.Reader (MonadReader(..))
import Data.Map (Map)

import Vehicle.Prelude
import Vehicle.Language.AST

--------------------------------------------------------------------------------
-- Bound variable context

-- | The names, types and values if known of the variables that are in
-- currently in scope, indexed into via De Bruijn expressions.
type BoundCtx a    = [a]
type BoundDBCtx    = BoundCtx DBBinding
type NamedBoundCtx = BoundCtx NamedBinding

class HasBoundCtx a where
  boundContextOf :: a -> [DBBinding]

instance HasBoundCtx (BoundCtx DBBinding) where
  boundContextOf = id

instance HasBoundCtx (BoundCtx Symbol) where
  boundContextOf = map Just

addToBoundCtx :: MonadReader (BoundCtx b) m => b -> m c -> m c
addToBoundCtx v = local (v :)

getBoundCtx :: MonadReader (BoundCtx b) m => m (BoundCtx b)
getBoundCtx = ask

--------------------------------------------------------------------------------
-- Declaration contexts

-- | Stores information associated with the declarations that are currently in
-- scope, indexed into via their names.
type DeclCtx a = Map Identifier a
