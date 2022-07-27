
module Vehicle.Compile.Prelude.Contexts where

import Control.Monad.Reader (MonadReader(..), asks)
import Data.Map (Map, insert)

import Vehicle.Prelude
import Vehicle.Language.AST

--------------------------------------------------------------------------------
-- Bound variable context

-- | The names, types and values if known of the variables that are in
-- currently in scope, indexed into via De Bruijn expressions.
type BoundCtx a = [a]

class HasBoundCtx a where
  boundContextOf :: a -> [DBBinding]

instance HasBoundCtx (BoundCtx DBBinding) where
  boundContextOf = id

instance HasBoundCtx (BoundCtx Symbol) where
  boundContextOf = map Just

--------------------------------------------------------------------------------
-- Declaration contexts

-- | Stores information associated with the declarations that are currently in
-- scope, indexed into via their names.
type DeclCtx a = Map Identifier a

--------------------------------------------------------------------------------
-- Variable contexts

-- | Combined context
data VariableCtx a b = VariableCtx
  { declCtx  :: DeclCtx  a
  , boundCtx :: BoundCtx b
  } deriving (Show)

emptyVariableCtx :: VariableCtx a b
emptyVariableCtx = VariableCtx mempty mempty

getVariableCtx :: MonadReader (VariableCtx a b) m => m (VariableCtx a b)
getVariableCtx = ask

getDeclCtx :: MonadReader (VariableCtx a b) m => m (DeclCtx a)
getDeclCtx = asks declCtx

getBoundCtx :: MonadReader (VariableCtx a b) m => m (BoundCtx b)
getBoundCtx = asks boundCtx

addToBoundCtx :: MonadReader (VariableCtx a b) m => b -> m c -> m c
addToBoundCtx v = local $ \VariableCtx{..} ->
  VariableCtx { boundCtx = v : boundCtx, ..}

addToDeclCtx :: MonadReader (VariableCtx a b) m => Identifier -> a -> m c -> m c
addToDeclCtx ident value = local $ \VariableCtx{..} ->
  VariableCtx { declCtx = insert ident value declCtx, ..}
