module Vehicle.Data.Variable.Bound.Context.Name.Core where

import Vehicle.Data.Variable.Bound.Context.Core (GenericBoundCtx)
import Vehicle.Prelude

type NamedBoundCtx = GenericBoundCtx (Maybe Name)

emptyNamedCtx :: NamedBoundCtx
emptyNamedCtx = mempty

prettyNamedBoundCtx :: NamedBoundCtx -> Doc a
prettyNamedBoundCtx = prettyFlatList . fmap (maybe "_" pretty)

-- | A context where every bound variable is guaranteed to be assigned a name.
type CompleteNamedBoundCtx = GenericBoundCtx Name
