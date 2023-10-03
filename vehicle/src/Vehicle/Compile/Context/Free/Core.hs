module Vehicle.Compile.Context.Free.Core where

import Data.Map (Map)
import Vehicle.Data.DeBruijn (Ix)
import Vehicle.Syntax.AST

-- | Stores information associated with the declarations that are currently in
-- scope, indexed into via their names.
type GenericFreeCtx a = Map Identifier a

type FreeCtx builtin = GenericFreeCtx (Decl Ix builtin)
