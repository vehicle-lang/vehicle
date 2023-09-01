module Vehicle.Compile.Context.Free.Core where

import Data.Map (Map)
import Vehicle.Expr.Normalised
import Vehicle.Syntax.AST

-- | Stores information associated with the declarations that are currently in
-- scope, indexed into via their names.
type GenericFreeCtx a = Map Identifier a

type FreeCtx builtin = GenericFreeCtx (GluedDecl builtin)