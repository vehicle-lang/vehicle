module Vehicle.Compile.Context.Free.Core where

import Data.Map (Map)
import Vehicle.Data.Code.Expr (Decl)
import Vehicle.Data.Code.Value
import Vehicle.Prelude

-- | Stores information associated with the declarations that are currently in
-- scope, indexed into via their names.
type GenericFreeCtx a = Map Identifier a

type FreeCtxEntry builtin =
  ( Decl builtin,
    VDecl builtin
  )

type FreeCtx builtin = GenericFreeCtx (FreeCtxEntry builtin)
