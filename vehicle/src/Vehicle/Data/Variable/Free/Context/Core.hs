module Vehicle.Data.Variable.Free.Context.Core where

import Data.Map (Map)
import Data.Map qualified as Map (lookup)
import GHC.Stack (HasCallStack)
import Vehicle.Data.Code.Value
import Vehicle.Prelude

-- | Stores information associated with the declarations that are currently in
-- scope, indexed into via their names.
type GenericFreeCtx a = Map Identifier a

type FreeCtxEntry builtin =
  ( VDecl builtin
  )

type FreeCtx builtin = GenericFreeCtx (FreeCtxEntry builtin)

-- | Looks up the declaration associated the provided `Identifier`, throwing
-- an error if that identifier is out of scope.
lookupInFreeCtx :: (HasCallStack) => Identifier -> GenericFreeCtx a -> a
lookupInFreeCtx ident ctx = case Map.lookup ident ctx of
  Nothing -> internalScopingError $ pretty ident
  Just x -> x

-- | Looks up the declaration associated the provided `Identifier`, throwing
-- an error if that identifier is out of scope.
lookupInFreeCtxMaybe :: (HasCallStack) => Identifier -> GenericFreeCtx a -> Maybe a
lookupInFreeCtxMaybe = Map.lookup
