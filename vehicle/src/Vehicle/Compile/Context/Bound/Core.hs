module Vehicle.Compile.Context.Bound.Core where

import Data.Coerce (coerce)
import Vehicle.Data.DeBruijn
import Vehicle.Prelude
import Vehicle.Syntax.AST (Binder, Name, nameOf)

--------------------------------------------------------------------------------
-- Bound context

-- | The binders of the variables that are in currently in scope, indexed into
-- via De Bruijn expressions.
-- Therefore the variables at the start of the list are the most
-- recent variables introduced to the scope.
-- Unlike a `BoundCtx`, it can store arbitrary generic data instead of
-- expressions.
type GenericBoundCtx a = [a]

lookupIx :: GenericBoundCtx b -> Ix -> Maybe b
lookupIx ctx i = ctx !!? coerce i

lookupLv :: GenericBoundCtx b -> Lv -> Maybe b
lookupLv ctx l = lookupIx ctx (dbLevelToIndex (Lv $ length ctx) l)

boundCtxLv :: GenericBoundCtx b -> Lv
boundCtxLv = Lv . length

-- | The binders of the variables that are in currently in scope, indexed into
-- via De Bruijn expressions.
-- Therefore the variables at the start of the list are the most
-- recent variables introduced to the scope.
type BoundCtx builtin = GenericBoundCtx (Binder Ix builtin)

emptyBoundCtx :: BoundCtx builtin
emptyBoundCtx = mempty

toNamedBoundCtx :: BoundCtx builtin -> NamedBoundCtx
toNamedBoundCtx = fmap nameOf

class HasBoundCtx a builtin | a -> builtin where
  boundContextOf :: a -> BoundCtx builtin

namedBoundCtxOf :: (HasBoundCtx a builtin) => a -> NamedBoundCtx
namedBoundCtxOf = toNamedBoundCtx . boundContextOf

type NamedBoundCtx = GenericBoundCtx (Maybe Name)

emptyNamedCtx :: NamedBoundCtx
emptyNamedCtx = mempty
