module Vehicle.Compile.Context.Bound.Core where

import Data.Coerce (coerce)
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Vehicle.Data.DeBruijn
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Bound context

-- | The binders of the variables that are in currently in scope, indexed into
-- via De Bruijn expressions.
-- Therefore the variables at the start of the list are the most
-- recent variables introduced to the scope.
-- Unlike a `BoundCtx`, it can store arbitrary generic data instead of
-- expressions.
type GenericBoundCtx a = [a]

boundCtxLv :: GenericBoundCtx b -> Lv
boundCtxLv = Lv . length

-- | The binders of the variables that are in currently in scope, indexed into
-- via De Bruijn expressions.
-- Therefore the variables at the start of the list are the most
-- recent variables introduced to the scope.
type BoundCtx expr = GenericBoundCtx (GenericBinder expr)

emptyBoundCtx :: BoundCtx expr
emptyBoundCtx = mempty

toNamedBoundCtx :: BoundCtx expr -> NamedBoundCtx
toNamedBoundCtx = fmap nameOf

class HasBoundCtx a expr | a -> expr where
  boundContextOf :: a -> BoundCtx expr

namedBoundCtxOf :: (HasBoundCtx a builtin) => a -> NamedBoundCtx
namedBoundCtxOf = toNamedBoundCtx . boundContextOf

type NamedBoundCtx = GenericBoundCtx (Maybe Name)

emptyNamedCtx :: NamedBoundCtx
emptyNamedCtx = mempty

prettyNamedBoundCtx :: NamedBoundCtx -> Doc a
prettyNamedBoundCtx = prettyFlatList . fmap (maybe "_" pretty)

-- | A context where every bound variable is guaranteed to be assigned a name.
type CompleteNamedBoundCtx = GenericBoundCtx Name

--------------------------------------------------------------------------------
-- Lookup functions

lookupIx :: GenericBoundCtx b -> Ix -> Maybe b
lookupIx ctx i = ctx !!? coerce i

lookupLv :: GenericBoundCtx b -> Lv -> Maybe b
lookupLv ctx l = lookupIx ctx (dbLevelToIndex (Lv $ length ctx) l)

-- | Looks up the value associated with the variable given the provided `Lv`, throwing
-- an error if that level is out of scope.
lookupLvInBoundCtx :: (HasCallStack) => Lv -> GenericBoundCtx a -> a
lookupLvInBoundCtx lv ctx = case lookupLv ctx lv of
  Nothing -> do
    let pass = pretty $ prettyCallStack callStack
    outOfBoundsError pass ctx (dbLevelToIndex (Lv $ length ctx) lv)
  Just x -> x

-- | Looks up the value associated with the variable given the provided `Ix`, throwing
-- an error if that index is out of scope.
lookupIxInBoundCtx :: (HasCallStack) => Ix -> GenericBoundCtx a -> a
lookupIxInBoundCtx ix ctx = case lookupIx ctx ix of
  Nothing -> do
    let pass = pretty $ prettyCallStack callStack
    outOfBoundsError pass ctx ix
  Just x -> x

outOfBoundsError :: Doc () -> GenericBoundCtx a -> Ix -> b
outOfBoundsError pass ctx i =
  developerError $
    "Internal scoping error during"
      <+> pass
      <> ":"
        <+> "the bound context of length"
        <+> quotePretty (length ctx)
        <+> "is smaller than the found DB index"
        <+> pretty i
