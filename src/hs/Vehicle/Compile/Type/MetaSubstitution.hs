{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Compile.Type.MetaSubstitution
  ( MetaSubstitution(..)
  , singleton
  , lookup
  , map
  , insertWith
  ) where

import Prelude hiding (map, lookup)
import Data.Coerce (coerce)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap

import Vehicle.Compile.Prelude

newtype MetaSubstitution = MetaSubstitution (IntMap CheckedExpr)
  deriving (Show, Semigroup, Monoid)

singleton :: Meta -> CheckedExpr -> MetaSubstitution
singleton m e = coerce (IntMap.singleton (coerce m) e)

lookup :: Meta -> MetaSubstitution -> Maybe CheckedExpr
lookup m s = IntMap.lookup (coerce m) (coerce s)

map :: (CheckedExpr -> CheckedExpr) -> MetaSubstitution -> MetaSubstitution
map f = coerce. IntMap.map f . coerce

insertWith :: (CheckedExpr -> CheckedExpr -> CheckedExpr)
           -> Meta
           -> CheckedExpr
           -> MetaSubstitution
           -> MetaSubstitution
insertWith f m e s = coerce (IntMap.insertWith f (coerce m) e (coerce s))
