{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Compile.Type.MetaMap
  ( MetaMap(..)
  , singleton
  , lookup
  , map
  , insert
  , insertWith
  , keys
  , member
  , unions
  , toList
  ) where

import Data.Coerce (coerce)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Prelude hiding (lookup, map)

import Data.Bifunctor (Bifunctor (first))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.MetaSet (MetaSet)
import Vehicle.Compile.Type.MetaSet qualified as MetaSet

--------------------------------------------------------------------------------
-- Meta substitution

newtype MetaMap a = MetaMap (IntMap a)
  deriving (Show, Semigroup, Monoid, Foldable)

singleton :: Meta -> a -> MetaMap a
singleton m e = coerce (IntMap.singleton (coerce m) e)

lookup :: Meta -> MetaMap a -> Maybe a
lookup m s = IntMap.lookup (coerce m) (coerce s)

map :: (a -> a) -> MetaMap a -> MetaMap a
map f = coerce. IntMap.map f . coerce

insert :: Meta -> a -> MetaMap a -> MetaMap a
insert m e s = coerce (IntMap.insert (coerce m) e (coerce s))

insertWith :: (a -> a -> a)
           -> Meta
           -> a
           -> MetaMap a
           -> MetaMap a
insertWith f m e s = coerce (IntMap.insertWith f (coerce m) e (coerce s))

unions :: [MetaMap a] -> MetaMap a
unions xs = MetaMap (IntMap.unions (fmap coerce xs))

member :: Meta -> MetaMap a -> Bool
member m (MetaMap xs) = IntMap.member (coerce m) xs

keys :: MetaMap a -> MetaSet
keys (MetaMap s) = MetaSet.fromIntSet $ IntMap.keysSet s

toList :: MetaMap a -> [(Meta, a)]
toList (MetaMap xs) = fmap (first MetaVar) (IntMap.toList xs)
