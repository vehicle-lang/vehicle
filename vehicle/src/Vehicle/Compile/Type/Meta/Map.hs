{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Compile.Type.Meta.Map
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
import Vehicle.Compile.Type.Meta.Set (MetaSet)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet

--------------------------------------------------------------------------------
-- Meta substitution

newtype MetaMap a = MetaMap (IntMap a)
  deriving (Show, Semigroup, Monoid, Foldable, Functor)

singleton :: MetaID -> a -> MetaMap a
singleton m e = coerce (IntMap.singleton (coerce m) e)

lookup :: MetaID -> MetaMap a -> Maybe a
lookup m s = IntMap.lookup (coerce m) (coerce s)

map :: (a -> a) -> MetaMap a -> MetaMap a
map f = coerce. IntMap.map f . coerce

insert :: MetaID -> a -> MetaMap a -> MetaMap a
insert m e s = coerce (IntMap.insert (coerce m) e (coerce s))

insertWith :: (a -> a -> a)
           -> MetaID
           -> a
           -> MetaMap a
           -> MetaMap a
insertWith f m e s = coerce (IntMap.insertWith f (coerce m) e (coerce s))

unions :: [MetaMap a] -> MetaMap a
unions xs = MetaMap (IntMap.unions (fmap coerce xs))

member :: MetaID -> MetaMap a -> Bool
member m (MetaMap xs) = IntMap.member (coerce m) xs

keys :: MetaMap a -> MetaSet
keys (MetaMap s) = MetaSet.fromIntSet $ IntMap.keysSet s

toList :: MetaMap a -> [(MetaID, a)]
toList (MetaMap xs) = fmap (first MetaID) (IntMap.toList xs)
