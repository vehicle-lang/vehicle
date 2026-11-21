{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Compile.Type.Meta.Map
  ( MetaMap (..),
    singleton,
    lookup,
    map,
    mapMaybe,
    insert,
    insertWith,
    delete,
    filter,
    partition,
    keys,
    member,
    unions,
    adjust,
    toList,
    fromList,
    fromListWith,
  )
where

import Data.Bifunctor (Bifunctor (first))
import Data.Coerce (coerce)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Vehicle.Compile.Type.Meta.Set (MetaSet)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Data.Meta (MetaID (..))
import Prelude hiding (filter, lookup, map)

--------------------------------------------------------------------------------
-- Meta substitution

newtype MetaMap a = MetaMap (IntMap a)
  deriving stock (Show, Traversable)
  deriving newtype (Semigroup, Monoid, Foldable, Functor)

singleton :: MetaID -> a -> MetaMap a
singleton m e = coerce (IntMap.singleton (coerce m) e)

lookup :: MetaID -> MetaMap a -> Maybe a
lookup m s = IntMap.lookup (coerce m) (coerce s)

map :: (a -> b) -> MetaMap a -> MetaMap b
map f = coerce . IntMap.map f . coerce

mapMaybe :: (a -> Maybe b) -> MetaMap a -> MetaMap b
mapMaybe f = coerce . IntMap.mapMaybe f . coerce

adjust :: (a -> a) -> MetaID -> MetaMap a -> MetaMap a
adjust f x = coerce . IntMap.adjust f (coerce x) . coerce

filter :: (a -> Bool) -> MetaMap a -> MetaMap a
filter f = coerce . IntMap.filter f . coerce

partition :: (a -> Bool) -> MetaMap a -> (MetaMap a, MetaMap a)
partition f = coerce . IntMap.partition f . coerce

insert :: MetaID -> a -> MetaMap a -> MetaMap a
insert m e s = coerce (IntMap.insert (coerce m) e (coerce s))

insertWith ::
  (a -> a -> a) ->
  MetaID ->
  a ->
  MetaMap a ->
  MetaMap a
insertWith f m e s = coerce (IntMap.insertWith f (coerce m) e (coerce s))

delete :: MetaID -> MetaMap a -> MetaMap a
delete (MetaID m) (MetaMap s) = MetaMap (IntMap.delete m s)

unions :: [MetaMap a] -> MetaMap a
unions xs = MetaMap (IntMap.unions (fmap coerce xs))

member :: MetaID -> MetaMap a -> Bool
member m (MetaMap xs) = IntMap.member (coerce m) xs

keys :: MetaMap a -> MetaSet
keys (MetaMap s) = MetaSet.fromIntSet $ IntMap.keysSet s

toList :: MetaMap a -> [(MetaID, a)]
toList (MetaMap xs) = fmap (first MetaID) (IntMap.toList xs)

fromList :: [(MetaID, a)] -> MetaMap a
fromList = MetaMap . IntMap.fromList . coerce

fromListWith :: (a -> a -> a) -> [(MetaID, a)] -> MetaMap a
fromListWith f = MetaMap . IntMap.fromListWith f . coerce
