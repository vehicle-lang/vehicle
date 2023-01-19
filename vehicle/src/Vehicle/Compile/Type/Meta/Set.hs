{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Compile.Type.Meta.Set
  ( MetaSet,
    toList,
    fromList,
    fromIntSet,
    singleton,
    member,
    insert,
    difference,
    null,
    disjoint,
    unions,
    isSubsetOf,
  )
where

import Data.Coerce (coerce)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Vehicle.Prelude
import Vehicle.Syntax.AST
import Prelude hiding (null)

newtype MetaSet = MetaSet IntSet
  deriving (Show, Eq, Semigroup, Monoid)

instance Pretty MetaSet where
  pretty = pretty . toList

member :: MetaID -> MetaSet -> Bool
member m ms = coerce m `IntSet.member` coerce ms

toList :: MetaSet -> [MetaID]
toList = coerce . IntSet.toList . coerce

fromList :: [MetaID] -> MetaSet
fromList = coerce . IntSet.fromList . coerce

fromIntSet :: IntSet -> MetaSet
fromIntSet = MetaSet

singleton :: MetaID -> MetaSet
singleton = coerce . IntSet.singleton . coerce

null :: MetaSet -> Bool
null = coerce . IntSet.null . coerce

insert :: MetaID -> MetaSet -> MetaSet
insert = coerce . IntSet.insert . coerce

disjoint :: MetaSet -> MetaSet -> Bool
disjoint = coerce . IntSet.disjoint . coerce

difference :: MetaSet -> MetaSet -> MetaSet
difference s t = coerce (IntSet.difference (coerce s) (coerce t))

unions :: [MetaSet] -> MetaSet
unions s = coerce (IntSet.unions (coerce <$> s))

isSubsetOf :: MetaSet -> MetaSet -> Bool
isSubsetOf t1 t2 = IntSet.isSubsetOf (coerce t1) (coerce t2)
