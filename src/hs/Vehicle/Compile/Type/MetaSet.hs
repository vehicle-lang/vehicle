{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Compile.Type.MetaSet
  ( MetaSet
  , toList
  , fromList
  , fromIntSet
  , singleton
  , member
  , difference
  , null
  , disjoint
  , unions
  , isSubsetOf
  ) where

import Prelude hiding (null)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Coerce (coerce)

import Vehicle.Prelude
import Vehicle.Language.AST

newtype MetaSet = MetaSet IntSet
  deriving (Show, Eq, Semigroup, Monoid)

instance Pretty MetaSet where
  pretty = pretty . toList

member :: Meta -> MetaSet -> Bool
member m ms = coerce m `IntSet.member` coerce ms

toList :: MetaSet -> [Meta]
toList = coerce . IntSet.toList . coerce

fromList :: [Meta] -> MetaSet
fromList = coerce . IntSet.fromList . coerce

fromIntSet :: IntSet -> MetaSet
fromIntSet = MetaSet

singleton :: Meta -> MetaSet
singleton = coerce . IntSet.singleton . coerce

null :: MetaSet -> Bool
null = coerce . IntSet.null . coerce

disjoint :: MetaSet -> MetaSet -> Bool
disjoint = coerce . IntSet.disjoint . coerce

difference :: MetaSet -> MetaSet -> MetaSet
difference s t = coerce (IntSet.difference (coerce s) (coerce t))

unions :: [MetaSet] -> MetaSet
unions s = coerce (IntSet.unions (coerce <$> s))

isSubsetOf :: MetaSet -> MetaSet -> Bool
isSubsetOf t1 t2 = IntSet.isSubsetOf (coerce t1) (coerce t2)