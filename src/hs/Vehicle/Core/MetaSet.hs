{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Core.MetaSet
  ( MetaSet
  , toList
  , singleton
  , null
  , disjoint
  ) where

import Prelude hiding (null)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Coerce (coerce)

import Vehicle.Core.AST

newtype MetaSet = MetaSet IntSet
  deriving (Semigroup, Monoid)

toList :: MetaSet -> [Meta]
toList = coerce . IntSet.toList . coerce

singleton :: Meta -> MetaSet
singleton = coerce . IntSet.singleton . coerce

null :: MetaSet -> Bool
null = coerce . IntSet.null . coerce

disjoint :: MetaSet -> MetaSet -> Bool
disjoint = coerce . IntSet.disjoint . coerce