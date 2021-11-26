{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Compile.Type.MetaSet
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

import Vehicle.Prelude
import Vehicle.Language.AST

newtype MetaSet = MetaSet IntSet
  deriving (Semigroup, Monoid)

instance Pretty MetaSet where
  pretty = pretty . toList

toList :: MetaSet -> [Meta]
toList = coerce . IntSet.toList . coerce

singleton :: Meta -> MetaSet
singleton = coerce . IntSet.singleton . coerce

null :: MetaSet -> Bool
null = coerce . IntSet.null . coerce

disjoint :: MetaSet -> MetaSet -> Bool
disjoint = coerce . IntSet.disjoint . coerce