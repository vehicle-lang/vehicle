{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Vehicle.Core.AST.Constraint where

import Data.Set (Set)
import Vehicle.Prelude (Provenance, HasProvenance(..))

--------------------------------------------------------------------------------
-- Definitions

data ConstraintType
  -- |Supports truth values (i.e. either Prop or Bool)
  = Truthy
  -- |Supports equality tests
  | Distinguishable
  -- |Supports comparison tests
  | Comparable
  -- |Supports indexed lookup
  | Indexable
  -- |Supports existential and universal quantification
  | Quantifiable
  | HasMul
  | HasDiv
  | HasAdd
  | HasSub
  | HasNeg
  deriving (Eq, Ord, Show)

data Constraint = Constraint Provenance ConstraintType
  deriving (Eq, Ord, Show)

type Constraints = Set Constraint

--------------------------------------------------------------------------------
-- Instances

instance HasProvenance Constraint where
  prov (Constraint p _) = p
