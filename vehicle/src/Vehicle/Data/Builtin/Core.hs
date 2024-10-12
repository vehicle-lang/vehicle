module Vehicle.Data.Builtin.Core
  ( module SyntaxBuiltins,
    module Vehicle.Data.Builtin.Core,
  )
where

import GHC.Generics (Generic)
import Vehicle.Data.Tensor (Tensor)
import Vehicle.Syntax.Builtin as SyntaxBuiltins

--------------------------------------------------------------------------------
-- Dimensions

data DimensionTypeBuiltin
  = DimensionType
  | DimensionsType
  | DimensionIndexType
  | TensorType
  deriving (Show, Eq, Generic)

data DimensionDataBuiltin
  = Dimension Int
  | DimensionNil
  | DimensionCons
  | DimensionIndex Int
  | DimensionLookup
  | DimensionIndexTensor (Tensor Int)
  | StackTensor Int
  | ConstTensor
  deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- Rational tensor builtins

data RatTensorBuiltin
  = RatTensor (Tensor Rational)
  | RatType
  | RatLiteral Rational
  | NegRatTensor
  | AddRatTensor
  | SubRatTensor
  | MulRatTensor
  | DivRatTensor
  | MinRatTensor
  | MaxRatTensor
  | ReduceAddRatTensor
  | ReduceMulRatTensor
  | ReduceMinRatTensor
  | ReduceMaxRatTensor
  | SearchRatTensor
  deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- Boolean tensor builtins

data BoolTensorBuiltin
  = BoolTensor (Tensor Bool)
  | BoolType
  | BoolLiteral Bool
  | AndBoolTensor
  | OrBoolTensor
  | NotBoolTensor
  | EqualsRatTensor EqualityOp
  | OrderRatTensor OrderOp
  | ReduceAndTensor
  | ReduceOrTensor
  | QuantifyRatTensor Quantifier
  deriving (Show, Eq, Generic)
