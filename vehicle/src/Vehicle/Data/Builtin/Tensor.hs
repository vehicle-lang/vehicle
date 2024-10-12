module Vehicle.Data.Builtin.Tensor
  ( module Vehicle.Data.Builtin.Tensor,
    OrderOp (..),
    EqualityOp (..),
    RatTensorBuiltin (..),
    BoolTensorBuiltin (..),
    DimensionTypeBuiltin (..),
    DimensionDataBuiltin (..),
  )
where

import GHC.Generics (Generic)
import Prettyprinter (Pretty (..))
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Builtin.Interface

data TensorBuiltin
  = TensorRat RatTensorBuiltin
  | TensorBool BoolTensorBuiltin
  | TensorDimType DimensionTypeBuiltin
  | TensorDimData DimensionDataBuiltin
  deriving (Show, Eq, Generic)

instance Pretty TensorBuiltin where
  pretty = pretty . show

instance BuiltinHasDimensionData TensorBuiltin where
  getDimensionDataBuiltin e = case e of
    TensorDimData t -> Just t
    _ -> Nothing
  mkDimensionDataBuiltin = TensorDimData

instance BuiltinHasDimensionTypes TensorBuiltin where
  getDimensionTypeBuiltin e = case e of
    TensorDimType t -> Just t
    _ -> Nothing
  mkDimensionTypeBuiltin = TensorDimType

instance BuiltinHasRatTensor TensorBuiltin where
  getRatTensorBuiltin e = case e of
    TensorRat t -> Just t
    _ -> Nothing
  mkRatTensorBuiltin = TensorRat

instance BuiltinHasBoolTensor TensorBuiltin where
  getBoolTensorBuiltin e = case e of
    TensorBool t -> Just t
    _ -> Nothing
  mkBoolTensorBuiltin = TensorBool
