module Vehicle.Data.Builtin.Loss
  ( module Vehicle.Data.Builtin.Loss,
    DimensionTypeBuiltin (..),
    DimensionDataBuiltin (..),
    RatTensorBuiltin (..),
  )
where

import GHC.Generics (Generic)
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Builtin.Tensor
import Vehicle.Prelude (Pretty (..))

--------------------------------------------------------------------------------
-- Builtin datatype

-- | The builtin types after translation to loss functions (missing all builtins
-- that involve the Bool type).
data LossTensorBuiltin
  = LossTensorRat RatTensorBuiltin
  | LossTensorDimType DimensionTypeBuiltin
  | LossTensorDimData DimensionDataBuiltin
  deriving (Show, Eq, Generic)

instance Pretty LossTensorBuiltin where
  pretty = pretty . show

instance BuiltinHasDimensionData LossTensorBuiltin where
  getDimensionDataBuiltin e = case e of
    LossTensorDimData t -> Just t
    _ -> Nothing
  mkDimensionDataBuiltin = LossTensorDimData

instance BuiltinHasDimensionTypes LossTensorBuiltin where
  getDimensionTypeBuiltin e = case e of
    LossTensorDimType t -> Just t
    _ -> Nothing
  mkDimensionTypeBuiltin = LossTensorDimType

instance BuiltinHasRatTensor LossTensorBuiltin where
  getRatTensorBuiltin e = case e of
    LossTensorRat t -> Just t
    _ -> Nothing
  mkRatTensorBuiltin = LossTensorRat
