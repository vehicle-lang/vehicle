module Vehicle.Compile.Constants.TensorValue.Core where

import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Real (ExtendedRational)

-----------------------------------------------------------------------------
-- Dimensioned values

-- | Represents a tensor constant expression. We store the coefficient
-- separately so that we can normalise out coefficient operations. Therefore
-- represents the value:
--
--   tensorCoefficient * tensorValue
--
-- If `tensorValue` is not provided then it is implicitly equal to `1`.
--
-- Because there are no dependent types in Haskell, we cannot create
-- type-classes over tensor values with a given dimension. Hence we need
-- to wrap them in this ugly type-class that stores the dimensions internally.
data TensorConstantValue = TensorConstantValue
  { tensorValueDims :: UnforcedDims Builtin,
    tensorCoefficient :: ExtendedRational,
    tensorValue :: Maybe (Thunk Builtin)
  }
  deriving (Show, Eq, Ord)
