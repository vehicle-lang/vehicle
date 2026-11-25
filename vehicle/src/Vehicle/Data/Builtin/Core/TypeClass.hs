module Vehicle.Data.Builtin.Core.TypeClass where

import Control.DeepSeq (NFData (..))
import Data.Hashable (Hashable (..))
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..))

--------------------------------------------------------------------------------
-- Type classes

data TypeClass
  = -- Literal type-classes
    HasNatLits
  | HasRatLits
  | HasVecLits
  | -- Overloading of the tensor type
    IsTensorType
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NFData, Hashable, Serialize)

instance Pretty TypeClass where
  pretty = \case
    HasNatLits -> "HasNatLiterals"
    HasRatLits -> "HasRatLiterals"
    HasVecLits -> "HasVecLiterals"
    IsTensorType -> "IsTensorType"

-- Builtin operations for type-classes
data TypeClassOp
  = -- | Needed to overload `Bool`/`Rat` as both `BoolElementType` in `Tensor Bool dims` and as `Tensor Bool []` in `Bool`
    FromNatTC
  | FromRatTC
  | -- Note we need to have `FromNat` and `FromRat` as actual functions as the
    -- `fromNat` requires us to inspect the actual value being cast in the type-checker
    -- when casting to `Index`. No such restriction applies to vector literals so we can
    -- have it as a literal in the type-class.
    VecLiteralTC
  | TensorTypeTC
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NFData, Hashable, Serialize)

instance Pretty TypeClassOp where
  pretty = \case
    FromNatTC -> "fromNatTC"
    FromRatTC -> "fromRatTC"
    VecLiteralTC {} -> "vecTC"
    TensorTypeTC -> "TensorTC"
