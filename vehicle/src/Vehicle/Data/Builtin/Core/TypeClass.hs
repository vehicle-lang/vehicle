module Vehicle.Data.Builtin.Core.TypeClass where

import Control.DeepSeq (NFData (..))
import Data.Hashable (Hashable (..))
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..))
import Vehicle.Data.AST.Decl (ParameterSort)
import Vehicle.Data.Builtin.Core.BasicOperations

--------------------------------------------------------------------------------
-- Type classes

data TypeClass
  = -- Operation type-classes
    HasCompare ComparisonOp
  | HasQuantifier Quantifier
  | HasNeg
  | HasFold
  | HasMap
  | HasQuantifierIn Quantifier
  | HasAt
  | HasForeach
  | -- Literal type-classes
    HasNatLits
  | HasRatLits
  | HasVecLits
  | -- Overloading of the tensor type
    IsTensorType
  | -- Declaration type restrictions
    ValidPropertyType
  | ValidParameterType ParameterSort
  | ValidNetworkTensorType
  | ValidTensorLikeType
  deriving (Eq, Ord, Generic, Show)

instance NFData TypeClass

instance Hashable TypeClass

instance Serialize TypeClass

instance Pretty TypeClass where
  pretty = \case
    HasCompare {} -> "HasComparison"
    HasQuantifier Forall -> "HasForall"
    HasQuantifier Exists -> "HasExists"
    HasQuantifierIn Forall -> "HasForallIn"
    HasQuantifierIn Exists -> "HasExistsIn"
    HasNeg -> "HasNeg"
    HasMap -> "HasMap"
    HasFold -> "HasFold"
    HasNatLits -> "HasNatLiterals"
    HasRatLits -> "HasRatLiterals"
    HasVecLits -> "HasVecLiterals"
    HasAt -> "HasAt"
    HasForeach -> "HasForeach"
    IsTensorType -> "IsTensorType"
    ValidPropertyType -> "ValidPropertyType"
    ValidParameterType {} -> "ValidParameterType"
    ValidNetworkTensorType -> "ValidNetworkTensorType"
    ValidTensorLikeType -> "ValidTensorLikeType"

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
  | NegTC
  | CompareTC ComparisonOp
  | AtTC
  | MapTC
  | FoldTC
  | ForeachTC
  | TensorTypeTC
  deriving (Eq, Ord, Generic, Show)

instance NFData TypeClassOp

instance Hashable TypeClassOp

instance Serialize TypeClassOp

instance Pretty TypeClassOp where
  pretty = \case
    NegTC -> "-"
    FromNatTC -> "fromNat"
    FromRatTC -> "fromRat"
    VecLiteralTC {} -> "vec"
    CompareTC op -> pretty op
    AtTC -> "!"
    MapTC -> "map"
    FoldTC -> "fold"
    ForeachTC -> "foreach"
    TensorTypeTC -> "TensorTC"
