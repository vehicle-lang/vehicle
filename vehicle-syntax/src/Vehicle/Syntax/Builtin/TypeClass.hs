module Vehicle.Syntax.Builtin.TypeClass where

import Control.DeepSeq (NFData (..))
import Data.Hashable (Hashable (..))
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..))
import Vehicle.Syntax.AST.Decl (ParameterSort)
import Vehicle.Syntax.Builtin.BasicOperations

--------------------------------------------------------------------------------
-- Type classes

data TypeClass
  = -- Operation type-classes
    HasCompare ComparisonOp
  | HasQuantifier Quantifier
  | HasAdd
  | HasSub
  | HasMul
  | HasDiv
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
  | ValidNetworkType
  | ValidNetworkTensorType
  | ValidDatasetType
  | ValidDatasetListElementType
  | ValidDatasetTensorElementType
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
    HasAdd -> "HasAdd"
    HasSub -> "HasSub"
    HasMul -> "HasMul"
    HasDiv -> "HasDiv"
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
    ValidNetworkType -> "ValidNetworkType"
    ValidNetworkTensorType -> "ValidNetworkTensorType"
    ValidDatasetType -> "ValidDatasetType"
    ValidDatasetListElementType -> "ValidDatasetListElementType"
    ValidDatasetTensorElementType -> "ValidDatasetTensorElementType"

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
  | AddTC
  | SubTC
  | MulTC
  | DivTC
  | CompareTC ComparisonOp
  | AtTC
  | MapTC
  | FoldTC
  | ForeachTC
  | QuantifierTC Quantifier
  | TensorTypeTC
  deriving (Eq, Ord, Generic, Show)

instance NFData TypeClassOp

instance Hashable TypeClassOp

instance Serialize TypeClassOp

instance Pretty TypeClassOp where
  pretty = \case
    NegTC -> "-"
    AddTC -> "+"
    SubTC -> "-"
    MulTC -> "*"
    DivTC -> "/"
    FromNatTC -> "fromNat"
    FromRatTC -> "fromRat"
    VecLiteralTC {} -> "vec"
    CompareTC op -> pretty op
    AtTC -> "!"
    MapTC -> "map"
    FoldTC -> "fold"
    ForeachTC -> "foreach"
    QuantifierTC q -> pretty q
    TensorTypeTC -> "TensorTC"
