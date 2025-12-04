module Vehicle.Syntax.Builtin.TypeClass where

import Control.DeepSeq (NFData (..))
import Data.Hashable (Hashable (..))
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..))
import Vehicle.Syntax.Builtin.BasicOperations (ComparisonOp (..))

--------------------------------------------------------------------------------
-- Type classes

data TypeClass
  = -- Operation type-classes
    HasComparisons
  | HasForall
  | HasExists
  | HasAdd
  | HasSub
  | HasMul
  | HasDiv
  | HasNeg
  | HasFold
  | HasMap
  | HasForallIn
  | HasExistsIn
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
  | ValidInferableParameterType
  | ValidNonInferableParameterType
  | ValidNetworkType
  | ValidNetworkTensorType
  | ValidDatasetType
  | ValidDatasetListElementType
  | ValidDatasetTensorElementType
  | ValidTensorLikeType
  deriving (Eq, Ord, Enum, Bounded, Generic, Show)

instance NFData TypeClass

instance Hashable TypeClass

instance Serialize TypeClass

instance Pretty TypeClass where
  pretty = \case
    HasComparisons -> "HasComparisons"
    HasForall -> "HasForall"
    HasExists -> "HasExists"
    HasForallIn -> "HasForallIn"
    HasExistsIn -> "HasExistsIn"
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
    ValidInferableParameterType -> "ValidInferableParameterType"
    ValidNonInferableParameterType -> "ValidNonInferableParameterType"
    ValidNetworkType -> "ValidNetworkType"
    ValidNetworkTensorType -> "ValidNetworkTensorType"
    ValidDatasetType -> "ValidDatasetType"
    ValidDatasetListElementType -> "ValidDatasetListElementType"
    ValidDatasetTensorElementType -> "ValidDatasetTensorElementType"
    ValidTensorLikeType -> "ValidTensorLikeType"

-- | Builtin operations for type-classes
--
-- Note we need to have `FromNat` and `FromRat` as actual functions as the
-- `fromNat` requires us to inspect the actual value being cast in the type-checker
-- when casting to `Index`. No such restriction applies to vector literals so we can
-- have it as a literal in the type-class.
data TypeClassOp
  = FromNatTC
  | FromRatTC
  | FromVecTC
  | NegTC
  | AddTC
  | SubTC
  | MulTC
  | DivTC
  | LeTC
  | LtTC
  | GeTC
  | GtTC
  | EqTC
  | NeTC
  | AtTC
  | MapTC
  | FoldTC
  | ForeachTC
  | ForallTC
  | ExistsTC
  | TensorTypeTC
  deriving (Eq, Ord, Enum, Bounded, Generic, Show)

instance NFData TypeClassOp

instance Hashable TypeClassOp

instance Serialize TypeClassOp

instance Pretty TypeClassOp where
  pretty = \case
    NegTC -> "negTC"
    AddTC -> "addTC"
    SubTC -> "subTC"
    MulTC -> "mulTC"
    DivTC -> "divTC"
    FromNatTC -> "fromNatTC"
    FromRatTC -> "fromRatTC"
    FromVecTC {} -> "fromVecTC"
    LeTC -> "leTC"
    LtTC -> "ltTC"
    GeTC -> "geTC"
    GtTC -> "gtTC"
    EqTC -> "eqTC"
    NeTC -> "neTC"
    AtTC -> "atTC"
    MapTC -> "mapTC"
    FoldTC -> "foldTC"
    ForeachTC -> "foreachTC"
    ForallTC -> "forallTC"
    ExistsTC -> "existsTC"
    TensorTypeTC -> "TensorTC"

opToTCOp :: ComparisonOp -> TypeClassOp
opToTCOp = \case
  Eq -> EqTC
  Ne -> NeTC
  Le -> LeTC
  Lt -> LtTC
  Ge -> GeTC
  Gt -> GtTC
