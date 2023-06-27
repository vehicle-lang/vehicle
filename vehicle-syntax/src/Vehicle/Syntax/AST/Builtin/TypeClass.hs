module Vehicle.Syntax.AST.Builtin.TypeClass where

import Control.DeepSeq (NFData (..))
import Data.Aeson (ToJSON)
import Data.Hashable (Hashable (..))
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..), (<+>))
import Vehicle.Syntax.AST.Builtin.Core

--------------------------------------------------------------------------------
-- Type classes

data TypeClass
  = -- Operation type-classes
    HasEq EqualityOp
  | HasOrd OrderOp
  | HasQuantifier Quantifier
  | HasAdd
  | HasSub
  | HasMul
  | HasDiv
  | HasNeg
  | HasFold
  | HasMap
  | HasQuantifierIn Quantifier
  | -- Literal type-classes

    HasNatLits
  | HasRatLits
  | HasVecLits
  | -- Utility constraints
    NatInDomainConstraint
  deriving (Eq, Ord, Generic, Show)

instance NFData TypeClass

instance Hashable TypeClass

instance ToJSON TypeClass

instance Serialize TypeClass

instance Pretty TypeClass where
  pretty = \case
    HasEq {} -> "HasEq"
    HasOrd {} -> "HasOrd"
    HasQuantifier q -> "HasQuantifier" <+> pretty q
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
    NatInDomainConstraint {} -> "NatInDomainConstraint"

-- Builtin operations for type-classes
data TypeClassOp
  = FromNatTC
  | FromRatTC
  | FromVecTC
  | NegTC
  | AddTC
  | SubTC
  | MulTC
  | DivTC
  | EqualsTC EqualityOp
  | OrderTC OrderOp
  | MapTC
  | FoldTC
  | QuantifierTC Quantifier
  deriving (Eq, Ord, Generic, Show)

instance NFData TypeClassOp

instance Hashable TypeClassOp

instance ToJSON TypeClassOp

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
    FromVecTC -> "fromVec"
    EqualsTC op -> pretty op
    OrderTC op -> pretty op
    MapTC -> "map"
    FoldTC -> "fold"
    QuantifierTC q -> pretty q

opOfTypeClass :: TypeClass -> TypeClassOp
opOfTypeClass = \case
  HasEq op -> EqualsTC op
  HasOrd op -> OrderTC op
  HasQuantifier q -> QuantifierTC q
  HasAdd -> AddTC
  HasSub -> SubTC
  HasMul -> MulTC
  HasDiv -> DivTC
  HasNeg -> NegTC
  HasFold -> FoldTC
  HasMap -> MapTC
  HasNatLits -> FromNatTC
  HasRatLits -> FromRatTC
  HasVecLits -> FromVecTC
  NatInDomainConstraint -> error "`NatInDomainConstraint` has no corresponding type class."
