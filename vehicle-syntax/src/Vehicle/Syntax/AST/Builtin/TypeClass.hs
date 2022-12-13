{-# LANGUAGE CPP #-}


module Vehicle.Syntax.AST.Builtin.TypeClass where

import Control.DeepSeq (NFData (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (Hashable (..))
import GHC.Generics (Generic)
import Prettyprinter (Doc, Pretty (..), (<+>))
import Vehicle.Syntax.AST.Builtin.Core (EqualityOp, OrderOp, Quantifier)
import Vehicle.Syntax.AST.Builtin.Linearity (LinearityTypeClass)
import Vehicle.Syntax.AST.Builtin.Polarity (PolarityTypeClass)

#if nothunks
import NoThunks.Class (NoThunks)
#endif

--------------------------------------------------------------------------------
-- Type classes

data TypeClass
  = -- Operation type-classes
    HasEq !EqualityOp
  | HasOrd !OrderOp
  | HasNot
  | HasAnd
  | HasOr
  | HasImplies
  | HasQuantifier !Quantifier
  | HasAdd
  | HasSub
  | HasMul
  | HasDiv
  | HasNeg
  | HasFold
  | HasIf
  | HasQuantifierIn !Quantifier
  | -- Literal type-classes

    -- | The parameter is the value (needed for Index).
    HasNatLits !Int
  | HasRatLits
  | -- | The parameter is the size of the vector.
    HasVecLits !Int
  | -- Utility constraints

    -- | Types are equal, modulo the auxiliary constraints.
    AlmostEqualConstraint
  | NatInDomainConstraint !Int
  | ----------------------------
    -- Synthetic type-classes --
    ----------------------------

    LinearityTypeClass !LinearityTypeClass
  | PolarityTypeClass !PolarityTypeClass
  -- Linearity type-classes

  deriving (Eq, Generic, Show)

#if nothunks
instance NoThunks TypeClass
#endif
instance NFData TypeClass

instance Hashable TypeClass

instance ToJSON TypeClass

instance FromJSON TypeClass

instance Pretty TypeClass where
  pretty :: TypeClass -> Doc ann
  pretty = \case
    HasEq {}                 -> "HasEq"
    HasOrd {}                -> "HasOrd"
    HasNot                   -> "HasNot"
    HasAnd                   -> "HasAnd"
    HasOr                    -> "HasOr"
    HasImplies               -> "HasImplies"
    HasQuantifier q          -> "HasQuantifier" <+> pretty q
    HasAdd                   -> "HasAdd"
    HasSub                   -> "HasSub"
    HasMul                   -> "HasMul"
    HasDiv                   -> "HasDiv"
    HasNeg                   -> "HasNeg"
    HasFold                  -> "HasFold"
    HasQuantifierIn q        -> "HasQuantifierIn" <+> pretty q
    HasIf                    -> "HasIf"
    HasNatLits n             -> "HasNatLiterals[" <> pretty n <> "]"
    HasRatLits               -> "HasRatLiterals"
    HasVecLits n             -> "HasVecLiterals[" <> pretty n <> "]"
    AlmostEqualConstraint {} -> "AlmostEqualConstraint"
    NatInDomainConstraint {} -> "NatInDomainConstraint"
    LinearityTypeClass tc    -> pretty tc
    PolarityTypeClass tc     -> pretty tc

-- Builtin operations for type-classes
data TypeClassOp
  = NotTC
  | AndTC
  | OrTC
  | ImpliesTC
  | FromNatTC !Int
  | FromRatTC
  | FromVecTC !Int
  | NegTC
  | AddTC
  | SubTC
  | MulTC
  | DivTC
  | EqualsTC !EqualityOp
  | OrderTC !OrderOp
  | MapTC
  | FoldTC
  | QuantifierTC !Quantifier
  | QuantifierInTC !Quantifier
  deriving (Eq, Generic, Show)

#if nothunks
instance NoThunks TypeClassOp
#endif
instance NFData TypeClassOp

instance Hashable TypeClassOp

instance ToJSON TypeClassOp

instance FromJSON TypeClassOp

instance Pretty TypeClassOp where
  pretty :: TypeClassOp -> Doc ann
  pretty = \case
    NotTC            -> "not"
    AndTC            -> "and"
    OrTC             -> "or"
    ImpliesTC        -> "=>"
    NegTC            -> "-"
    AddTC            -> "+"
    SubTC            -> "-"
    MulTC            -> "*"
    DivTC            -> "/"
    FromNatTC n      -> "fromNat[" <> pretty n <> "]"
    FromRatTC        -> "fromRat"
    FromVecTC n      -> "fromVec[" <> pretty n <> "]"
    EqualsTC op      -> pretty op
    OrderTC op       -> pretty op
    MapTC            -> "map"
    FoldTC           -> "fold"
    QuantifierTC q   -> pretty q
    QuantifierInTC q -> pretty q <> "In"
