-- | This module exports the datatype representations of the core builtin symbols.
module Vehicle.Data.Builtin.Core.BasicOperations where

import Control.DeepSeq (NFData (..))
import Data.Aeson (ToJSON)
import Data.Hashable (Hashable (..))
import Data.Serialize (Serialize)
import Data.Serialize.Text ()
import Data.Text (Text)
import GHC.Generics (Generic)
import Prettyprinter (Doc, Pretty (..))

--------------------------------------------------------------------------------
-- Function positions

-- | Represents whether something is an input or an output of a function
data FunctionPosition
  = FunctionInput Text Int
  | FunctionOutput Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Hashable, Serialize)

instance Pretty FunctionPosition where
  pretty = \case
    FunctionInput n i -> "Input[" <> pretty n <> "][" <> pretty i <> "]"
    FunctionOutput n -> "Output[" <> pretty n <> "]"

--------------------------------------------------------------------------------
-- Comparisons

data ComparisonOp
  = Le
  | Lt
  | Ge
  | Gt
  | Eq
  | Ne
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Hashable, Serialize)

instance Pretty ComparisonOp where
  pretty = \case
    Le -> "<="
    Lt -> "<"
    Ge -> ">="
    Gt -> ">"
    Eq -> "=="
    Ne -> "!="

comparisonOp :: (Ord a) => ComparisonOp -> (a -> a -> Bool)
comparisonOp Le = (<=)
comparisonOp Lt = (<)
comparisonOp Ge = (>=)
comparisonOp Gt = (>)
comparisonOp Eq = (==)
comparisonOp Ne = (/=)

comparisonOpName :: ComparisonOp -> Doc a
comparisonOpName = \case
  Le -> "le"
  Lt -> "lt"
  Ge -> "ge"
  Gt -> "gt"
  Eq -> "eq"
  Ne -> "ne"

isStrict :: ComparisonOp -> Bool
isStrict order = order == Lt || order == Gt

isForward :: ComparisonOp -> Bool
isForward order = order == Lt || order == Le

flipStrictness :: ComparisonOp -> ComparisonOp
flipStrictness = \case
  Le -> Lt
  Lt -> Le
  Ge -> Gt
  Gt -> Ge
  Eq -> Eq
  Ne -> Ne

flipOrder :: ComparisonOp -> ComparisonOp
flipOrder = \case
  Le -> Ge
  Lt -> Gt
  Ge -> Le
  Gt -> Lt
  Eq -> Eq
  Ne -> Ne

isOrder :: ComparisonOp -> Bool
isOrder op = not (op == Eq || op == Ne)

chainable :: ComparisonOp -> ComparisonOp -> Bool
chainable e1 e2 = (e1 == e2 || e1 == flipStrictness e2) && e1 /= Ne

--------------------------------------------------------------------------------
-- Quantifiers

data Quantifier
  = Forall
  | Exists
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (NFData, Hashable, ToJSON, Serialize)

instance Pretty Quantifier where
  pretty = \case
    Forall -> "forall"
    Exists -> "exists"

--------------------------------------------------------------------------------
-- Domains

data NegDomain
  = NegRatTensor
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Hashable, Serialize)

instance Pretty NegDomain where
  pretty = \case
    NegRatTensor -> "RatTensor"

data AddDomain
  = AddNat
  | AddRatTensor
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Hashable, Serialize)

instance Pretty AddDomain where
  pretty = \case
    AddNat -> "Nat"
    AddRatTensor -> "RatTensor"

data SubDomain
  = SubRatTensor
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Hashable, Serialize)

instance Pretty SubDomain where
  pretty = \case
    SubRatTensor -> "RatTensor"

data MulDomain
  = MulNat
  | MulRatTensor
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Hashable, Serialize)

instance Pretty MulDomain where
  pretty = \case
    MulNat -> "Nat"
    MulRatTensor -> "RatTensor"

data DivDomain
  = DivRatTensor
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Hashable, Serialize)

instance Pretty DivDomain where
  pretty = \case
    DivRatTensor -> "RatTensor"

data PowDomain
  = PowRatTensor
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Hashable, Serialize)

instance Pretty PowDomain where
  pretty = \case
    PowRatTensor -> "RatTensor"

data LogDomain
  = LogRatTensor
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Hashable, Serialize)

instance Pretty LogDomain where
  pretty = \case
    LogRatTensor -> "RatTensor"

data ExpDomain
  = ExpRatTensor
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Hashable, Serialize)

instance Pretty ExpDomain where
  pretty = \case
    ExpRatTensor -> "RatTensor"

data MinDomain
  = MinRatTensor
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Hashable, Serialize)

instance Pretty MinDomain where
  pretty = \case
    MinRatTensor -> "RatTensor"

data MaxDomain
  = MaxRatTensor
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Hashable, Serialize)

instance Pretty MaxDomain where
  pretty = \case
    MaxRatTensor -> "RatTensor"

data FromRatDomain
  = FromRatToRat
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Hashable, Serialize)

instance Pretty FromRatDomain where
  pretty = \case
    FromRatToRat -> "Rat"

data FromNatDomain
  = -- This is actually needed as it takes an empty type-class parameter (see typing module)
    FromNatToNat
  | FromNatToIndex
  | FromNatToRat
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Serialize, NFData, Hashable)

instance Pretty FromNatDomain where
  pretty = \case
    FromNatToNat -> "Nat"
    FromNatToIndex -> "Index"
    FromNatToRat -> "Rat"

{-
--------------------------------------------------------------------------------
-- Tensor element types

data TensorElementType
  = BoolElementType
  | IndexElementType
  | NatElementType
  | RatElementType
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NFData, Hashable, Serialize)

instance Pretty TensorElementType where
  pretty = \case
    BoolElementType -> "Bool"
    NatElementType -> "Bool"
    IndexElementType -> "Bool"
    RatElementType -> "Rat"
-}
