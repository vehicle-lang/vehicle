-- | This module exports the datatype representations of the core builtin symbols.
module Vehicle.Syntax.Builtin.BasicOperations where

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
  deriving (Eq, Ord, Show, Generic)

instance NFData FunctionPosition

instance Hashable FunctionPosition

instance Serialize FunctionPosition

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
  deriving (Eq, Ord, Enum, Bounded, Show, Generic)

instance NFData ComparisonOp

instance Hashable ComparisonOp

instance Serialize ComparisonOp

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
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance NFData Quantifier

instance Hashable Quantifier

instance ToJSON Quantifier

instance Serialize Quantifier

instance Pretty Quantifier where
  pretty = \case
    Forall -> "forall"
    Exists -> "exists"
