-- | This module exports the datatype representations of the core builtin symbols.
module Vehicle.Syntax.Builtin.BasicOperations
  ( Quantifier (..),
    EqualityOp (..),
    equalityOp,
    equalityOpName,
    OrderOp (..),
    orderOp,
    orderOpName,
    Strictness (..),
    isStrict,
    isForward,
    flipStrictness,
    flipOrder,
    chainable,
    FunctionPosition (..),
  )
where

import Control.DeepSeq (NFData (..))
import Data.Aeson (FromJSON, ToJSON)
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
-- EqualityOp

data EqualityOp
  = Eq
  | Neq
  deriving (Eq, Ord, Show, Generic)

instance Hashable EqualityOp

instance Serialize EqualityOp

instance NFData EqualityOp

instance Pretty EqualityOp where
  pretty = \case
    Eq -> "=="
    Neq -> "!="

equalityOpName :: EqualityOp -> Doc a
equalityOpName = \case
  Eq -> "equals"
  Neq -> "notEquals"

equalityOp :: (Eq a) => EqualityOp -> (a -> a -> Bool)
equalityOp Eq = (==)
equalityOp Neq = (/=)

--------------------------------------------------------------------------------
-- Orders

data OrderOp
  = Le
  | Lt
  | Ge
  | Gt
  deriving (Eq, Ord, Show, Generic)

instance NFData OrderOp

instance Hashable OrderOp

instance Serialize OrderOp

instance Pretty OrderOp where
  pretty = \case
    Le -> "<="
    Lt -> "<"
    Ge -> ">="
    Gt -> ">"

orderOp :: (Ord a) => OrderOp -> (a -> a -> Bool)
orderOp Le = (<=)
orderOp Lt = (<)
orderOp Ge = (>=)
orderOp Gt = (>)

orderOpName :: OrderOp -> Doc a
orderOpName = \case
  Le -> "leq"
  Lt -> "lt"
  Ge -> "geq"
  Gt -> "gt"

isStrict :: OrderOp -> Bool
isStrict order = order == Lt || order == Gt

isForward :: OrderOp -> Bool
isForward order = order == Lt || order == Le

flipStrictness :: OrderOp -> OrderOp
flipStrictness = \case
  Le -> Lt
  Lt -> Le
  Ge -> Gt
  Gt -> Ge

flipOrder :: OrderOp -> OrderOp
flipOrder = \case
  Le -> Ge
  Lt -> Gt
  Ge -> Le
  Gt -> Lt

chainable :: OrderOp -> OrderOp -> Bool
chainable e1 e2 = e1 == e2 || e1 == flipStrictness e2

--------------------------------------------------------------------------------
-- Strictness

data Strictness
  = Strict
  | NonStrict
  deriving (Show, Eq, Ord, Generic)

instance NFData Strictness

instance ToJSON Strictness

instance FromJSON Strictness

--------------------------------------------------------------------------------
-- Quantifiers

data Quantifier
  = Forall
  | Exists
  deriving (Show, Eq, Ord, Generic)

instance NFData Quantifier

instance Hashable Quantifier

instance ToJSON Quantifier

instance Serialize Quantifier

instance Pretty Quantifier where
  pretty = \case
    Forall -> "forall"
    Exists -> "exists"
