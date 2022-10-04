-- | This module exports the datatype representations of the core builtin symbols.

module Vehicle.Language.AST.Builtin.Core
  ( Quantifier(..)
  , EqualityOp(..)
  , equalityOp
  , equalityOpName
  , EqualityDomain(..)
  , OrderOp(..)
  , orderOp
  , orderOpName
  , OrderDomain(..)
  , isStrict
  , flipStrictness
  , flipOrder
  , chainable
  , FunctionPosition(..)
  ) where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData(..))
import Data.Hashable (Hashable (..))

import Vehicle.Prelude

-- | Represents whether something is an input or an output of a function
data FunctionPosition
  = FunctionInput Symbol Int
  | FunctionOutput Symbol
  deriving (Eq, Show, Generic)

instance NFData   FunctionPosition
instance Hashable FunctionPosition

instance Pretty FunctionPosition where
  pretty = \case
    FunctionInput n i -> "Input[" <> pretty n <> "][" <> pretty i <> "]"
    FunctionOutput n  -> "Output[" <> pretty n <> "]"

--------------------------------------------------------------------------------
-- EqualityOpOp

data EqualityOp
  = Eq
  | Neq
  deriving (Eq, Ord, Show, Generic)

instance NFData   EqualityOp
instance Hashable EqualityOp

instance Pretty EqualityOp where
  pretty = \case
    Eq  -> "=="
    Neq -> "!="

instance Negatable EqualityOp where
  neg Eq = Neq
  neg Neq = Eq

equalityOpName :: EqualityOp -> Doc a
equalityOpName = \case
  Eq  -> "equals"
  Neq -> "notEquals"

equalityOp :: Eq a => EqualityOp -> (a -> a -> Bool)
equalityOp Eq  = (==)
equalityOp Neq = (/=)

data EqualityDomain
  = EqNat
  | EqIndex
  | EqInt
  | EqRat
  deriving (Eq, Ord, Show, Generic)

instance NFData   EqualityDomain
instance Hashable EqualityDomain

instance Pretty EqualityDomain where
  pretty = \case
    EqNat   -> "Nat"
    EqIndex -> "Index"
    EqInt   -> "Int"
    EqRat   -> "Rat"

--------------------------------------------------------------------------------
-- Orders

data OrderOp
  = Le
  | Lt
  | Ge
  | Gt
  deriving (Eq, Ord, Show, Generic)

instance NFData   OrderOp
instance Hashable OrderOp

instance Pretty OrderOp where
  pretty = \case
    Le -> "<="
    Lt -> "<"
    Ge -> ">="
    Gt -> ">"

instance Negatable OrderOp where
  neg = \case
    Le -> Gt
    Lt -> Ge
    Ge -> Lt
    Gt -> Le

orderOp :: Ord a => OrderOp -> (a -> a -> Bool)
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

data OrderDomain
  = OrderNat
  | OrderIndex
  | OrderInt
  | OrderRat
  deriving (Eq, Ord, Show, Generic)

instance NFData   OrderDomain
instance Hashable OrderDomain

instance Pretty OrderDomain where
  pretty = \case
    OrderNat   -> "Nat"
    OrderIndex -> "Index"
    OrderInt   -> "Int"
    OrderRat   -> "Rat"

--------------------------------------------------------------------------------
-- Quantifiers

data Quantifier
  = Forall
  | Exists
  deriving (Show, Eq, Ord, Generic)

instance NFData   Quantifier
instance Hashable Quantifier

instance Negatable Quantifier where
  neg Forall = Exists
  neg Exists = Forall

instance Pretty Quantifier where
  pretty = \case
    Forall -> "forall"
    Exists -> "exists"
