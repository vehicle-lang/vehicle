-- | This module exports the datatype representations of the builtin symbols.

module Vehicle.Language.AST.Builtin
  ( Builtin(..)
  , Quantifier(..)
  , Order(..)
  , TypeClass(..)
  , builtinFromSymbol
  , symbolFromBuiltin
  ) where

import Data.Bifunctor (first)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Text (pack)

import Vehicle.Prelude

-- TODO all the show instances should really be obtainable from the grammar
-- somehow.

--------------------------------------------------------------------------------
-- Type classes

data TypeClass
  = HasEq
  | HasOrd
  | IsTruth
  | IsNatural
  | IsIntegral
  | IsRational
  | IsReal
  | IsContainer
  | IsQuantifiable
  deriving (Eq, Ord, Generic)

instance NFData TypeClass

instance Show TypeClass where
  show = \case
    HasEq          -> "HasEq"
    HasOrd         -> "HasOrd"
    IsTruth        -> "IsTruth"
    IsContainer    -> "IsContainer"
    IsNatural      -> "IsNatural"
    IsIntegral     -> "IsIntegral"
    IsRational     -> "IsRational"
    IsReal         -> "IsReal"
    IsQuantifiable -> "IsQuantify"

--------------------------------------------------------------------------------
-- Quantifiers

data Quantifier
  = All
  | Any
  deriving (Eq, Ord, Show, Generic)

instance NFData Quantifier

instance Pretty Quantifier where
  pretty = \case
    All -> "every"
    Any -> "some"

--------------------------------------------------------------------------------
-- Orders

data Order
  = Le
  | Lt
  | Ge
  | Gt
  deriving (Eq, Ord, Generic)

instance NFData Order

instance Show Order where
  show = \case
    Le -> "<="
    Lt -> "<"
    Ge -> ">="
    Gt -> ">"

instance Pretty Order where
  pretty = pretty . show

--------------------------------------------------------------------------------
-- Standard builtins

-- |Builtins to the Vehicle language
data Builtin
  -- Types
  = Bool
  | Prop
  | Nat
  | Int
  | Real
  | List
  | Tensor
  -- Expressions
  | If
  | Impl
  | And
  | Or
  | Not
  | Eq
  | Neq
  | Mul
  | Div
  | Add
  | Sub
  | Neg
  | Cons
  | At
  | Map
  | Fold
  | Order     Order
  | TypeClass TypeClass
  | Quant     Quantifier
  | QuantIn   Quantifier
  deriving (Eq, Ord, Generic)

instance NFData Builtin

instance Pretty Builtin where
  pretty b = pretty $ symbolFromBuiltin b

--------------------------------------------------------------------------------
-- Conversion to symbols

instance Show Builtin where
  show = \case
    Bool         -> "Bool"
    Prop         -> "Prop"
    Nat          -> "Nat"
    Int          -> "Int"
    Real         -> "Real"
    List         -> "List"
    Tensor       -> "Tensor"
    If           -> "if"
    Impl         -> "implies"
    And          -> "and"
    Or           -> "or"
    Not          -> "not"
    Eq           -> "=="
    Neq          -> "!="
    Add          -> "+"
    Mul          -> "*"
    Div          -> "/"
    Sub          -> "-"
    Neg          -> "~"
    At           -> "!"
    Cons         -> "::"
    Order o      -> show o
    TypeClass tc -> show tc
    Map          -> "map"
    Fold         -> "fold"
    Quant All    -> "every"
    Quant Any    -> "some"
    QuantIn All  -> "everyIn"
    QuantIn Any  -> "someIn"

builtinSymbols :: [(Symbol, Builtin)]
builtinSymbols = map (first pack)
  [ show Bool          |-> Bool
  , show Prop          |-> Prop
  , show Nat           |-> Nat
  , show Int           |-> Int
  , show Real          |-> Real
  , show List          |-> List
  , show Tensor        |-> Tensor
  , show If            |-> If
  , show Impl          |-> Impl
  , show And           |-> And
  , show Or            |-> Or
  , show Not           |-> Not
  , show Eq            |-> Eq
  , show Neq           |-> Neq
  , show (Order Le)    |-> Order Le
  , show (Order Lt)    |-> Order Lt
  , show (Order Ge)    |-> Order Ge
  , show (Order Gt)    |-> Order Gt
  , show Add           |-> Add
  , show Mul           |-> Mul
  , show Div           |-> Div
  , show Sub           |-> Sub
  , show Neg           |-> Neg
  , show At            |-> At
  , show Cons          |-> Cons
  , show (Quant All)   |-> Quant All
  , show (Quant Any)   |-> Quant Any
  , show (QuantIn All) |-> QuantIn All
  , show (QuantIn Any) |-> QuantIn Any
  , show Map           |-> Map
  , show Fold          |-> Fold
  ]

builtinFromSymbol :: Symbol -> Maybe Builtin
builtinFromSymbol symbol = lookup symbol builtinSymbols

symbolFromBuiltin :: Builtin -> Symbol
symbolFromBuiltin builtin = pack $ show builtin