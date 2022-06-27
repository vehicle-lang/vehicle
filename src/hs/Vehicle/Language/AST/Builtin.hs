-- | This module exports the datatype representations of the builtin symbols.

module Vehicle.Language.AST.Builtin
  ( Builtin(..)
  , NumericType(..)
  , ContainerType(..)
  , TypeClass(..)
  , builtinFromSymbol
  , symbolFromBuiltin
  , module X
  ) where

import Data.Bifunctor (first)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData(..))
import Data.Text (pack)
import Data.Hashable (Hashable (..))

import Vehicle.Prelude
import Vehicle.Language.AST.Builtin.Core as X
import Vehicle.Language.AST.Builtin.Polarity as X
import Vehicle.Language.AST.Builtin.Linearity as X

-- TODO all the show instances should really be obtainable from the grammar
-- somehow.

--------------------------------------------------------------------------------
-- Numeric types

data NumericType
  = Nat
  | Int
  | Rat
  deriving (Eq, Ord, Show, Generic)

instance NFData   NumericType
instance Hashable NumericType

instance Pretty NumericType where
  pretty = pretty . show

--------------------------------------------------------------------------------
-- Container types

data ContainerType
  = List
  | Tensor
  deriving (Eq, Ord, Show, Generic)

instance NFData   ContainerType
instance Hashable ContainerType

instance Pretty ContainerType where
  pretty = pretty . show

--------------------------------------------------------------------------------
-- Type classes

data TypeClass
  -- Operation type-classes
  = HasEq Equality
  | HasOrd Order
  | HasNot
  | HasAnd
  | HasOr
  | HasImpl
  | HasQuantifier Quantifier
  | HasAdd
  | HasSub
  | HasMul
  | HasDiv
  | HasNeg
  | HasFold
  | HasQuantifierIn Quantifier

  -- Literal type-classes
  | HasNatLitsUpTo Int
  -- ^ The parameter is the maximum value (needed for Index).
  | HasIntLits
  | HasRatLits
  | HasConLitsOfSize Int
  -- ^ Don't need to store the size as it stores the type of every element.

  ----------------------------
  -- Synthetic type-classes --
  ----------------------------

  -- Linearity type-classes
  | MaxLinearity
  | MulLinearity

  -- Polarity type-classes
  | NegPolarity
  | AddPolarity Quantifier
  | EqPolarity Equality
  | ImplPolarity
  | MaxPolarity

  -- Utility type-classes
  | TypesEqualModAuxiliaryAnnotations
  -- ^ Types are equal, modulo the auxiliary constraints.
  deriving (Eq, Generic, Show)

instance NFData   TypeClass
instance Hashable TypeClass

instance Pretty TypeClass where
  pretty = \case
    HasEq{}            -> "HasEq"
    HasOrd{}           -> "HasOrd"
    HasNot             -> "HasNot"
    HasAnd             -> "HasAnd"
    HasOr              -> "HasOr"
    HasImpl            -> "HasImpl"
    HasQuantifier q    -> "HasQuantifier" <+> pretty q
    HasAdd             -> "HasAdd"
    HasSub             -> "HasSub"
    HasMul             -> "HasMul"
    HasDiv             -> "HasDiv"
    HasNeg             -> "HasNeg"
    HasFold            -> "HasFold"
    HasQuantifierIn q  -> "HasQuantifierIn" <+> pretty q

    HasNatLitsUpTo n   -> "HasNatLiteralsUpTo[" <> pretty n <> "]"
    HasIntLits         -> "HasIntLiterals"
    HasRatLits         -> "HasRatLiterals"
    HasConLitsOfSize n -> "HasConLiteralsOfSize[" <>  pretty n <> "]"

    MaxLinearity       -> "MaxLinearity"
    MulLinearity       -> "MulLinearity"

    NegPolarity        -> "NegPolarity"
    AddPolarity q      -> "AddPolarity" <+> pretty q
    EqPolarity eq      -> "EqPolarity" <+> pretty eq
    ImplPolarity       -> "ImplPolarity"
    MaxPolarity        -> "MaxPolarity"

    TypesEqualModAuxiliaryAnnotations{} -> "TypesEqual"


--------------------------------------------------------------------------------
-- Builtin

-- |Builtins in the Vehicle language
data Builtin
  -- Types
  = Bool
  | NumericType   NumericType
  | ContainerType ContainerType
  | Index
  -- Type classes
  | TypeClass TypeClass
  -- Expressions
  | If
  | Not
  | BooleanOp2 BooleanOp2
  | Neg
  | NumericOp2 NumericOp2
  | Cons
  | At
  | Map
  | Fold
  | Equality  Equality
  | Order     Order
  | Quant     Quantifier
  | QuantIn   Quantifier
  | Foreach
  | ForeachIn

  -- Annotations - these should not be shown to the user.
  | Polarity Polarity
  | Linearity Linearity
  deriving (Eq, Generic)

instance NFData   Builtin
instance Hashable Builtin

instance Pretty Builtin where
  pretty = \case
    Bool                 -> "Bool"
    NumericType   t      -> pretty t
    ContainerType t      -> pretty t
    Index                -> "Index"
    BooleanOp2 op        -> pretty op
    Not                  -> "not"
    NumericOp2 op        -> pretty op
    Neg                  -> "-"
    If                   -> "if"
    At                   -> "!"
    Cons                 -> "::"
    Equality e           -> pretty e
    Order o              -> pretty o
    Map                  -> "map"
    Fold                 -> "fold"
    Quant   Forall       -> "forall"
    Quant   Exists       -> "exists"
    QuantIn q            -> pretty (Quant q) <> "In"
    Foreach              -> "foreach"
    ForeachIn            -> "foreachIn"
    TypeClass tc         -> pretty tc
    Polarity pol         -> pretty pol
    Linearity lin        -> pretty lin

instance Show Builtin where
  show = \case
    Bool                 -> "Bool"
    NumericType   t      -> show t
    ContainerType t      -> show t
    Index                -> "Index"
    BooleanOp2 op        -> show op
    Not                  -> "not"
    NumericOp2 op        -> show op
    Neg                  -> "-"
    If                   -> "if"
    At                   -> "!"
    Cons                 -> "::"
    Equality e           -> show e
    Order o              -> show o
    Map                  -> "map"
    Fold                 -> "fold"
    Quant   Forall       -> "forall"
    Quant   Exists       -> "exists"
    QuantIn q            -> show (Quant q) <> "In"
    Foreach              -> "foreach"
    ForeachIn            -> "foreachIn"
    TypeClass tc         -> show tc
    Polarity pol         -> show pol
    Linearity lin        -> show lin

builtinSymbols :: [(Symbol, Builtin)]
builtinSymbols = map (first pack)
  [ show Bool                         |-> Bool
  , show (NumericType Nat)            |-> NumericType Nat
  , show (NumericType Int)            |-> NumericType Int
  , show (NumericType Rat)            |-> NumericType Rat
  , show (ContainerType List)         |-> ContainerType List
  , show (ContainerType Tensor)       |-> ContainerType Tensor
  , show If                           |-> If
  , show (BooleanOp2 Impl)            |-> BooleanOp2 Impl
  , show (BooleanOp2 And)             |-> BooleanOp2 And
  , show (BooleanOp2 Or)              |-> BooleanOp2 Or
  , show Not                          |-> Not
  , show (Equality Eq)                |-> Equality Eq
  , show (Equality Neq)               |-> Equality Neq
  , show (Order Le)                   |-> Order Le
  , show (Order Lt)                   |-> Order Lt
  , show (Order Ge)                   |-> Order Ge
  , show (Order Gt)                   |-> Order Gt
  , show (NumericOp2 Add)             |-> NumericOp2 Add
  , show (NumericOp2 Mul)             |-> NumericOp2 Mul
  , show (NumericOp2 Div)             |-> NumericOp2 Div
  , show (NumericOp2 Sub)             |-> NumericOp2 Sub
  , show Neg                          |-> Neg
  , show At                           |-> At
  , show Cons                         |-> Cons
  , show (Quant Forall)               |-> Quant Forall
  , show (Quant Exists)               |-> Quant Exists
  , show (QuantIn Forall)             |-> QuantIn Forall
  , show (QuantIn Exists)             |-> QuantIn Exists
  , show Foreach                      |-> Foreach
  , show ForeachIn                    |-> ForeachIn
  , show Map                          |-> Map
  , show Fold                         |-> Fold
  ]

builtinFromSymbol :: Symbol -> Maybe Builtin
builtinFromSymbol symbol = lookup symbol builtinSymbols

symbolFromBuiltin :: Builtin -> Symbol
symbolFromBuiltin builtin = pack $ show builtin
