-- | This module exports the datatype representations of the builtin symbols.

module Vehicle.Language.AST.Builtin
  ( Builtin(..)
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
-- Type classes

data TypeClass
  -- Operation type-classes
  = HasEq Equality
  | HasOrd Order
  | HasNot
  | HasAnd
  | HasOr
  | HasImplies
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
  | ImpliesPolarity
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
    HasImplies         -> "HasImplies"
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
    ImpliesPolarity    -> "ImpliesPolarity"
    MaxPolarity        -> "MaxPolarity"

    TypesEqualModAuxiliaryAnnotations{} -> "TypesEqual"


--------------------------------------------------------------------------------
-- Builtin

-- |Builtins in the Vehicle language
data Builtin
  -- Types
  = Unit
  | Bool
  | Index
  | Nat
  | Int
  | Rat
  | List
  | Tensor

  -- Type classes
  | TypeClass TypeClass

  -- Boolean expressions
  | Not
  | And
  | Or
  | Implies
  | If

  -- Arithmetic expressions
  | Neg
  | Add
  | Sub
  | Mul
  | Div

  -- Container expressions
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
  | Polarity  Polarity
  | Linearity Linearity
  deriving (Eq, Generic)

instance NFData   Builtin
instance Hashable Builtin

instance Show Builtin where
  show = \case
    Unit                 -> "Unit"
    Bool                 -> "Bool"
    Index                -> "Index"
    Nat                  -> "Nat"
    Int                  -> "Int"
    Rat                  -> "Rat"
    List                 -> "List"
    Tensor               -> "Tensor"

    Not                  -> "not"
    And                  -> "and"
    Or                   -> "or"
    Implies              -> "=>"
    If                   -> "if"

    Add                  -> "+"
    Sub                  -> "-"
    Mul                  -> "*"
    Div                  -> "/"
    Neg                  -> "-"

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

instance Pretty Builtin where
  pretty b = case b of
    Unit                 -> pretty $ show b
    Bool                 -> pretty $ show b
    Index                -> pretty $ show b
    Nat                  -> pretty $ show b
    Int                  -> pretty $ show b
    Rat                  -> pretty $ show b
    List                 -> pretty $ show b
    Tensor               -> pretty $ show b

    Not                  -> pretty $ show b
    And                  -> pretty $ show b
    Or                   -> pretty $ show b
    Implies              -> pretty $ show b

    Add                  -> pretty $ show b
    Sub                  -> pretty $ show b
    Mul                  -> pretty $ show b
    Div                  -> pretty $ show b
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

builtinSymbols :: [(Symbol, Builtin)]
builtinSymbols = map (first pack)
  [ show Bool                         |-> Bool
  , show Nat                          |-> Nat
  , show Int                          |-> Int
  , show Rat                          |-> Rat
  , show List                         |-> List
  , show Tensor                       |-> Tensor

  , show If                           |-> If
  , show Implies                      |-> Implies
  , show And                          |-> And
  , show Or                           |-> Or
  , show Not                          |-> Not
  , show (Equality Eq)                |-> Equality Eq
  , show (Equality Neq)               |-> Equality Neq
  , show (Order Le)                   |-> Order Le
  , show (Order Lt)                   |-> Order Lt
  , show (Order Ge)                   |-> Order Ge
  , show (Order Gt)                   |-> Order Gt
  , show Add                          |-> Add
  , show Mul                          |-> Mul
  , show Div                          |-> Div
  , show Sub                          |-> Sub
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
