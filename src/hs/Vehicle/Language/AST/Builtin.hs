-- | This module exports the datatype representations of the builtin symbols.

module Vehicle.Language.AST.Builtin
  ( module Vehicle.Language.AST.Builtin
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
  = HasEq EqualityOp
  | HasOrd OrderOp
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
  | HasNatLits Int
  -- ^ The parameter is the value (needed for Index).
  | HasRatLits
  | HasVecLits Int
  -- ^ The parameter is the size of the vector.

  -- Utility constraints
  | AlmostEqualConstraint
  -- ^ Types are equal, modulo the auxiliary constraints.
  | NatInDomainConstraint Int

  ----------------------------
  -- Synthetic type-classes --
  ----------------------------

  -- Linearity type-classes
  | MaxLinearity
  | MulLinearity
  | FunctionLinearity FunctionPosition

  -- Polarity type-classes
  | NegPolarity
  | AddPolarity Quantifier
  | EqPolarity EqualityOp
  | ImpliesPolarity
  | MaxPolarity
  | FunctionPolarity FunctionPosition
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

    HasNatLits n       -> "HasNatLiterals[" <> pretty n <> "]"
    HasRatLits         -> "HasRatLiterals"
    HasVecLits n       -> "HasVecLiterals[" <>  pretty n <> "]"

    MaxLinearity        -> "MaxLinearity"
    MulLinearity        -> "MulLinearity"
    FunctionLinearity{} -> "FunctionOutputLinearity"

    NegPolarity        -> "NegPolarity"
    AddPolarity q      -> "AddPolarity" <+> pretty q
    EqPolarity eq      -> "EqPolarity" <+> pretty eq
    ImpliesPolarity    -> "ImpliesPolarity"
    MaxPolarity        -> "MaxPolarity"
    FunctionPolarity{} -> "FunctionOutputPolarity"

    AlmostEqualConstraint{}    -> "AlmostEqualConstraint"
    NatInDomainConstraint{}    -> "NatInDomainConstraint"


-- Builtin operations for type-classes
data TypeClassOp
  = NotTC
  | AndTC
  | OrTC
  | ImpliesTC

  | FromNatTC Int
  | FromRatTC
  | FromVecTC Int

  | NegTC
  | AddTC
  | SubTC
  | MulTC
  | DivTC

  | EqualsTC EqualityOp
  | OrderTC OrderOp

  | MapTC
  | FoldTC

  | QuantifierTC   Quantifier
  | QuantifierInTC Quantifier
  deriving (Eq, Generic, Show)

instance NFData   TypeClassOp
instance Hashable TypeClassOp

instance Pretty TypeClassOp where
  pretty = \case
    NotTC   -> "not"
    AndTC   -> "and"
    OrTC    -> "or"
    ImpliesTC -> "=>"

    NegTC   -> "-"
    AddTC   -> "+"
    SubTC   -> "-"
    MulTC   -> "*"
    DivTC   -> "/"

    FromNatTC n -> "fromNat[" <> pretty n <> "]"
    FromRatTC   -> "fromRat"
    FromVecTC n -> "fromVec[" <> pretty n <> "]"

    EqualsTC op -> pretty op
    OrderTC  op -> pretty op

    MapTC  -> "map"
    FoldTC -> "fold"

    QuantifierTC   q -> pretty q
    QuantifierInTC q -> pretty q <> "In"

--------------------------------------------------------------------------------
-- Builtin

data NegDomain
  = NegInt
  | NegRat
  deriving (Eq, Ord, Show, Generic)

instance NFData   NegDomain
instance Hashable NegDomain

instance Pretty NegDomain where
  pretty = \case
    NegInt -> "Int"
    NegRat -> "Rat"

data AddDomain
  = AddNat
  | AddInt
  | AddRat
  deriving (Eq, Ord, Show, Generic)

instance NFData   AddDomain
instance Hashable AddDomain

instance Pretty AddDomain where
  pretty = \case
    AddNat -> "Nat"
    AddInt -> "Int"
    AddRat -> "Rat"

data SubDomain
  = SubInt
  | SubRat
  deriving (Eq, Ord, Show, Generic)

instance NFData   SubDomain
instance Hashable SubDomain

instance Pretty SubDomain where
  pretty = \case
    SubInt -> "Int"
    SubRat -> "Rat"

subToAddDomain :: SubDomain -> AddDomain
subToAddDomain = \case
  SubInt -> AddInt
  SubRat -> AddRat

subToNegDomain :: SubDomain -> NegDomain
subToNegDomain = \case
  SubInt -> NegInt
  SubRat -> NegRat

data MulDomain
  = MulNat
  | MulInt
  | MulRat
  deriving (Eq, Ord, Show, Generic)

instance NFData   MulDomain
instance Hashable MulDomain

instance Pretty MulDomain where
  pretty = \case
    MulNat -> "Nat"
    MulInt -> "Int"
    MulRat -> "Rat"

data DivDomain
  = DivRat
  deriving (Eq, Ord, Show, Generic)

instance NFData   DivDomain
instance Hashable DivDomain

instance Pretty DivDomain where
  pretty = \case
    DivRat -> "Rat"

data FromNatDomain
  = FromNatToIndex
  | FromNatToNat
  | FromNatToInt
  | FromNatToRat
  deriving (Eq, Ord, Show, Generic)

instance Pretty FromNatDomain where
  pretty = \case
    FromNatToIndex -> "Index"
    FromNatToNat   -> "Nat"
    FromNatToInt   -> "Int"
    FromNatToRat   -> "Rat"

instance NFData   FromNatDomain
instance Hashable FromNatDomain

data FromRatDomain
  = FromRatToRat
  deriving (Eq, Ord, Show, Generic)

instance Pretty FromRatDomain where
  pretty = \case
    FromRatToRat -> "Rat"

instance NFData   FromRatDomain
instance Hashable FromRatDomain

data FromVecDomain
  = FromVecToVec
  | FromVecToList
  deriving (Eq, Ord, Show, Generic)

instance Pretty FromVecDomain where
  pretty = \case
    FromVecToVec  -> "Vector"
    FromVecToList -> "List"

instance NFData   FromVecDomain
instance Hashable FromVecDomain

data FoldDomain
  = FoldList
  | FoldVector
  deriving (Eq, Ord, Show, Generic)

instance Pretty FoldDomain where
  pretty = \case
    FoldList   -> "List"
    FoldVector -> "Vector"

instance NFData   FoldDomain
instance Hashable FoldDomain

data MapDomain
  = MapList
  | MapVector
  deriving (Eq, Ord, Show, Generic)

instance Pretty MapDomain where
  pretty = \case
    MapList   -> "List"
    MapVector -> "Vector"

instance NFData   MapDomain
instance Hashable MapDomain

-- |Builtins in the Vehicle language
data Builtin
  -- Annotations - these should not be shown to the user.
  = Polarity  Polarity
  | Linearity Linearity

  -- Types
  | Unit
  | Bool
  | Index
  | Nat
  | Int
  | Rat
  | List
  | Vector
  | Tensor

  -- Type classes
  | TypeClass   TypeClass
  | TypeClassOp TypeClassOp

  -- Boolean expressions
  | Not
  | And
  | Or
  | Implies
  | If

  -- Numeric conversion
  | FromNat Int FromNatDomain
  | FromRat FromRatDomain
  | FromVec Int FromVecDomain

  -- Numeric operations
  | Neg NegDomain
  | Add AddDomain
  | Sub SubDomain
  | Mul MulDomain
  | Div DivDomain

  -- Comparison expressions
  | Equals EqualityDomain EqualityOp
  | Order  OrderDomain OrderOp

  -- Container expressions
  | Nil
  | Cons
  | At
  | Map  MapDomain
  | Fold FoldDomain
  | Foreach
  deriving (Eq, Show, Generic)

instance NFData   Builtin
instance Hashable Builtin

instance Pretty Builtin where
  pretty = \case
    Polarity  pol -> pretty pol
    Linearity lin -> pretty lin

    TypeClass   tc   -> pretty tc
    TypeClassOp tcOp -> pretty tcOp

    Unit   -> "Unit"
    Bool   -> "Bool"
    Index  -> "Index"
    Nat    -> "Nat"
    Int    -> "Int"
    Rat    -> "Rat"
    List   -> "List"
    Vector -> "Vector"
    Tensor -> "Tensor"

    Not     -> "notBool"
    And     -> "andBool"
    Or      -> "orBool"
    Implies -> "impliesBool"
    If      -> "if"

    Neg dom -> "neg" <> pretty dom
    Add dom -> "add" <> pretty dom
    Sub dom -> "sub" <> pretty dom
    Mul dom -> "mul" <> pretty dom
    Div dom -> "div" <> pretty dom

    FromNat n dom -> "fromNat[" <> pretty n <> "]To" <> pretty dom
    FromRat dom   -> "fromRatTo" <> pretty dom
    FromVec n dom -> "fromVec[" <> pretty n <> "]To" <> pretty dom

    Equals dom op -> equalityOpName op <> pretty dom
    Order  dom op -> orderOpName op <> pretty dom

    Foreach  -> "foreach"
    Fold dom -> "fold" <> pretty dom
    Map dom  -> "map" <> pretty dom
    At       -> "!"
    Nil      -> "nil"
    Cons     -> "::"

builtinSymbols :: [(Symbol, Builtin)]
builtinSymbols = map (first pack)
  [ show Bool                         |-> Bool
  , show Nat                          |-> Nat
  , show Int                          |-> Int
  , show Rat                          |-> Rat
  , show List                         |-> List
  , show Tensor                       |-> Tensor

  , show If                           |-> If
  , show At                           |-> At
  , show Cons                         |-> Cons
  ]

builtinFromSymbol :: Symbol -> Maybe Builtin
builtinFromSymbol symbol = lookup symbol builtinSymbols

symbolFromBuiltin :: Builtin -> Symbol
symbolFromBuiltin builtin = layoutAsText $ pretty builtin