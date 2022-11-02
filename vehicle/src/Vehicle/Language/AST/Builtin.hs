-- | This module exports the datatype representations of the builtin symbols.

module Vehicle.Language.AST.Builtin
  ( module Vehicle.Language.AST.Builtin
  , module X
  ) where

import Control.DeepSeq (NFData (..))
import Data.Bifunctor (first)
import Data.Hashable (Hashable (..))
import Data.Text (Text, pack)
import GHC.Generics (Generic)

import Vehicle.Language.AST.Builtin.Core as X
import Vehicle.Language.AST.Builtin.Linearity as X
import Vehicle.Language.AST.Builtin.Polarity as X
import Vehicle.Language.AST.Builtin.TypeClass as X
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Constructors

-- | Constructors for types in the language. The types and type-classes
-- are viewed as constructors for `Type`.
data BuiltinConstructor
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

  -- Type classes
  | TypeClass   TypeClass

  -- Container expressions
  | Nil
  | Cons
  deriving (Eq, Show, Generic)

instance NFData   BuiltinConstructor
instance Hashable BuiltinConstructor

instance Pretty BuiltinConstructor where
  pretty = \case
    Unit           -> "Unit"
    Bool           -> "Bool"
    Index          -> "Index"
    Nat            -> "Nat"
    Int            -> "Int"
    Rat            -> "Rat"
    List           -> "List"
    Vector         -> "Vector"

    TypeClass   tc -> pretty tc

    Polarity  pol  -> pretty pol
    Linearity lin  -> pretty lin

    Nil            -> "nil"
    Cons           -> "::"

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

negToMulDomain :: NegDomain -> MulDomain
negToMulDomain = \case
  NegInt -> MulInt
  NegRat -> MulRat

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
  = Constructor BuiltinConstructor

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

  | At
  | Map  MapDomain

  -- Derived
  | Tensor
  | TypeClassOp TypeClassOp
  | Fold FoldDomain
  | Foreach
  deriving (Eq, Show, Generic)

instance NFData   Builtin
instance Hashable Builtin

-- TODO all the show instances should really be obtainable from the grammar
-- somehow.
instance Pretty Builtin where
  pretty = \case
    Constructor c    -> pretty c

    TypeClassOp tcOp -> pretty tcOp

    Not              -> "notBool"
    And              -> "andBool"
    Or               -> "orBool"
    Implies          -> "impliesBool"
    If               -> "if"

    Neg dom          -> "neg" <> pretty dom
    Add dom          -> "add" <> pretty dom
    Sub dom          -> "sub" <> pretty dom
    Mul dom          -> "mul" <> pretty dom
    Div dom          -> "div" <> pretty dom

    FromNat n dom    -> "fromNat[" <> pretty n <> "]To" <> pretty dom
    FromRat dom      -> "fromRatTo" <> pretty dom
    FromVec n dom    -> "fromVec[" <> pretty n <> "]To" <> pretty dom

    Equals dom op    -> equalityOpName op <> pretty dom
    Order  dom op    -> orderOpName op <> pretty dom

    Tensor           -> "Tensor"
    Foreach          -> "foreach"
    Fold dom         -> "fold" <> pretty dom
    Map dom          -> "map" <> pretty dom
    At               -> "!"

builtinSymbols :: [(Text, Builtin)]
builtinSymbols = map (first pack)
  [ show If                           |-> If
  , show At                           |-> At
  ]

builtinFromSymbol :: Text -> Maybe Builtin
builtinFromSymbol symbol = lookup symbol builtinSymbols

symbolFromBuiltin :: Builtin -> Text
symbolFromBuiltin builtin = layoutAsText $ pretty builtin
