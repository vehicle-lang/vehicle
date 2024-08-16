{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module exports the datatype representations of the builtin symbols.
module Vehicle.Syntax.Builtin
  ( module Vehicle.Syntax.Builtin,
    module X,
  )
where

import Control.DeepSeq (NFData (..))
import Data.Aeson (ToJSON (..))
import Data.Hashable (Hashable (..))
import Data.Serialize (Serialize)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Vehicle.Syntax.Builtin.BasicOperations as X
import Vehicle.Syntax.Builtin.TypeClass as X

--------------------------------------------------------------------------------
-- Types

-- | Constructors for types in the language. The types and type-classes
-- are viewed as constructors for `Type`.
data BuiltinType
  = -- Types
    Unit
  | Bool
  | Index
  | Nat
  | Rat
  | List
  | Vector
  deriving (Eq, Ord, Show, Generic)

instance NFData BuiltinType

instance Hashable BuiltinType

instance ToJSON BuiltinType

instance Serialize BuiltinType

instance Pretty BuiltinType where
  pretty = \case
    Unit -> "Unit"
    Bool -> "Bool"
    Index -> "Index"
    Nat -> "Nat"
    Rat -> "Rat"
    List -> "List"
    Vector -> "Vector"

--------------------------------------------------------------------------------
-- Constructors

-- | Constructors for types in the language. The types and type-classes
-- are viewed as constructors for `Type`.
data BuiltinConstructor
  = Nil
  | Cons
  | LUnit
  | LBool Bool
  | LIndex Int
  | LNat Int
  | LRat Rational
  | LVec Int
  deriving (Eq, Ord, Show, Generic)

instance NFData BuiltinConstructor

instance Hashable BuiltinConstructor

instance ToJSON BuiltinConstructor

instance Serialize BuiltinConstructor

instance Pretty BuiltinConstructor where
  pretty = \case
    Nil -> "nil"
    Cons -> "::"
    LUnit -> "()"
    LBool x -> pretty x
    LIndex x -> pretty x
    LNat x -> pretty x
    LRat x -> pretty x
    LVec n -> "LVec[" <> pretty n <> "]"

instance Pretty Rational where
  pretty p = pretty (fromRational p :: Double)

--------------------------------------------------------------------------------
-- Builtin

data OrderDomain
  = OrderIndex
  | OrderNat
  | OrderRat
  deriving (Eq, Ord, Show, Generic)

instance NFData OrderDomain

instance Hashable OrderDomain

instance Serialize OrderDomain

instance Pretty OrderDomain where
  pretty = \case
    OrderNat -> "Nat"
    OrderIndex -> "Index"
    OrderRat -> "Rat"

data EqualityDomain
  = EqIndex
  | EqNat
  | EqRat
  deriving (Eq, Ord, Show, Generic)

instance NFData EqualityDomain

instance Hashable EqualityDomain

instance Serialize EqualityDomain

instance Pretty EqualityDomain where
  pretty = \case
    EqIndex -> "Index"
    EqNat -> "Nat"
    EqRat -> "Rat"

data NegDomain
  = NegRat
  deriving (Eq, Ord, Show, Generic)

instance NFData NegDomain

instance Hashable NegDomain

instance Serialize NegDomain

instance Pretty NegDomain where
  pretty = \case
    NegRat -> "Rat"

negToMulDomain :: NegDomain -> MulDomain
negToMulDomain = \case
  NegRat -> MulRat

data AddDomain
  = AddNat
  | AddRat
  deriving (Eq, Ord, Show, Generic)

instance NFData AddDomain

instance Hashable AddDomain

instance Serialize AddDomain

instance Pretty AddDomain where
  pretty = \case
    AddNat -> "Nat"
    AddRat -> "Rat"

data SubDomain
  = SubRat
  deriving (Eq, Ord, Show, Generic)

instance NFData SubDomain

instance Hashable SubDomain

instance Serialize SubDomain

instance Pretty SubDomain where
  pretty = \case
    SubRat -> "Rat"

subToAddDomain :: SubDomain -> AddDomain
subToAddDomain = \case
  SubRat -> AddRat

subToNegDomain :: SubDomain -> NegDomain
subToNegDomain = \case
  SubRat -> NegRat

data MulDomain
  = MulNat
  | MulRat
  deriving (Eq, Ord, Show, Generic)

instance NFData MulDomain

instance Hashable MulDomain

instance Serialize MulDomain

instance Pretty MulDomain where
  pretty = \case
    MulNat -> "Nat"
    MulRat -> "Rat"

data DivDomain
  = DivRat
  deriving (Eq, Ord, Show, Generic)

instance NFData DivDomain

instance Hashable DivDomain

instance Serialize DivDomain

instance Pretty DivDomain where
  pretty = \case
    DivRat -> "Rat"

divToMulDomain :: DivDomain -> MulDomain
divToMulDomain = \case
  DivRat -> MulRat

data FromNatDomain
  = FromNatToIndex
  | FromNatToNat
  | FromNatToRat
  deriving (Eq, Ord, Show, Generic)

instance Pretty FromNatDomain where
  pretty = \case
    FromNatToIndex -> "Index"
    FromNatToNat -> "Nat"
    FromNatToRat -> "Rat"

instance Serialize FromNatDomain

instance NFData FromNatDomain

instance Hashable FromNatDomain

data FromRatDomain
  = FromRatToRat
  deriving (Eq, Ord, Show, Generic)

instance Pretty FromRatDomain where
  pretty = \case
    FromRatToRat -> "Rat"

instance NFData FromRatDomain

instance Hashable FromRatDomain

instance Serialize FromRatDomain

data BuiltinFunction
  = Not
  | And
  | Or
  | Implies
  | Quantifier Quantifier
  | If
  | -- Numeric conversion
    FromNat FromNatDomain
  | FromRat FromRatDomain
  | -- Numeric operations
    Neg NegDomain
  | Add AddDomain
  | Sub SubDomain
  | Mul MulDomain
  | Div DivDomain
  | PowRat
  | MinRat
  | MaxRat
  | -- Comparison expressions
    Equals EqualityDomain EqualityOp
  | Order OrderDomain OrderOp
  | At
  | FoldList
  | FoldVector
  | MapList
  | MapVector
  | ZipWithVector
  | Indices
  deriving (Eq, Ord, Show, Generic)

instance NFData BuiltinFunction

instance Hashable BuiltinFunction

instance Serialize BuiltinFunction

-- TODO all the show instances should really be obtainable from the grammar
-- somehow.
instance Pretty BuiltinFunction where
  pretty = \case
    Not -> "not"
    And -> "and"
    Or -> "or"
    Implies -> "=>"
    Quantifier q -> pretty q
    If -> "if"
    Neg dom -> "neg" <> pretty dom
    Add dom -> "add" <> pretty dom
    Sub dom -> "sub" <> pretty dom
    Mul dom -> "mul" <> pretty dom
    Div dom -> "div" <> pretty dom
    PowRat -> "**"
    MinRat -> "min"
    MaxRat -> "max"
    FromNat dom -> "fromNatTo" <> pretty dom
    FromRat dom -> "fromRatTo" <> pretty dom
    Equals dom op -> equalityOpName op <> pretty dom
    Order dom op -> orderOpName op <> pretty dom
    FoldList -> "foldList"
    FoldVector -> "foldVector"
    MapList -> "mapList"
    MapVector -> "mapVector"
    ZipWithVector -> "zipWith"
    At -> "!"
    Indices -> "indices"

-- | Builtins in the Vehicle language
data Builtin
  = BuiltinConstructor BuiltinConstructor
  | BuiltinFunction BuiltinFunction
  | BuiltinType BuiltinType
  | TypeClass TypeClass
  | TypeClassOp TypeClassOp
  | NatInDomainConstraint
  deriving (Eq, Ord, Show, Generic)

instance NFData Builtin

instance Hashable Builtin

instance Serialize Builtin

-- TODO all the show instances should really be obtainable from the grammar
-- somehow.
instance Pretty Builtin where
  pretty = \case
    BuiltinFunction f -> pretty f
    BuiltinType t -> pretty t
    BuiltinConstructor c -> pretty c
    TypeClass tc -> pretty tc
    TypeClassOp o -> pretty o
    NatInDomainConstraint {} -> "NatInDomainConstraint"

builtinSymbols :: [(Text, Builtin)]
builtinSymbols = mempty

builtinFromSymbol :: Text -> Maybe Builtin
builtinFromSymbol symbol = lookup symbol builtinSymbols

symbolFromBuiltin :: Builtin -> Text
symbolFromBuiltin builtin = renderStrict . layoutPretty defaultLayoutOptions $ pretty builtin
