-- | This module exports the datatype representations of the builtin symbols.
module Vehicle.Syntax.AST.Builtin
  ( module Vehicle.Syntax.AST.Builtin,
    module X,
  )
where

import Control.DeepSeq (NFData (..))
import Data.Aeson (FromJSON)
import Data.Aeson.Types (ToJSON)
import Data.Bifunctor (first)
import Data.Hashable (Hashable (..))
import Data.Serialize (Serialize)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty, (<+>))
import Prettyprinter.Render.Text (renderStrict)
import Vehicle.Syntax.AST.Builtin.Core as X
import Vehicle.Syntax.AST.Builtin.TypeClass as X

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
  | Int
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
    Int -> "Int"
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
  | LInt Int
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
    LInt x -> pretty x
    LRat x -> pretty x
    LVec n -> "LVec[" <> pretty n <> "]"

instance Pretty Rational where
  pretty p = pretty (fromRational p :: Double)

--------------------------------------------------------------------------------
-- Builtin

data NegDomain
  = NegInt
  | NegRat
  deriving (Eq, Ord, Show, Generic)

instance NFData NegDomain

instance Hashable NegDomain

instance ToJSON NegDomain

instance FromJSON NegDomain

instance Serialize NegDomain

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

instance NFData AddDomain

instance Hashable AddDomain

instance ToJSON AddDomain

instance FromJSON AddDomain

instance Serialize AddDomain

instance Pretty AddDomain where
  pretty = \case
    AddNat -> "Nat"
    AddInt -> "Int"
    AddRat -> "Rat"

data SubDomain
  = SubInt
  | SubRat
  deriving (Eq, Ord, Show, Generic)

instance NFData SubDomain

instance Hashable SubDomain

instance ToJSON SubDomain

instance FromJSON SubDomain

instance Serialize SubDomain

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

instance NFData MulDomain

instance Hashable MulDomain

instance ToJSON MulDomain

instance FromJSON MulDomain

instance Serialize MulDomain

instance Pretty MulDomain where
  pretty = \case
    MulNat -> "Nat"
    MulInt -> "Int"
    MulRat -> "Rat"

data DivDomain
  = DivRat
  deriving (Eq, Ord, Show, Generic)

instance NFData DivDomain

instance Hashable DivDomain

instance ToJSON DivDomain

instance FromJSON DivDomain

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
  | FromNatToInt
  | FromNatToRat
  deriving (Eq, Ord, Show, Generic)

instance Pretty FromNatDomain where
  pretty = \case
    FromNatToIndex -> "Index"
    FromNatToNat -> "Nat"
    FromNatToInt -> "Int"
    FromNatToRat -> "Rat"

instance NFData FromNatDomain

instance Hashable FromNatDomain

instance ToJSON FromNatDomain

instance FromJSON FromNatDomain

instance Serialize FromNatDomain

data FromRatDomain
  = FromRatToRat
  deriving (Eq, Ord, Show, Generic)

instance Pretty FromRatDomain where
  pretty = \case
    FromRatToRat -> "Rat"

instance NFData FromRatDomain

instance Hashable FromRatDomain

instance ToJSON FromRatDomain

instance FromJSON FromRatDomain

instance Serialize FromRatDomain

data FoldDomain
  = FoldList
  | FoldVector
  deriving (Eq, Ord, Show, Generic)

instance Pretty FoldDomain where
  pretty = \case
    FoldList -> "List"
    FoldVector -> "Vector"

instance NFData FoldDomain

instance Hashable FoldDomain

instance ToJSON FoldDomain

instance FromJSON FoldDomain

instance Serialize FoldDomain

data QuantifierDomain
  = QuantNat
  | QuantInt
  | QuantRat
  | QuantVec
  deriving (Eq, Ord, Show, Generic)

instance NFData QuantifierDomain

instance Hashable QuantifierDomain

instance ToJSON QuantifierDomain

instance FromJSON QuantifierDomain

instance Serialize QuantifierDomain

instance Pretty QuantifierDomain where
  pretty = \case
    QuantNat -> "Nat"
    QuantInt -> "Int"
    QuantRat -> "Rat"
    QuantVec -> "Vec"

data BuiltinFunction
  = Not
  | And
  | Or
  | Implies
  | Quantifier Quantifier QuantifierDomain
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
  | -- Comparison expressions
    Equals EqualityDomain EqualityOp
  | Order OrderDomain OrderOp
  | At
  | ConsVector
  | Fold FoldDomain
  | Indices
  deriving (Eq, Ord, Show, Generic)

instance NFData BuiltinFunction

instance Hashable BuiltinFunction

instance ToJSON BuiltinFunction

instance Serialize BuiltinFunction

-- TODO all the show instances should really be obtainable from the grammar
-- somehow.
instance Pretty BuiltinFunction where
  pretty = \case
    Not -> "not"
    And -> "and"
    Or -> "or"
    Implies -> "=>"
    Quantifier q dom -> pretty q <> pretty dom
    If -> "if"
    Neg dom -> "neg" <> pretty dom
    Add dom -> "add" <> pretty dom
    Sub dom -> "sub" <> pretty dom
    Mul dom -> "mul" <> pretty dom
    Div dom -> "div" <> pretty dom
    FromNat dom -> "fromNatTo" <> pretty dom
    FromRat dom -> "fromRatTo" <> pretty dom
    Equals dom op -> equalityOpName op <> pretty dom
    Order dom op -> orderOpName op <> pretty dom
    Fold dom -> "fold" <> pretty dom
    At -> "!"
    ConsVector -> "::v"
    Indices -> "indices"

-- | Builtins in the Vehicle language
data Builtin
  = Constructor BuiltinConstructor
  | BuiltinFunction BuiltinFunction
  | BuiltinType BuiltinType
  | TypeClass TypeClass
  | TypeClassOp TypeClassOp
  deriving (Eq, Show, Generic)

instance NFData Builtin

instance Hashable Builtin

instance ToJSON Builtin

instance Serialize Builtin

-- TODO all the show instances should really be obtainable from the grammar
-- somehow.
instance Pretty Builtin where
  pretty = \case
    Constructor c -> pretty c
    TypeClass tc -> pretty tc
    TypeClassOp o -> pretty o
    BuiltinFunction f -> pretty f
    BuiltinType t -> pretty t

builtinSymbols :: [(Text, Builtin)]
builtinSymbols = mempty

builtinFromSymbol :: Text -> Maybe Builtin
builtinFromSymbol symbol = lookup symbol builtinSymbols

symbolFromBuiltin :: Builtin -> Text
symbolFromBuiltin builtin = renderStrict . layoutPretty defaultLayoutOptions $ pretty builtin
