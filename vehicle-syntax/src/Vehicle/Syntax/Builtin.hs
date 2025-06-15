{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module exports the datatype representations of the builtin symbols.
module Vehicle.Syntax.Builtin
  ( module Vehicle.Syntax.Builtin,
    module X,
  )
where

import Control.DeepSeq (NFData (..))
import Data.Hashable (Hashable (..))
import Data.Serialize (Serialize)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Vehicle.Syntax.Builtin.BasicOperations as X
import Vehicle.Syntax.Builtin.Derived as X
import Vehicle.Syntax.Builtin.TypeClass as X
import Vehicle.Syntax.Tensor

--------------------------------------------------------------------------------
-- Types

-- | Constructors for types in the language. The types and type-classes
-- are viewed as constructors for `Type`.
data BuiltinType
  = UnitType
  | BoolType
  | IndexType
  | NatType
  | RatType
  | ListType
  | VectorType
  | TensorType
  deriving (Eq, Ord, Show, Generic)

instance NFData BuiltinType

instance Hashable BuiltinType

instance Serialize BuiltinType

instance Pretty BuiltinType where
  pretty = \case
    UnitType -> "Unit"
    BoolType -> "Bool"
    IndexType -> "Index"
    NatType -> "Nat"
    RatType -> "Rat"
    ListType -> "List"
    VectorType -> "Vector"
    TensorType -> "Tensor"

--------------------------------------------------------------------------------
-- Constructors

-- | Constructors for types in the language. The types and type-classes
-- are viewed as constructors for `Type`.
data BuiltinConstructor
  = Nil
  | Cons
  | UnitLiteral
  | IndexLiteral Int
  | NatLiteral Int
  | VectorLiteral
  | BoolTensorLiteral (Tensor Bool)
  | NatTensorLiteral (Tensor Int)
  | RatTensorLiteral (Tensor Rational)
  deriving (Eq, Ord, Show, Generic)

instance NFData BuiltinConstructor

instance Hashable BuiltinConstructor

instance Serialize BuiltinConstructor

instance Pretty BuiltinConstructor where
  pretty = \case
    Nil -> "nil"
    Cons -> "::"
    UnitLiteral -> "()"
    NatLiteral n -> pretty n
    IndexLiteral n -> pretty n
    VectorLiteral -> "vec"
    BoolTensorLiteral x -> pretty x
    NatTensorLiteral x -> pretty x
    RatTensorLiteral x -> pretty x

instance Pretty Rational where
  pretty p = pretty (fromRational p :: Double)

--------------------------------------------------------------------------------
-- Builtin

data BuiltinFunction
  = -- Boolean operations
    Not
  | And
  | Or
  | Implies
  | QuantifyRatTensor Quantifier
  | If
  | CompareIndex ComparisonOp
  | CompareNat ComparisonOp
  | CompareRatTensorPointwise ComparisonOp
  | ReduceAndTensor
  | ReduceOrTensor
  | -- Rat operations
    Add AddDomain
  | Mul MulDomain
  | Neg NegDomain
  | Sub SubDomain
  | Div DivDomain
  | Min MinDomain
  | Max MaxDomain
  | PowRat
  | ReduceAddRatTensor
  | ReduceMulRatTensor
  | ReduceMinRatTensor
  | ReduceMaxRatTensor
  | -- Tensor operations
    AtTensor
  | StackTensor
  | ConstTensor
  | Iterate
  | ForeachTensor
  | -- Vector operations
    AtVector
  | ForeachVector
  | -- List operations
    FoldList
  | MapList
  deriving (Eq, Ord, Show, Generic)

instance NFData BuiltinFunction

instance Hashable BuiltinFunction

instance Serialize BuiltinFunction

-- TODO all the show instances should really be obtainable from the grammar
-- somehow.
instance Pretty BuiltinFunction where
  pretty = \case
    And -> "and"
    Or -> "or"
    Not -> "not"
    Implies -> "=>"
    QuantifyRatTensor q -> pretty q
    If -> "if"
    ReduceAndTensor -> "reduceAndTensor"
    ReduceOrTensor -> "reduceOrTensor"
    Neg dom -> "neg" <> pretty dom
    Add dom -> "add" <> pretty dom
    Sub dom -> "sub" <> pretty dom
    Mul dom -> "mul" <> pretty dom
    Div dom -> "div" <> pretty dom
    Min dom -> "min" <> pretty dom
    Max dom -> "max" <> pretty dom
    PowRat -> "**"
    ReduceAddRatTensor -> "reduceAddRatTensor"
    ReduceMulRatTensor -> "reduceMulRatTensor"
    ReduceMinRatTensor -> "reduceMinRatTensor"
    ReduceMaxRatTensor -> "reduceMaxRatTensor"
    CompareIndex op -> comparisonOpName op <> "Index"
    CompareNat op -> comparisonOpName op <> "Nat"
    CompareRatTensorPointwise op -> comparisonOpName op <> "RatTensorPointwise"
    FoldList -> "foldList"
    MapList -> "mapList"
    ForeachTensor -> "foreachTensor"
    ForeachVector -> "foreachVector"
    Iterate -> "iterate"
    AtTensor -> "!t"
    AtVector -> "!v"
    StackTensor {} -> "stack"
    ConstTensor -> "const"

data BuiltinCast
  = -- Cast operations
    FromNat FromNatDomain
  | FromRat FromRatDomain
  | FromVectorToList
  deriving (Eq, Ord, Show, Generic)

instance NFData BuiltinCast

instance Hashable BuiltinCast

instance Serialize BuiltinCast

instance Pretty BuiltinCast where
  pretty = \case
    FromNat dom -> "fromNatTo" <> pretty dom
    FromRat dom -> "fromRatTo" <> pretty dom
    FromVectorToList -> "fromVectorToList"

-- | Builtins in the Vehicle language
data Builtin
  = BuiltinConstructor BuiltinConstructor
  | BuiltinFunction BuiltinFunction
  | BuiltinType BuiltinType
  | BuiltinCast BuiltinCast
  | DerivedFunction DerivedFunction
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
    BuiltinCast c -> pretty c
    DerivedFunction f -> pretty f
    TypeClass tc -> pretty tc
    TypeClassOp o -> pretty o
    NatInDomainConstraint {} -> "NatInDomainConstraint"

builtinSymbols :: [(Text, Builtin)]
builtinSymbols = mempty

builtinFromSymbol :: Text -> Maybe Builtin
builtinFromSymbol symbol = lookup symbol builtinSymbols

symbolFromBuiltin :: Builtin -> Text
symbolFromBuiltin builtin = renderStrict . layoutPretty defaultLayoutOptions $ pretty builtin

--------------------------------------------------------------------------------
-- Negation

class Negatable a where
  neg :: a -> a

instance Negatable ComparisonOp where
  neg = \case
    Eq -> Ne
    Ne -> Eq
    Le -> Gt
    Lt -> Ge
    Ge -> Lt
    Gt -> Le

instance Negatable Quantifier where
  neg Forall = Exists
  neg Exists = Forall
