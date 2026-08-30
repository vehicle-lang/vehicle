module Vehicle.Data.Builtin.Core
  ( module Vehicle.Data.Builtin.Core,
    module X,
  )
where

import Control.DeepSeq (NFData (..))
import Data.Hashable (Hashable (..))
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..))
import Vehicle.Data.Builtin.Core.BasicOperations as X
import Vehicle.Data.Builtin.Core.Derived as X
import Vehicle.Data.Builtin.Core.TypeClass as X
  ( TypeClass (..),
    TypeClassOp (..),
  )
import Vehicle.Data.Real
import Vehicle.Data.Tensor

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
  | RatTensorLiteral (Tensor ExtendedRational)
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
    VectorLiteral -> "vecLit"
    BoolTensorLiteral x -> pretty x
    NatTensorLiteral x -> pretty x
    RatTensorLiteral x -> pretty x

--------------------------------------------------------------------------------
-- Builtin

data BuiltinFunction
  = -- Boolean operations
    Not
  | And
  | Or
  | Implies
  | QuantifyRatTensor Quantifier
  | QuantifyRecord Quantifier
  | If
  | CompareIndex ComparisonOp
  | CompareNat ComparisonOp
  | CompareRatTensor ComparisonOp
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
  | Pow PowDomain
  | Log LogDomain
  | Exp ExpDomain
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
  | Transpose
  | WhereTensor
  | SearchRatTensor
  | -- Vector operations
    AtVector
  | ForeachVector
  | -- List operations
    FoldList
  | MapList
  | ReverseList
  | AppendList
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
    QuantifyRecord q -> pretty q
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
    Pow dom -> "pow" <> pretty dom
    Log dom -> "log" <> pretty dom
    Exp dom -> "exp" <> pretty dom
    ReduceAddRatTensor -> "reduceAddRatTensor"
    ReduceMulRatTensor -> "reduceMulRatTensor"
    ReduceMinRatTensor -> "reduceMinRatTensor"
    ReduceMaxRatTensor -> "reduceMaxRatTensor"
    CompareIndex op -> comparisonOpName op <> "Index"
    CompareNat op -> comparisonOpName op <> "Nat"
    CompareRatTensor op -> comparisonOpName op <> "RatTensor"
    FoldList -> "foldList"
    MapList -> "mapList"
    ReverseList -> "reverseList"
    AppendList -> "appendList"
    ForeachTensor -> "foreachTensor"
    ForeachVector -> "foreachVector"
    Iterate -> "iterate"
    AtTensor -> "atTensor"
    AtVector -> "atVector"
    StackTensor {} -> "stack"
    ConstTensor -> "const"
    Transpose -> "transpose"
    SearchRatTensor -> "search"
    WhereTensor -> "where"

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
