module Vehicle.Backend.Tensors.Core where

import Data.Hashable (Hashable)
import Data.Ratio ((%))
import GHC.Generics (Generic)
import Vehicle.Compile.Arity (Arity)
import Vehicle.Data.BuiltinInterface (PrintableBuiltin (..), cheatConvertBuiltin)
import Vehicle.Libraries.StandardLibrary.Definitions (StdLibFunction (..))
import Vehicle.Syntax.AST (Provenance (..))
import Vehicle.Syntax.AST qualified as V
import Vehicle.Syntax.Builtin qualified as V

--------------------------------------------------------------------------------
-- Builtin datatype

-- | The builtin types suitable for tensor operations.
data TensorBuiltin
  = -----------------------
    -- Types operations --
    -----------------------
    UnitType
  | IndexType
  | TensorType
  | ListType
  | ------------------
    -- Constructors --
    ------------------
    NilList
  | ConsList
  | Index Int
  | BoolTensor Bool
  | NatTensor Int
  | IntTensor Int
  | RatTensor Int Int
  | ---------------------------------
    -- Pointwise tensor operations --
    ---------------------------------
    Not
  | And
  | Or
  | Neg
  | Add
  | Sub
  | Mul
  | Div
  | PowRat
  | MinRat
  | MaxRat
  | EqIndex
  | EqTensor
  | NeIndex
  | NeTensor
  | LeIndex
  | LeTensor
  | LtIndex
  | LtTensor
  | GeIndex
  | GeTensor
  | GtIndex
  | GtTensor
  | --------------------------
    -- Reduction operations --
    --------------------------
    ReduceAnd
  | ReduceOr
  | ReduceSum
  | ----------------------
    -- Other operations --
    ----------------------
    Lookup
  | Stack Int
  | FoldList
  | FoldVector
  | MapList
  | MapVector
  | ZipWithVector
  | Indices
  | Optimise Bool
  | Implies
  | If
  | Forall
  | Exists
  deriving (Show, Eq, Generic)

instance Hashable TensorBuiltin

instance PrintableBuiltin TensorBuiltin where
  isCoercion :: TensorBuiltin -> Bool
  isCoercion = const False

  convertBuiltin :: Provenance -> TensorBuiltin -> V.Expr var V.Builtin
  convertBuiltin p b = case b of
    NilList -> builtinConstructor V.Nil
    ConsList -> builtinConstructor V.Cons
    BoolTensor x -> builtinConstructor $ V.LBool x
    NatTensor n -> builtinConstructor $ V.LNat n
    IntTensor i -> builtinConstructor $ V.LInt i
    RatTensor n d -> builtinConstructor $ V.LRat (fromIntegral n % fromIntegral d)
    Index i -> builtinConstructor $ V.LIndex i
    Stack n -> builtinConstructor (V.LVec n)
    Not -> builtinFunction V.Not
    And -> builtinFunction V.And
    Or -> builtinFunction V.Or
    Implies -> builtinFunction V.Implies
    Forall -> builtinFunction (V.Quantifier V.Forall)
    Exists -> builtinFunction (V.Quantifier V.Exists)
    If -> builtinFunction V.If
    Neg -> builtinTCOp V.NegTC
    Add -> builtinTCOp V.AddTC
    Sub -> builtinTCOp V.SubTC
    Mul -> builtinTCOp V.MulTC
    Div -> builtinTCOp V.DivTC
    PowRat -> builtinFunction V.PowRat
    MinRat -> builtinFunction V.MinRat
    MaxRat -> builtinFunction V.MaxRat
    EqIndex -> builtinFunction $ V.Equals V.EqIndex V.Eq
    EqTensor -> builtinTCOp $ V.EqualsTC V.Eq
    NeIndex -> builtinFunction $ V.Equals V.EqIndex V.Neq
    NeTensor -> builtinTCOp $ V.EqualsTC V.Neq
    LeIndex -> builtinFunction $ V.Order V.OrderIndex V.Le
    LeTensor -> builtinTCOp $ V.OrderTC V.Le
    LtIndex -> builtinFunction $ V.Order V.OrderIndex V.Lt
    LtTensor -> builtinTCOp $ V.OrderTC V.Lt
    GeIndex -> builtinFunction $ V.Order V.OrderIndex V.Ge
    GeTensor -> builtinTCOp $ V.OrderTC V.Ge
    GtIndex -> builtinFunction $ V.Order V.OrderIndex V.Gt
    GtTensor -> builtinTCOp $ V.OrderTC V.Gt
    Lookup -> builtinFunction V.At
    FoldList -> builtinFunction V.FoldList
    FoldVector -> builtinFunction V.FoldVector
    MapList -> builtinFunction V.MapList
    MapVector -> builtinFunction V.MapVector
    ZipWithVector -> builtinFunction V.ZipWithVector
    Indices -> builtinFunction V.Indices
    UnitType -> builtinType V.Unit
    IndexType -> builtinType V.Index
    TensorType -> V.FreeVar p (V.identifierOf StdTensor)
    ListType -> builtinType V.List
    ReduceAnd -> cheatConvertBuiltin p "ReduceAnd"
    ReduceOr -> cheatConvertBuiltin p "ReduceOr"
    ReduceSum -> cheatConvertBuiltin p "ReduceSum"
    Optimise minimise -> builtinFunction $ V.Optimise minimise
    where
      builtinConstructor = V.Builtin p . V.BuiltinConstructor
      builtinFunction = V.Builtin p . V.BuiltinFunction
      builtinTCOp = V.Builtin p . V.TypeClassOp
      builtinType = V.Builtin p . V.BuiltinType

arityOf :: TensorBuiltin -> Arity
arityOf b = case b of
  UnitType -> 0
  IndexType -> 0
  TensorType -> 0
  ListType -> 0
  NilList -> 0
  ConsList -> 2
  BoolTensor {} -> 0
  NatTensor {} -> 0
  IntTensor {} -> 0
  RatTensor {} -> 0
  Index {} -> 0
  Stack {} -> 1
  Not -> 1
  And -> 2
  Or -> 2
  Implies -> 2
  Forall -> 1
  Exists -> 1
  If -> 3
  Neg -> 1
  Add -> 2
  Sub -> 2
  Mul -> 2
  Div -> 2
  PowRat -> 2
  MinRat -> 2
  MaxRat -> 2
  EqIndex -> 2
  EqTensor -> 2
  NeIndex -> 2
  NeTensor -> 2
  LeIndex -> 2
  LeTensor -> 2
  LtIndex -> 2
  LtTensor -> 2
  GeIndex -> 2
  GeTensor -> 2
  GtIndex -> 2
  GtTensor -> 2
  Lookup -> 2
  FoldList -> 3
  FoldVector -> 2
  MapList -> 2
  MapVector -> 2
  ZipWithVector -> 3
  Indices -> 1
  ReduceAnd -> 1
  ReduceOr -> 1
  ReduceSum -> 1
  Optimise {} -> 2
