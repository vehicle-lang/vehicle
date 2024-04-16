module Vehicle.Backend.Tensors.Core where

import Data.Ratio
import GHC.Generics (Generic)
import Vehicle.Compile.Arity (Arity)
import Vehicle.Data.BuiltinInterface (PrintableBuiltin (..), cheatConvertBuiltin)
import Vehicle.Data.BuiltinInterface.Expr qualified as V
import Vehicle.Data.NormalisedExpr
import Vehicle.Data.Tensor (Tensor (..))
import Vehicle.Libraries.StandardLibrary.Definitions (StdLibFunction (..))
import Vehicle.Prelude.Prettyprinter
import Vehicle.Syntax.AST (Provenance (..))
import Vehicle.Syntax.AST qualified as V
import Vehicle.Syntax.Builtin qualified as V
import Vehicle.Syntax.Prelude (developerError)

--------------------------------------------------------------------------------
-- Builtin datatype

-- | Tensorflow doesn't support arbitrary precision integers. We should think
-- about this in the more future, about the actual precision the tensor backend
-- can represent rationals in, e.g. Storable.getSize, Haskell int64, etc.
type Rat = Ratio Int

convertTRat :: Rat -> Rational
convertTRat r = (toInteger $ numerator r) % (toInteger $ denominator r)

convertRat :: Rational -> Rat
convertRat r = do
  let num = toInt $ numerator r
  let denom = toInt $ denominator r
  num % denom
  where
    toInt x
      | x < toInteger (minBound :: Int) = developerError $ "Underflow converting" <+> pretty x <+> "to `Int`"
      | x > toInteger (maxBound :: Int) = developerError $ "Overflow converting" <+> pretty x <+> "to `Int`"
      | otherwise = fromInteger x

-- data MinimiseOrMaximise = Minimise | Maximise

-- | The builtin types suitable for tensor operations.
data TensorBuiltin
  = ----------------------
    -- Types operations --
    ----------------------
    IndexType
  | BoolTensorType
  | IndexTensorType
  | NatTensorType
  | RatTensorType
  | ListType
  | ------------------
    -- Constructors --
    ------------------
    Unit
  | Index Int
  | BoolTensor (Tensor Bool)
  | NatTensor (Tensor Int)
  | RatTensor (Tensor Rat)
  | NilList
  | ConsList
  | ---------------------------------
    -- Pointwise tensor operations --
    ---------------------------------
    NotTensor
  | AndTensor
  | OrTensor
  | NegTensor
  | AddTensor
  | SubTensor
  | MulTensor
  | DivTensor
  | EqTensor
  | NeTensor
  | LeTensor
  | LtTensor
  | GeTensor
  | GtTensor
  | PowRatTensor
  | MinRatTensor
  | MaxRatTensor
  | --------------------------
    -- Reduction operations --
    --------------------------
    ReduceAndTensor
  | ReduceOrTensor
  | ReduceSumTensor
  | ReduceTensor
  | ----------------------
    -- Index operations --
    ----------------------
    EqIndex
  | NeIndex
  | LeIndex
  | LtIndex
  | GeIndex
  | GtIndex
  | ----------------------
    -- Other operations --
    ----------------------
    LookupTensor
  | StackTensor Int
  | ConstTensor Rat
  | FoldList
  | MapList
  | MapTensor
  | ZipWithTensor
  | Indices
  | Optimise Bool
  | If
  | Forall
  | Exists
  deriving (Show, Eq, Generic)

-- instance Hashable TensorBuiltin

instance Pretty TensorBuiltin where
  pretty = pretty . show

instance PrintableBuiltin TensorBuiltin where
  isCoercion :: TensorBuiltin -> Bool
  isCoercion = const False

  convertBuiltin :: Provenance -> TensorBuiltin -> V.Expr var V.Builtin
  convertBuiltin p b = case b of
    IndexType -> builtinType V.Index
    BoolTensorType -> builtinTensorType V.BoolType
    NatTensorType -> builtinTensorType V.NatType
    RatTensorType -> builtinTensorType V.RatType
    -- Constructors
    Unit -> V.UnitLiteral mempty
    Index i -> builtinConstructor $ V.LIndex i
    BoolTensor vs -> V.tensorToExpr (V.BoolLiteral mempty) vs
    NatTensor vs -> V.tensorToExpr (V.NatLiteral mempty) vs
    RatTensor vs -> V.tensorToExpr (V.RatLiteral mempty . convertTRat) vs
    NilList -> builtinConstructor V.Nil
    ConsList -> builtinConstructor V.Cons
    ConstTensor r -> cheatConvertBuiltin p $ "Const[" <+> pretty (convertTRat r) <+> "]"
    StackTensor n -> builtinConstructor (V.LVec n)
    Forall -> builtinFunction (V.Quantifier V.Forall)
    Exists -> builtinFunction (V.Quantifier V.Exists)
    If -> builtinFunction V.If
    -- Tensor operations
    NotTensor -> builtinFunction V.Not
    AndTensor -> builtinFunction V.And
    OrTensor -> builtinFunction V.Or
    NegTensor -> builtinTCOp V.NegTC
    AddTensor -> builtinTCOp V.AddTC
    SubTensor -> builtinTCOp V.SubTC
    MulTensor -> builtinTCOp V.MulTC
    DivTensor -> builtinTCOp V.DivTC
    EqTensor -> builtinTCOp $ V.EqualsTC V.Eq
    NeTensor -> builtinTCOp $ V.EqualsTC V.Neq
    LeTensor -> builtinTCOp $ V.OrderTC V.Le
    LtTensor -> builtinTCOp $ V.OrderTC V.Lt
    GeTensor -> builtinTCOp $ V.OrderTC V.Ge
    GtTensor -> builtinTCOp $ V.OrderTC V.Gt
    PowRatTensor -> builtinFunction V.PowRat
    MinRatTensor -> builtinFunction V.MinRat
    MaxRatTensor -> builtinFunction V.MaxRat
    GtIndex -> builtinFunction $ V.Order V.OrderIndex V.Gt
    GeIndex -> builtinFunction $ V.Order V.OrderIndex V.Ge
    LtIndex -> builtinFunction $ V.Order V.OrderIndex V.Lt
    LeIndex -> builtinFunction $ V.Order V.OrderIndex V.Le
    EqIndex -> builtinFunction $ V.Equals V.EqIndex V.Eq
    NeIndex -> builtinFunction $ V.Equals V.EqIndex V.Neq
    LookupTensor -> builtinFunction V.At
    FoldList -> builtinFunction V.FoldList
    ReduceTensor -> builtinFunction V.FoldVector
    MapList -> builtinFunction V.MapList
    MapTensor -> builtinFunction V.MapVector
    ZipWithTensor -> builtinFunction V.ZipWithVector
    Indices -> builtinFunction V.Indices
    IndexTensorType -> V.FreeVar p (V.identifierOf StdTensor)
    ListType -> builtinType V.List
    ReduceAndTensor -> cheatConvertBuiltin p "ReduceAnd"
    ReduceOrTensor -> cheatConvertBuiltin p "ReduceOr"
    ReduceSumTensor -> cheatConvertBuiltin p "ReduceSum"
    Optimise minimise -> builtinFunction $ V.Optimise minimise
    where
      builtinConstructor = V.Builtin p . V.BuiltinConstructor
      builtinFunction = V.Builtin p . V.BuiltinFunction
      builtinTCOp = V.Builtin p . V.TypeClassOp
      builtinType = V.Builtin p . V.BuiltinType
      builtinTensorType t = V.StandardLib StdTensor [V.Arg mempty V.Explicit V.Relevant (t V.noProvenance)]

arityOf :: TensorBuiltin -> Arity
arityOf b = case b of
  IndexType -> 0
  BoolTensorType -> 0
  IndexTensorType -> 0
  NatTensorType -> 0
  RatTensorType -> 0
  ListType -> 0
  NilList -> 0
  ConsList -> 2
  BoolTensor {} -> 0
  NatTensor {} -> 0
  RatTensor {} -> 0
  Unit -> 0
  Index {} -> 0
  NotTensor -> 1
  AndTensor -> 2
  OrTensor -> 2
  Forall -> 1
  Exists -> 1
  If -> 3
  NegTensor -> 1
  AddTensor -> 2
  SubTensor -> 2
  MulTensor -> 2
  DivTensor -> 2
  PowRatTensor -> 2
  MinRatTensor -> 2
  MaxRatTensor -> 2
  EqTensor -> 2
  NeTensor -> 2
  LeTensor -> 2
  LtTensor -> 2
  GeTensor -> 2
  GtTensor -> 2
  EqIndex -> 2
  NeIndex -> 2
  LeIndex -> 2
  LtIndex -> 2
  GeIndex -> 2
  GtIndex -> 2
  LookupTensor -> 2
  ConstTensor {} -> 0
  FoldList -> 3
  ReduceTensor -> 2
  MapList -> 2
  MapTensor -> 2
  ZipWithTensor -> 3
  Indices -> 1
  ReduceAndTensor -> 1
  ReduceOrTensor -> 1
  ReduceSumTensor -> 1
  Optimise {} -> 2
  StackTensor n -> n

--------------------------------------------------------------------------------
-- Tensor literals

pattern VTensorLiteral :: TensorBuiltin -> NFValue TensorBuiltin
pattern VTensorLiteral c <- VBuiltin c []
  where
    -- Can't be bidirectional as Haskell 8.10.7 doesn't support the empty list literal being bidirectional.
    VTensorLiteral c = VBuiltin c []

pattern VBoolTensor :: Tensor Bool -> NFValue TensorBuiltin
pattern VBoolTensor t = VTensorLiteral (BoolTensor t)

pattern VNatTensor :: Tensor Int -> NFValue TensorBuiltin
pattern VNatTensor t = VTensorLiteral (NatTensor t)

pattern VRatTensor :: Tensor Rat -> NFValue TensorBuiltin
pattern VRatTensor t = VTensorLiteral (RatTensor t)

pattern VLookup :: NFArg TensorBuiltin -> NFValue TensorBuiltin -> NFValue TensorBuiltin
pattern VLookup xs i <- VBuiltin LookupTensor [xs, V.RelevantExplicitArg _ i]

getBoolTensor :: NFValue TensorBuiltin -> Maybe (Tensor Bool)
getBoolTensor (VBoolTensor t) = Just t
getBoolTensor _ = Nothing

getNatTensor :: NFValue TensorBuiltin -> Maybe (Tensor Int)
getNatTensor (VNatTensor t) = Just t
getNatTensor _ = Nothing

getRatTensor :: NFValue TensorBuiltin -> Maybe (Tensor Rat)
getRatTensor (VRatTensor t) = Just t
getRatTensor _ = Nothing
