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

type TRational = (Int, Int)

convertTRat :: TRational -> Rational
convertTRat (n, d) = toInteger n % toInteger d

convertRat :: Rational -> TRational
convertRat r = do
  let num = toInt $ numerator r
  let denom = toInt $ denominator r
  (num, denom)
  where
    toInt x
      | x < toInteger (minBound :: Int) = developerError $ "Underflow converting" <+> pretty x <+> "to `Int`"
      | x > toInteger (maxBound :: Int) = developerError $ "Overflow converting" <+> pretty x <+> "to `Int`"
      | otherwise = fromInteger x

-- | The builtin types suitable for tensor operations.
data TensorBuiltin
  = ----------------------
    -- Types operations --
    ----------------------
    IndexType
  | BoolTensorType
  | IndexTensorType
  | NatTensorType
  | IntTensorType
  | RatTensorType
  | ListType
  | ------------------
    -- Constructors --
    ------------------
    Index Int
  | BoolTensor (Tensor Bool)
  | NatTensor (Tensor Int)
  | IntTensor (Tensor Int)
  | RatTensor (Tensor TRational)
  | NilList
  | ConsList
  | ---------------------------------
    -- Pointwise tensor operations --
    ---------------------------------
    PointwiseNot
  | PointwiseAnd
  | PointwiseOr
  | PointwiseNeg
  | PointwiseAdd
  | PointwiseSub
  | PointwiseMul
  | PointwiseDiv
  | PointwiseEq
  | PointwiseNe
  | PointwiseLe
  | PointwiseLt
  | PointwiseGe
  | PointwiseGt
  | ----------------------
    -- Index operations --
    ----------------------
    EqIndex
  | NeIndex
  | LeIndex
  | LtIndex
  | GeIndex
  | GtIndex
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
  | PowRat
  | MinRat
  | MaxRat
  | Stack Int
  | FoldList
  | FoldVector
  | MapList
  | MapVector
  | ZipWithVector
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
    IntTensorType -> builtinTensorType V.IntType
    RatTensorType -> builtinTensorType V.RatType
    -- Constructors
    Index i -> builtinConstructor $ V.LIndex i
    BoolTensor vs -> V.tensorToExpr (V.BoolLiteral mempty) vs
    NatTensor vs -> V.tensorToExpr (V.NatLiteral mempty) vs
    IntTensor vs -> V.tensorToExpr (V.IntLiteral mempty) vs
    RatTensor vs -> V.tensorToExpr (V.RatLiteral mempty . convertTRat) vs
    NilList -> builtinConstructor V.Nil
    ConsList -> builtinConstructor V.Cons
    Stack n -> builtinConstructor (V.LVec n)
    Forall -> builtinFunction (V.Quantifier V.Forall)
    Exists -> builtinFunction (V.Quantifier V.Exists)
    If -> builtinFunction V.If
    -- Tensor operations
    PointwiseNot -> builtinFunction V.Not
    PointwiseAnd -> builtinFunction V.And
    PointwiseOr -> builtinFunction V.Or
    PointwiseNeg -> builtinTCOp V.NegTC
    PointwiseAdd -> builtinTCOp V.AddTC
    PointwiseSub -> builtinTCOp V.SubTC
    PointwiseMul -> builtinTCOp V.MulTC
    PointwiseDiv -> builtinTCOp V.DivTC
    PointwiseEq -> builtinTCOp $ V.EqualsTC V.Eq
    PointwiseNe -> builtinTCOp $ V.EqualsTC V.Neq
    PointwiseLe -> builtinTCOp $ V.OrderTC V.Le
    PointwiseLt -> builtinTCOp $ V.OrderTC V.Lt
    PointwiseGe -> builtinTCOp $ V.OrderTC V.Ge
    PointwiseGt -> builtinTCOp $ V.OrderTC V.Gt
    PowRat -> builtinFunction V.PowRat
    MinRat -> builtinFunction V.MinRat
    MaxRat -> builtinFunction V.MaxRat
    GtIndex -> builtinFunction $ V.Order V.OrderIndex V.Gt
    GeIndex -> builtinFunction $ V.Order V.OrderIndex V.Ge
    LtIndex -> builtinFunction $ V.Order V.OrderIndex V.Lt
    LeIndex -> builtinFunction $ V.Order V.OrderIndex V.Le
    EqIndex -> builtinFunction $ V.Equals V.EqIndex V.Eq
    NeIndex -> builtinFunction $ V.Equals V.EqIndex V.Neq
    Lookup -> builtinFunction V.At
    FoldList -> builtinFunction V.FoldList
    FoldVector -> builtinFunction V.FoldVector
    MapList -> builtinFunction V.MapList
    MapVector -> builtinFunction V.MapVector
    ZipWithVector -> builtinFunction V.ZipWithVector
    Indices -> builtinFunction V.Indices
    IndexTensorType -> V.FreeVar p (V.identifierOf StdTensor)
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
      builtinTensorType t = V.StandardLib StdTensor [V.Arg mempty V.Explicit V.Relevant (t V.noProvenance)]

arityOf :: TensorBuiltin -> Arity
arityOf b = case b of
  IndexType -> 0
  BoolTensorType -> 0
  IndexTensorType -> 0
  NatTensorType -> 0
  IntTensorType -> 0
  RatTensorType -> 0
  ListType -> 0
  NilList -> 0
  ConsList -> 2
  BoolTensor {} -> 0
  NatTensor {} -> 0
  IntTensor {} -> 0
  RatTensor {} -> 0
  Index {} -> 0
  PointwiseNot -> 1
  PointwiseAnd -> 2
  PointwiseOr -> 2
  Forall -> 1
  Exists -> 1
  If -> 3
  PointwiseNeg -> 1
  PointwiseAdd -> 2
  PointwiseSub -> 2
  PointwiseMul -> 2
  PointwiseDiv -> 2
  PowRat -> 2
  MinRat -> 2
  MaxRat -> 2
  PointwiseEq -> 2
  PointwiseNe -> 2
  PointwiseLe -> 2
  PointwiseLt -> 2
  PointwiseGe -> 2
  PointwiseGt -> 2
  EqIndex -> 2
  NeIndex -> 2
  LeIndex -> 2
  LtIndex -> 2
  GeIndex -> 2
  GtIndex -> 2
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
  Stack n -> n

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

pattern VIntTensor :: Tensor Int -> NFValue TensorBuiltin
pattern VIntTensor t = VTensorLiteral (IntTensor t)

pattern VRatTensor :: Tensor TRational -> NFValue TensorBuiltin
pattern VRatTensor t = VTensorLiteral (RatTensor t)

getBoolTensor :: NFValue TensorBuiltin -> Maybe (Tensor Bool)
getBoolTensor (VBoolTensor t) = Just t
getBoolTensor _ = Nothing

getNatTensor :: NFValue TensorBuiltin -> Maybe (Tensor Int)
getNatTensor (VIntTensor t) = Just t
getNatTensor _ = Nothing

getIntTensor :: NFValue TensorBuiltin -> Maybe (Tensor Int)
getIntTensor (VIntTensor t) = Just t
getIntTensor _ = Nothing

getRatTensor :: NFValue TensorBuiltin -> Maybe (Tensor TRational)
getRatTensor (VRatTensor t) = Just t
getRatTensor _ = Nothing
