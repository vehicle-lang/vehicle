module Vehicle.Data.Builtin.Tensor where

import Data.Ratio
import GHC.Generics (Generic)
import Vehicle.Compile.Arity (Arity)
import Vehicle.Data.Builtin.Standard.Core qualified as S
import Vehicle.Data.Code.Expr (ConvertableBuiltin (..), Expr (..), PrintableBuiltin (..), cheatConvertBuiltin, normAppList)
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value
import Vehicle.Data.Tensor (Tensor (..))
import Vehicle.Libraries.StandardLibrary.Definitions (StdLibFunction (..))
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Builtin datatype

-- | Tensorflow doesn't support arbitrary precision integers. We should think
-- about this in the more future, about the actual precision the tensor backend
-- can represent rationals in, e.g. Storable.getSize, Haskell int64, etc.
type Rat = Ratio Int

convertTRat :: Rat -> Rational
convertTRat r = toInteger (numerator r) % toInteger (denominator r)

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

-- | The builtin types suitable for tensor operations.
data TensorBuiltin
  = ----------------------
    -- Types operations --
    ----------------------
    IndexType
  | BoolTensorType
  | IndexTensorType
  | NatType
  | RatTensorType
  | ListType
  | ------------------
    -- Constructors --
    ------------------
    Unit
  | Index Int
  | BoolTensor (Tensor Bool)
  | Nat Int
  | RatTensor (Tensor Rat)
  | NilList
  | ConsList
  | ---------------------------------
    -- Pointwise tensor operations --
    ---------------------------------
    NegRatTensor
  | AddRatTensor
  | SubRatTensor
  | MulRatTensor
  | DivRatTensor
  | EqRatTensor
  | NeRatTensor
  | LeRatTensor
  | LtRatTensor
  | GeRatTensor
  | GtRatTensor
  | PowRatTensor
  | MinRatTensor
  | MaxRatTensor
  | --------------------------
    -- Reduction operations --
    --------------------------
    ReduceSumRatTensor
  | ReduceRatTensor
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
    LookupRatTensor
  | StackRatTensor Int
  | ConstRatTensor Rat
  | FoldList
  | MapList
  | MapRatTensor
  | ZipWithRatTensor
  | Indices
  | SearchRatTensor [Name]
  | If
  | Forall
  | Exists
  deriving (Show, Eq, Generic)

-- instance Hashable TensorBuiltin

instance Pretty TensorBuiltin where
  pretty = pretty . show

instance ConvertableBuiltin TensorBuiltin S.Builtin where
  convertBuiltin :: Provenance -> TensorBuiltin -> Expr S.Builtin
  convertBuiltin p b = case b of
    IndexType -> builtinType S.Index
    BoolTensorType -> builtinTensorType (builtinType S.Bool)
    NatType -> builtinTensorType (builtinType S.Nat)
    RatTensorType -> builtinTensorType (builtinType S.Rat)
    IndexTensorType -> builtinTensorType (builtinType S.Nat)
    -- Constructors
    Unit -> builtinType S.Unit
    Index i -> builtinConstructor $ S.LIndex i
    BoolTensor vs -> tensorToExpr (IBoolLiteral mempty) vs
    Nat vs -> INatLiteral mempty vs
    RatTensor vs -> tensorToExpr (IRatLiteral mempty . convertTRat) vs
    NilList -> builtinConstructor S.Nil
    ConsList -> builtinConstructor S.Cons
    ConstRatTensor r -> cheatConvertBuiltin p $ "const[" <+> pretty (convertTRat r) <+> "]"
    StackRatTensor n -> builtinConstructor (S.LVec n)
    Forall -> builtinFunction (S.Quantifier S.Forall)
    Exists -> builtinFunction (S.Quantifier S.Exists)
    If -> builtinFunction S.If
    -- Tensor operations
    NegRatTensor -> builtinTCOp S.NegTC
    AddRatTensor -> builtinTCOp S.AddTC
    SubRatTensor -> builtinTCOp S.SubTC
    MulRatTensor -> builtinTCOp S.MulTC
    DivRatTensor -> builtinTCOp S.DivTC
    EqRatTensor -> builtinTCOp $ S.EqualsTC S.Eq
    NeRatTensor -> builtinTCOp $ S.EqualsTC S.Neq
    LeRatTensor -> builtinTCOp $ S.OrderTC S.Le
    LtRatTensor -> builtinTCOp $ S.OrderTC S.Lt
    GeRatTensor -> builtinTCOp $ S.OrderTC S.Ge
    GtRatTensor -> builtinTCOp $ S.OrderTC S.Gt
    PowRatTensor -> builtinFunction S.PowRat
    MinRatTensor -> builtinFunction S.MinRat
    MaxRatTensor -> builtinFunction S.MaxRat
    GtIndex -> builtinFunction $ S.Order S.OrderIndex S.Gt
    GeIndex -> builtinFunction $ S.Order S.OrderIndex S.Ge
    LtIndex -> builtinFunction $ S.Order S.OrderIndex S.Lt
    LeIndex -> builtinFunction $ S.Order S.OrderIndex S.Le
    EqIndex -> builtinFunction $ S.Equals S.EqIndex S.Eq
    NeIndex -> builtinFunction $ S.Equals S.EqIndex S.Neq
    LookupRatTensor -> builtinFunction S.At
    FoldList -> builtinFunction S.FoldList
    ReduceRatTensor -> builtinFunction S.FoldVector
    MapList -> builtinFunction S.MapList
    MapRatTensor -> builtinFunction S.MapVector
    ZipWithRatTensor -> builtinFunction S.ZipWithVector
    Indices -> builtinFunction S.Indices
    ListType -> builtinType S.List
    ReduceSumRatTensor -> cheatConvertBuiltin p "reduceSum"
    SearchRatTensor {} -> cheatConvertBuiltin p "search"
    where
      builtinConstructor = Builtin p . S.BuiltinConstructor
      builtinFunction = Builtin p . S.BuiltinFunction
      builtinTCOp = Builtin p . S.TypeClassOp
      builtinType = Builtin p . S.BuiltinType
      builtinTensorType t = normAppList (FreeVar p (identifierOf StdTensor)) [Arg mempty Explicit Relevant t]

instance PrintableBuiltin TensorBuiltin where
  isCoercion :: TensorBuiltin -> Bool
  isCoercion = const False

arityOf :: TensorBuiltin -> Arity
arityOf b = case b of
  IndexType -> 0
  BoolTensorType -> 0
  IndexTensorType -> 0
  NatType -> 0
  RatTensorType -> 0
  ListType -> 0
  NilList -> 0
  ConsList -> 2
  BoolTensor {} -> 0
  Nat {} -> 0
  RatTensor {} -> 0
  Unit -> 0
  Index {} -> 0
  Forall -> 1
  Exists -> 1
  If -> 3
  NegRatTensor -> 1
  AddRatTensor -> 2
  SubRatTensor -> 2
  MulRatTensor -> 2
  DivRatTensor -> 2
  PowRatTensor -> 2
  MinRatTensor -> 2
  MaxRatTensor -> 2
  EqRatTensor -> 2
  NeRatTensor -> 2
  LeRatTensor -> 2
  LtRatTensor -> 2
  GeRatTensor -> 2
  GtRatTensor -> 2
  EqIndex -> 2
  NeIndex -> 2
  LeIndex -> 2
  LtIndex -> 2
  GeIndex -> 2
  GtIndex -> 2
  LookupRatTensor -> 2
  ConstRatTensor {} -> 1
  FoldList -> 3
  ReduceRatTensor -> 2
  MapList -> 2
  MapRatTensor -> 2
  ZipWithRatTensor -> 3
  Indices -> 1
  ReduceSumRatTensor -> 1
  SearchRatTensor {} -> 2
  StackRatTensor n -> n

--------------------------------------------------------------------------------
-- Tensor literals

pattern VTensorLiteral :: TensorBuiltin -> NFValue TensorBuiltin
pattern VTensorLiteral c <- VBuiltin c []
  where
    -- Can't be bidirectional as Haskell 8.10.7 doesn't support the empty list literal being bidirectional.
    VTensorLiteral c = VBuiltin c []

pattern VBoolTensor :: Tensor Bool -> NFValue TensorBuiltin
pattern VBoolTensor t = VTensorLiteral (BoolTensor t)

pattern VRatTensor :: Tensor Rat -> NFValue TensorBuiltin
pattern VRatTensor t = VTensorLiteral (RatTensor t)

pattern VLookup :: NFArg TensorBuiltin -> NFValue TensorBuiltin -> NFValue TensorBuiltin
pattern VLookup xs i <- VBuiltin LookupRatTensor [xs, RelevantExplicitArg _ i]

getBoolTensor :: NFValue TensorBuiltin -> Maybe (Tensor Bool)
getBoolTensor (VBoolTensor t) = Just t
getBoolTensor _ = Nothing

getRatTensor :: NFValue TensorBuiltin -> Maybe (Tensor Rat)
getRatTensor (VRatTensor t) = Just t
getRatTensor _ = Nothing
