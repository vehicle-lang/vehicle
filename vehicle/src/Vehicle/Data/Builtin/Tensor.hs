module Vehicle.Data.Builtin.Tensor where

import Data.Ratio
import GHC.Generics (Generic)
import Vehicle.Compile.Arity (Arity)
import Vehicle.Data.Builtin.Interface (ConvertableBuiltin (..), PrintableBuiltin (..))
import Vehicle.Data.Builtin.Standard.Core ()
import Vehicle.Data.Expr.Interface
import Vehicle.Data.Expr.Normalised
import Vehicle.Data.Expr.Standard (cheatConvertBuiltin)
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
  | SearchRatTensor [V.Name]
  | If
  | Forall
  | Exists
  deriving (Show, Eq, Generic)

-- instance Hashable TensorBuiltin

instance Pretty TensorBuiltin where
  pretty = pretty . show

instance ConvertableBuiltin TensorBuiltin V.Builtin where
  convertBuiltin :: Provenance -> TensorBuiltin -> V.Expr var V.Builtin
  convertBuiltin p b = case b of
    IndexType -> builtinType V.Index
    BoolTensorType -> builtinTensorType IBoolType
    NatType -> builtinTensorType INatType
    RatTensorType -> builtinTensorType IRatType
    -- Constructors
    Unit -> IUnitLiteral mempty
    Index i -> builtinConstructor $ V.LIndex i
    BoolTensor vs -> tensorToExpr (IBoolLiteral mempty) vs
    Nat vs -> INatLiteral mempty vs
    RatTensor vs -> tensorToExpr (IRatLiteral mempty . convertTRat) vs
    NilList -> builtinConstructor V.Nil
    ConsList -> builtinConstructor V.Cons
    ConstRatTensor r -> cheatConvertBuiltin p $ "const[" <+> pretty (convertTRat r) <+> "]"
    StackRatTensor n -> builtinConstructor (V.LVec n)
    Forall -> builtinFunction (V.Quantifier V.Forall)
    Exists -> builtinFunction (V.Quantifier V.Exists)
    If -> builtinFunction V.If
    -- Tensor operations
    NegRatTensor -> builtinTCOp V.NegTC
    AddRatTensor -> builtinTCOp V.AddTC
    SubRatTensor -> builtinTCOp V.SubTC
    MulRatTensor -> builtinTCOp V.MulTC
    DivRatTensor -> builtinTCOp V.DivTC
    EqRatTensor -> builtinTCOp $ V.EqualsTC V.Eq
    NeRatTensor -> builtinTCOp $ V.EqualsTC V.Neq
    LeRatTensor -> builtinTCOp $ V.OrderTC V.Le
    LtRatTensor -> builtinTCOp $ V.OrderTC V.Lt
    GeRatTensor -> builtinTCOp $ V.OrderTC V.Ge
    GtRatTensor -> builtinTCOp $ V.OrderTC V.Gt
    PowRatTensor -> builtinFunction V.PowRat
    MinRatTensor -> builtinFunction V.MinRat
    MaxRatTensor -> builtinFunction V.MaxRat
    GtIndex -> builtinFunction $ V.Order V.OrderIndex V.Gt
    GeIndex -> builtinFunction $ V.Order V.OrderIndex V.Ge
    LtIndex -> builtinFunction $ V.Order V.OrderIndex V.Lt
    LeIndex -> builtinFunction $ V.Order V.OrderIndex V.Le
    EqIndex -> builtinFunction $ V.Equals V.EqIndex V.Eq
    NeIndex -> builtinFunction $ V.Equals V.EqIndex V.Neq
    LookupRatTensor -> builtinFunction V.At
    FoldList -> builtinFunction V.FoldList
    ReduceRatTensor -> builtinFunction V.FoldVector
    MapList -> builtinFunction V.MapList
    MapRatTensor -> builtinFunction V.MapVector
    ZipWithRatTensor -> builtinFunction V.ZipWithVector
    Indices -> builtinFunction V.Indices
    IndexTensorType -> V.FreeVar p (V.identifierOf StdTensor)
    ListType -> builtinType V.List
    ReduceSumRatTensor -> cheatConvertBuiltin p "reduceSum"
    SearchRatTensor {} -> cheatConvertBuiltin p "search"
    where
      builtinConstructor = V.Builtin p . V.BuiltinConstructor
      builtinFunction = V.Builtin p . V.BuiltinFunction
      builtinTCOp = V.Builtin p . V.TypeClassOp
      builtinType = V.Builtin p . V.BuiltinType
      builtinTensorType t = IStandardLib StdTensor [V.Arg mempty V.Explicit V.Relevant (t V.noProvenance)]

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
pattern VLookup xs i <- VBuiltin LookupRatTensor [xs, V.RelevantExplicitArg _ i]

getBoolTensor :: NFValue TensorBuiltin -> Maybe (Tensor Bool)
getBoolTensor (VBoolTensor t) = Just t
getBoolTensor _ = Nothing

getRatTensor :: NFValue TensorBuiltin -> Maybe (Tensor Rat)
getRatTensor (VRatTensor t) = Just t
getRatTensor _ = Nothing
