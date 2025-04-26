module Vehicle.Data.Builtin.Interface.Type where

import Data.Proxy (Proxy)
import Vehicle.Compile.Context.Free (MonadFreeContext)
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Code.DSL
import Vehicle.Data.Code.Expr (Type)
import Vehicle.Data.DSL
import Vehicle.Prelude (Provenance, Relevance (..))
import Prelude hiding (iterate)

class (PrintableBuiltin builtin) => TypableBuiltin builtin where
  -- | Construct a type for the builtin
  typeBuiltin :: (MonadFreeContext builtin m) => Provenance -> builtin -> m (Type builtin)

  -- | Can meta variables depend on other values in the scope?
  -- Efficiency hack for polarity/linearity subsystems.
  useDependentMetas :: Proxy builtin -> Bool

  -- | Is the builtin a constructor?
  isConstructor :: builtin -> Bool

  -- | Is the builtin a constraint for a casting operation (e.g. of literals, tensors etc.)
  isCastConstraint :: builtin -> Bool

typeOfBuiltinType :: (HasStandardBuiltins builtin) => BuiltinType -> DSLExpr builtin
typeOfBuiltinType = \case
  UnitType -> type0
  BoolType -> type0
  IndexType -> tNat .~> type0
  NatType -> type0
  RatType -> type0
  VectorType -> type0 ~> tDim .~> type0
  ListType -> type0 ~> type0
  TensorType -> type0 ~> tDims .~> type0

typeOfBuiltinFunction :: (HasStandardBuiltins builtin) => BuiltinFunction -> DSLExpr builtin
typeOfBuiltinFunction = \case
  -- Boolean operations
  Not -> typeOfTensorOp1 tBool
  And -> typeOfTensorOp2 tBool
  Or -> typeOfTensorOp2 tBool
  Implies -> typeOfTensorOp2 tBool
  QuantifyRatTensor _ -> forAllDims $ \ds -> typeOfQuantifier (tRatTensor ds)
  If -> typeOfIf
  ReduceAndTensor -> typeOfTensorBoolReduceOp
  ReduceOrTensor -> typeOfTensorBoolReduceOp
  -- Arithmetic operations
  Neg dom -> case dom of
    NegRatTensor -> typeOfTensorOp1 tRat
  Add dom -> case dom of
    AddNat -> tNat ~> tNat ~> tNat
    AddRatTensor -> typeOfTensorOp2 tRat
  Sub dom -> case dom of
    SubRatTensor -> typeOfTensorOp2 tRat
  Mul dom -> case dom of
    MulNat -> tNat ~> tNat ~> tNat
    MulRatTensor -> typeOfTensorOp2 tRat
  Div dom -> case dom of
    DivRatTensor -> typeOfTensorOp2 tRat
  Min dom -> case dom of
    MinRatTensor -> typeOfTensorOp2 tRat
  Max dom -> case dom of
    MaxRatTensor -> typeOfTensorOp2 tRat
  PowRat -> forAllDims $ \dims -> tRatTensor dims ~> tNat ~> tRatTensor dims
  ReduceAddRatTensor -> typeOfTensorRatReduceOp
  ReduceMulRatTensor -> typeOfTensorRatReduceOp
  ReduceMinRatTensor -> typeOfTensorRatReduceOp
  ReduceMaxRatTensor -> typeOfTensorRatReduceOp
  CompareIndex {} ->
    forAllIrrelevantNat "n1" $ \n1 ->
      forAllIrrelevantNat "n2" $ \n2 ->
        tIndex n1 ~> tIndex n2 ~> tBoolTensor dimNil
  CompareNat {} ->
    tNat ~> tNat ~> tBoolTensor dimNil
  CompareRatTensorPointwise {} ->
    forAllDims $ \dims ->
      tRatTensor dims ~> tRatTensor dims ~> tBoolTensor dims
  -- Container functions
  FoldList -> typeOfFold tListRaw
  MapList -> typeOfMap tListRaw
  At -> typeOfAt
  StackTensor -> typeOfStackTensor
  ConstTensor -> typeOfConstTensor
  Foreach -> typeOfForeach
  Iterate -> forAllTypes $ \t -> ((t ~> t) ~> t ~> t) ~> tNat ~> t

typeOfBuiltinConstructor :: (HasStandardBuiltins builtin) => BuiltinConstructor -> DSLExpr builtin
typeOfBuiltinConstructor = \case
  Nil -> typeOfNil
  Cons -> typeOfCons
  UnitLiteral -> tUnit
  IndexLiteral {} -> forAllIrrelevantNat "n" $ \n -> tIndex n
  NatLiteral {} -> tNat
  NatTensorLiteral t -> tNatTensor (shapeOf t)
  BoolTensorLiteral t -> tBoolTensor (shapeOf t)
  IndexTensorLiteral t -> forAllIrrelevantNat "n" $ \n -> tTensor (tIndex n) (shapeOf t)
  RatTensorLiteral t -> tRatTensor (shapeOf t)

typeOfTCOp1 :: (DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin) -> DSLExpr builtin
typeOfTCOp1 constraint =
  forAll "A" type0 $ \t1 ->
    forAll "B" type0 $ \t2 ->
      constraint t1 t2 ~~~> t1 ~> t2

typeOfTCOp2 :: (DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin) -> DSLExpr builtin
typeOfTCOp2 constraint =
  forAll "A" type0 $ \t1 ->
    forAll "B" type0 $ \t2 ->
      forAll "C" type0 $ \t3 ->
        constraint t1 t2 t3 ~~~> t1 ~> t2 ~> t3

typeOfTensorOp1 :: (BuiltinHasStandardTypes builtin) => DSLExpr builtin -> DSLExpr builtin
typeOfTensorOp1 tElem = forAllDims $ \dims -> tTensor tElem dims ~> tTensor tElem dims

typeOfTensorOp2 :: (BuiltinHasStandardTypes builtin) => DSLExpr builtin -> DSLExpr builtin
typeOfTensorOp2 tElem = forAllDims $ \dims -> tTensor tElem dims ~> tTensor tElem dims ~> tTensor tElem dims

typeOfConstTensor :: (HasStandardBuiltins builtin) => DSLExpr builtin
typeOfConstTensor =
  forAll "A" type0 $ \tElem ->
    tElem ~> tDims ~> tTensor tElem tDims

typeOfTensorReduceOp ::
  (BuiltinHasStandardTypes builtin, BuiltinHasStandardData builtin) =>
  DSLExpr builtin ->
  DSLExpr builtin
typeOfTensorReduceOp tElem =
  forAllDims $ \dims -> tTensor tElem dimNil ~> tTensor tElem dims ~> tTensor tElem dimNil

typeOfTensorRatReduceOp :: (BuiltinHasStandardTypes builtin, BuiltinHasStandardData builtin) => DSLExpr builtin
typeOfTensorRatReduceOp = typeOfTensorReduceOp tRat

typeOfTensorBoolReduceOp :: (BuiltinHasStandardTypes builtin, BuiltinHasStandardData builtin) => DSLExpr builtin
typeOfTensorBoolReduceOp = typeOfTensorReduceOp tBool

typeOfIf :: (BuiltinHasStandardTypes builtin, BuiltinHasStandardData builtin) => DSLExpr builtin
typeOfIf =
  forAll "A" type0 $ \t ->
    tBoolTensor dimNil ~> t ~> t ~> t

typeOfNil :: (HasStandardBuiltins builtin) => DSLExpr builtin
typeOfNil =
  forAll "A" type0 $ \tElem ->
    tList tElem

typeOfCons :: (HasStandardBuiltins builtin) => DSLExpr builtin
typeOfCons =
  forAll "A" type0 $ \tElem ->
    tElem ~> tList tElem ~> tList tElem

typeOfAt :: (HasStandardBuiltins builtin) => DSLExpr builtin
typeOfAt =
  forAll "A" type0 $ \tElem ->
    forAllDim Irrelevant $ \d ->
      forAllDims $ \ds ->
        tTensor tElem (dimCons d ds) ~> tIndex d ~> tTensor tElem ds

typeOfVecLiteralCast :: (HasStandardBuiltins builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
typeOfVecLiteralCast tCont tElem d =
  iterate type0 (\fn t -> tElem ~> fn @@ [t]) d tCont

typeOfStackTensor :: (HasStandardBuiltins builtin) => DSLExpr builtin
typeOfStackTensor =
  forAllTypes $ \t ->
    forAllDim Relevant $ \d ->
      forAllDims $ \ds ->
        typeOfVecLiteralCast (tTensor t (dimCons d ds)) (tTensor t ds) d

typeOfForeach :: (HasStandardBuiltins builtin) => DSLExpr builtin
typeOfForeach =
  forAll "A" type0 $ \tElem ->
    forAll "d" tDim $ \d ->
      forAllDims $ \ds ->
        (tIndex d ~> tTensor tElem ds) ~> tTensor tElem (dimCons d ds)

typeOfMap :: (HasStandardBuiltins builtin) => DSLExpr builtin -> DSLExpr builtin
typeOfMap f =
  forAll "A" type0 $ \a ->
    forAll "B" type0 $ \b ->
      (a ~> b) ~> f @@ [a] ~> f @@ [b]

typeOfFold :: (HasStandardBuiltins builtin) => DSLExpr builtin -> DSLExpr builtin
typeOfFold f =
  forAll "A" type0 $ \a ->
    forAll "B" type0 $ \b ->
      (a ~> b ~> b) ~> b ~> f @@ [a] ~> b

typeOfQuantifier :: (HasStandardBuiltins builtin) => DSLExpr builtin -> DSLExpr builtin
typeOfQuantifier t = (t ~> tBoolTensor dimNil) ~> tBoolTensor dimNil
