module Vehicle.Data.Builtin.Interface.Type where

import Data.Proxy (Proxy)
import Data.Set (Set)
import Data.Set qualified as Set
import Vehicle.Compile.Type.Core (InstanceHead)
import Vehicle.Compile.Type.Monad.Class (MonadTypeChecker)
import Vehicle.Data.AST.Expr.Scoped (Type)
import Vehicle.Data.AST.Name (Identifier, stdlibIdentifier)
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Builtin.Interface.Normalise (NormalisableBuiltin)
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Code.DSL
import Vehicle.Data.DSL
import Vehicle.Prelude (Provenance, Relevance (..))
import Prelude hiding (iterate)

class (NormalisableBuiltin builtin, Ord builtin) => TypableBuiltin builtin where
  -- | Construct a type for the builtin
  typeBuiltin :: (MonadTypeChecker builtin m) => Provenance -> builtin -> m (Type builtin)

  -- | Can meta variables depend on other values in the scope?
  -- Efficiency hack for polarity/linearity subsystems.
  useDependentMetas :: Proxy builtin -> Bool

  -- | Is the builtin a constructor?
  isConstructor :: builtin -> Bool

  -- | Is the builtin a constraint for a casting operation (e.g. of literals, tensors etc.)
  isCastConstraint :: InstanceHead builtin -> Bool

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
  TimeType -> type0

typeOfBuiltinFunction :: (HasStandardBuiltins builtin, BuiltinHasTimeLiterals builtin) => BuiltinFunction -> DSLExpr builtin
typeOfBuiltinFunction = \case
  -- Boolean operations
  Not -> typeOfTensorOp1 tBool
  And -> typeOfTensorOp2 tBool
  Or -> typeOfTensorOp2 tBool
  Implies -> typeOfTensorOp2 tBool
  Temporal op -> case op of
    Globally -> typeOfTemporalOp1
    Finally -> typeOfTemporalOp1
    Until -> typeOfTemporalOp2
  QuantifyRatTensor _ -> forAllDims $ \ds -> typeOfQuantifier (tRatTensor ds)
  QuantifyTensorLike _ -> forAllTypes $ \ts -> typeOfQuantifier ts
  If -> typeOfIf
  ReduceAndTensor -> typeOfTensorBoolReduceOp
  ReduceOrTensor -> typeOfTensorBoolReduceOp
  -- Arithmetic operations
  Neg dom -> case dom of
    NegRatTensor -> typeOfTensorOp1 tRat
  Add dom -> case dom of
    AddNat -> tNat ~> tNat ~> tNat
    AddRatTensor -> typeOfTensorOp2 tRat
    AddTime -> tTime ~> tTime ~> tTime
  Sub dom -> case dom of
    SubRatTensor -> typeOfTensorOp2 tRat
    SubTime -> tTime ~> tTime ~> tTime
  Mul dom -> case dom of
    MulNat -> tNat ~> tNat ~> tNat
    MulRatTensor -> typeOfTensorOp2 tRat
    MulTime -> tTime ~> tTime ~> tTime
  Div dom -> case dom of
    DivRatTensor -> typeOfTensorOp2 tRat
    DivTime -> tTime ~> tTime ~> tTime
  Min dom -> case dom of
    MinRatTensor -> typeOfTensorOp2 tRat
  Max dom -> case dom of
    MaxRatTensor -> typeOfTensorOp2 tRat
  PowRat -> typeOfTensorOp2 tRat
  ExpRat -> typeOfTensorOp1 tRat
  LogRat -> typeOfTensorOp2 tRat
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
  AtVector -> typeOfAtVector
  AtTensor -> typeOfAtTensor
  StackTensor -> typeOfStackTensor
  ConstTensor -> typeOfConstTensor
  ForeachTensor -> typeOfForeachTensor
  ForeachVector -> typeOfForeachVector
  Iterate -> forAllTypes $ \t -> ((t ~> t) ~> t ~> t) ~> tNat ~> t
  Rollout -> typeOfRollout
  Transpose -> typeOfTranspose

-- | Stdlib decls referenced by a builtin's type signature (via `standardLib`
-- in the DSL). Invisible to the AST dependency graph, so callers that prune
-- unused decls before secondary-subsystem typecheck need them as extra roots.
typeBuiltinTypeLevelDeps :: BuiltinFunction -> Set Identifier
typeBuiltinTypeLevelDeps = \case
  Transpose -> Set.singleton (stdlibIdentifier "reverse")
  _ -> Set.empty

typeOfBuiltinConstructor :: (HasStandardBuiltins builtin) => BuiltinConstructor -> DSLExpr builtin
typeOfBuiltinConstructor = \case
  Nil -> typeOfNil
  Cons -> typeOfCons
  UnitLiteral -> tUnit
  IndexLiteral {} -> forAllIrrelevantNat "n" $ \n -> tIndex n
  NatLiteral {} -> tNat
  VectorLiteral {} -> typeOfVecLiteral
  NatTensorLiteral t -> tNatTensor (shapeOf t)
  BoolTensorLiteral t -> tBoolTensor (shapeOf t)
  RatTensorLiteral t -> tRatTensor (shapeOf t)
  TimeLiteral {} -> tTime

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

-- | (interval start) -> (interval end) -> signal -> signal, of matching shape.
typeOfTemporalOp1 :: (BuiltinHasStandardTypes builtin) => DSLExpr builtin
typeOfTemporalOp1 = forAllDims $ \dims -> tTime ~> tTime ~> tBoolTensor dims ~> tBoolTensor dims

typeOfTemporalOp2 :: (BuiltinHasStandardTypes builtin) => DSLExpr builtin
typeOfTemporalOp2 = forAllDims $ \dims -> tTime ~> tTime ~> tBoolTensor dims ~> tBoolTensor dims ~> tBoolTensor dims

typeOfConstTensor :: (HasStandardBuiltins builtin) => DSLExpr builtin
typeOfConstTensor =
  forAll "A" type0 $ \tElem ->
    tTensor tElem dimNil ~> forAllExpl "dims" tDims (\dims -> tTensor tElem dims)

-- | Split-axes reduction: input shape `keepDs ++ reduceDs`, output shape `keepDs`.
-- Source-level total reductions unify `keepDs` to `dimNil`.
typeOfTensorReduceOp ::
  (BuiltinHasStandardTypes builtin, BuiltinHasStandardData builtin) =>
  DSLExpr builtin ->
  DSLExpr builtin
typeOfTensorReduceOp tElem =
  forAllDims $ \reduceDs ->
    forAllDims $ \keepDs ->
      tTensor tElem dimNil ~> tTensor tElem reduceDs ~> tTensor tElem keepDs

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

typeOfAtVector :: (HasStandardBuiltins builtin) => DSLExpr builtin
typeOfAtVector =
  forAll "A" type0 $ \tElem ->
    forAllDim Irrelevant $ \d ->
      tVector tElem d ~> tIndex d ~> tElem

typeOfAtTensor :: (HasStandardBuiltins builtin) => DSLExpr builtin
typeOfAtTensor =
  forAll "A" type0 $ \tElem ->
    forAllDim Irrelevant $ \d ->
      forAllDims $ \ds ->
        tTensor tElem (dimCons d ds) ~> tIndex d ~> tTensor tElem ds

-- | transpose : forall A {ds} . Tensor A ds -> Tensor A (reverseDims ds)
typeOfTranspose :: (HasStandardBuiltins builtin) => DSLExpr builtin
typeOfTranspose =
  forAll "A" type0 $ \tElem ->
    forAllDims $ \ds ->
      tTensor tElem ds ~> tTensor tElem (reverseDims ds)

typeOfVecLiteralCast :: (HasStandardBuiltins builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
typeOfVecLiteralCast tCont tElem d =
  iterate type0 (\fn t -> tElem ~> fn @@ [t]) d tCont

typeOfVecLiteral :: (HasStandardBuiltins builtin) => DSLExpr builtin
typeOfVecLiteral =
  forAllTypes $ \t ->
    forAllDim Relevant $ \d ->
      typeOfVecLiteralCast (tVector t d) t d

typeOfStackTensor :: (HasStandardBuiltins builtin) => DSLExpr builtin
typeOfStackTensor =
  forAllTypes $ \t ->
    forAllDim Relevant $ \d ->
      forAllDims $ \ds ->
        typeOfVecLiteralCast (tTensor t (dimCons d ds)) (tTensor t ds) d

typeOfForeach :: DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
typeOfForeach tCont tInd tElem =
  (tInd ~> tElem) ~> tCont

typeOfForeachTensor :: (HasStandardBuiltins builtin) => DSLExpr builtin
typeOfForeachTensor =
  forAll "A" type0 $ \tElem ->
    forAll "d" tDim $ \d ->
      forAllDims $ \ds ->
        typeOfForeach (tTensor tElem (dimCons d ds)) (tIndex d) (tTensor tElem ds)

typeOfForeachVector :: (HasStandardBuiltins builtin) => DSLExpr builtin
typeOfForeachVector =
  forAll "A" type0 $ \tElem ->
    forAll "d" tDim $ \d ->
      typeOfForeach (tVector tElem d) (tIndex d) tElem

-- | rollout : forall S A {ds da}
--     . (n : Time) -> (S -> A) -> (S -> A -> S) -> S
--     -> Tensor S (fromTimeToNat n :: ds)
typeOfRollout :: (HasStandardBuiltins builtin, BuiltinHasTimeLiterals builtin) => DSLExpr builtin
typeOfRollout =
  forAll "S" type0 $ \tState ->
    forAll "A" type0 $ \tAction ->
      forAllIrrelevant "ds" tDims $ \dimsS ->
        forAllIrrelevant "da" tDims $ \dimsA ->
          forAllExpl "n" tTime $ \n ->
            let stateT = tTensor tState dimsS
                actionT = tTensor tAction dimsA
             in (stateT ~> actionT) ~> (stateT ~> actionT ~> stateT) ~> stateT ~> tTensor tState (dimCons (fromTimeToNat n) dimsS)

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
