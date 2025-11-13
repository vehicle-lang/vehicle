module Vehicle.Data.Code.TypedView
  ( TypeValue (..),
    toTypeValue,
    fromTypeValue,
    IndexValue (..),
    toIndexValue,
    NatValue (..),
    toNatValue,
    fromNatValue,
    VectorValue (..),
    toVectorValue,
    BoolValue (..),
    toBoolValue,
    fromBoolValue,
    BoolTensorValue (..),
    toBoolTensorValue,
    fromBoolTensorValue,
    MultiDimBoolTensorValue (..),
    toMultiDimBoolTensorValue,
    fromMultiDimBoolTensorValue,
    RatTensorValue (..),
    toRatTensorValue,
    fromRatTensorValue,
    DimensionsValue (..),
    toDimensionsValue,
    fromDimensionsValue,
    evalCompareRatTensor,
    etaReduceTensor,
    scaleValue,
    addValues,
  )
where

import GHC.Stack (HasCallStack)
import Vehicle.Compile.Normalise.NBE (normaliseBuiltin)
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Data.Builtin.Interface (Accessor (..), BuiltinHasIndexLiterals, BuiltinHasListLiterals, BuiltinHasNatLiterals, BuiltinHasNatType, BuiltinHasTensors)
import Vehicle.Data.Builtin.Interface.Normalise (EvalSimple, HasTensorLiterals, MonadNormBuiltin, evalAddRatTensor, evalCompareRatTensorPointwise, evalConstTensor, evalMulRatTensor, unoptimisedEvalAtTensor)
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Builtin.Standard.Normalise ()
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.Code.Value
import Vehicle.Data.Tensor (Tensor, pattern ZeroDimTensor)
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Data.Variable.Free.Context (MonadFreeContext)
import Vehicle.Prelude
import Vehicle.Prelude.Logging

-------------------------------------------------------------------------------
-- Types

-- | A view on all possible expressions that can have type `List Int`.
data TypeValue
  = VUnitType
  | VBoolType
  | VIndexType (Value Builtin)
  | VNatType
  | VRatType
  | VBoolTensorType (VDims Builtin)
  | VNatTensorType (VDims Builtin)
  | VRatTensorType (VDims Builtin)
  | VIndexTensorType (Value Builtin) (Value Builtin)
  | VListType (Value Builtin)
  | VVectorType (Value Builtin) (Value Builtin)
  | VPiType (VBinder Builtin) (Closure Builtin)
  | VBoundTypeVar Lv (Spine Builtin)
  | VFreeTypeVar Identifier (Spine Builtin)

toTypeValue :: (HasCallStack) => Value Builtin -> TypeValue
toTypeValue t = case t of
  VPi binder value -> VPiType binder value
  VBoundVar lv spine -> VBoundTypeVar lv spine
  VFreeVar v spine -> VFreeTypeVar v spine
  VBuiltin (BuiltinType typ) spine -> case (typ, spine) of
    (UnitType, []) -> VUnitType
    (BoolType, []) -> VBoolType
    (RatType, []) -> VRatType
    (IndexType, [n]) -> VIndexType (argExpr n)
    (NatType, []) -> VNatType
    (ListType, [tElem]) -> VListType (argExpr tElem)
    (TensorType, [toTypeValue . argExpr -> VBoolType, ds]) -> VBoolTensorType (argExpr ds)
    (TensorType, [toTypeValue . argExpr -> VRatType, ds]) -> VRatTensorType (argExpr ds)
    (TensorType, [toTypeValue . argExpr -> VNatType, ds]) -> VNatTensorType (argExpr ds)
    (TensorType, [toTypeValue . argExpr -> VIndexType n, ds]) -> VIndexTensorType n (argExpr ds)
    (VectorType, [tElem, dim]) -> VVectorType (argExpr tElem) (argExpr dim)
    _ -> err
  _ -> err
  where
    err = developerError $ "ill-typed type" <+> prettyVerbose t

fromTypeValue :: (HasCallStack) => TypeValue -> Value Builtin
fromTypeValue t = case t of
  VPiType binder value -> VPi binder value
  VBoundTypeVar lv spine -> VBoundVar lv spine
  VFreeTypeVar v spine -> VFreeVar v spine
  VUnitType -> VBuiltin (BuiltinType UnitType) []
  VBoolType -> IBoolType
  VRatType -> IRatType
  VIndexType n -> IIndexType n
  VNatType -> INatType
  VListType tElem -> IListType tElem
  VBoolTensorType ds -> ITensorType (fromTypeValue VBoolType) ds
  VRatTensorType ds -> ITensorType (fromTypeValue VRatType) ds
  VNatTensorType ds -> ITensorType (fromTypeValue VNatType) ds
  VIndexTensorType n ds -> ITensorType (fromTypeValue (VIndexType n)) ds
  VVectorType tElem d -> IVectorType tElem d

-------------------------------------------------------------------------------
-- Index

-- | A view on all possible expressions that can have type `Index n`.
data IndexValue
  = VIndexLiteral Int
  | VIndexBoundVar Lv (Spine Builtin)
  | VIndexIf (IfArgs (Value Builtin))

toIndexValue :: (HasCallStack) => Value Builtin -> IndexValue
toIndexValue e = case e of
  VBoundVar v spine -> VIndexBoundVar v spine
  (getExpr accessIndexLiteral -> Just i) -> VIndexLiteral i
  (getExpr accessIf -> Just args) -> VIndexIf args
  _ -> developerError $ "ill-typed index expression" <+> pretty (show e)

-------------------------------------------------------------------------------
-- Nat

-- | A view on all possible expressions that can have type `Nat`.
data NatValue
  = VNatLiteral Int
  | VNatBoundVar Lv (Spine Builtin)
  | VNatIf (IfArgs (Value Builtin))
  | VNatAdd (Op2Args (Value Builtin))
  | VNatMul (Op2Args (Value Builtin))
  | VNatParameter Identifier

toNatValue :: (HasCallStack) => Value Builtin -> NatValue
toNatValue expr = case expr of
  VBoundVar v spine -> VNatBoundVar v spine
  VFreeVar ident [] -> VNatParameter ident
  (getExpr accessNatLiteral -> Just i) -> VNatLiteral i
  (getExpr accessIf -> Just args) -> VNatIf args
  (getExpr accessAddNat -> Just args) -> VNatAdd args
  (getExpr accessMulNat -> Just args) -> VNatMul args
  _ -> developerError $ "ill-typed Nat expression:" <+> prettyVerbose expr

fromNatValue :: NatValue -> Value Builtin
fromNatValue = \case
  VNatBoundVar v spine -> VBoundVar v spine
  VNatParameter ident -> VFreeVar ident []
  VNatLiteral i -> mkExpr accessNatLiteral i
  VNatIf args -> mkExpr accessIf args
  VNatAdd args -> mkExpr accessAddNat args
  VNatMul args -> mkExpr accessMulNat args

-------------------------------------------------------------------------------
-- Vector

-- | A view on all possible expressions that can have type `Nat`.
data VectorValue
  = VVectorBoundVar Lv (Spine Builtin)
  | VVectorDataset Identifier
  | VVectorLiteral (VecLitArgs (Value Builtin))
  | VVectorIf (IfArgs (Value Builtin))
  | VVectorForeach (ForeachVectorArgs (Value Builtin))

toVectorValue :: Value Builtin -> VectorValue
toVectorValue value = case value of
  VBoundVar v spine -> VVectorBoundVar v spine
  VFreeVar ident [] -> VVectorDataset ident
  (getExpr accessVecLit -> Just (_size, args)) -> VVectorLiteral args
  (getExpr accessIf -> Just args) -> VVectorIf args
  (getExpr accessForeachVector -> Just args) -> VVectorForeach args
  _ -> developerError $ "ill-typed Vector expression:" <+> prettyVerbose value

-------------------------------------------------------------------------------
-- Bool

-- | A view on all possible expressions that can have type `Tensor Bool`.
data BoolValue
  = VBoolLiteral Bool
  | VNot (TensorOp1Args (Value Builtin))
  | VAnd (TensorOp2Args (Value Builtin))
  | VOr (TensorOp2Args (Value Builtin))
  | VCompareIndex (ComparisonOp, IndexComparisonArgs (Value Builtin))
  | VCompareNat (ComparisonOp, Op2Args (Value Builtin))
  | VCompareRatTensor (ComparisonOp, TensorOp2Args (Value Builtin))
  | VReduceAndTensor (TensorReductionArgs (Value Builtin))
  | VReduceOrTensor (TensorReductionArgs (Value Builtin))
  | VQuantifyRatTensor (Quantifier, QuantifyRatTensorArgs (Value Builtin) (Closure Builtin))
  | VBoolIf (IfArgs (Value Builtin))
  | VBoolAt (AtTensorArgs (Value Builtin))

toBoolValue :: (HasCallStack) => Value Builtin -> BoolValue
toBoolValue expr = case expr of
  (getExpr accessBoolTensorLiteral -> Just (ZeroDimTensor v)) -> VBoolLiteral v
  (getExpr accessAndTensor -> Just args) -> VAnd args
  (getExpr accessOrTensor -> Just args) -> VOr args
  (getExpr accessNotTensor -> Just args) -> VNot args
  (getExpr accessCompareRatTensorPointwise -> Just args) -> fromComparison $ Left args
  (getExpr accessCompareRatTensorReduced -> Just args) -> fromComparison $ Right args
  (getExpr accessCompareNat -> Just args) -> VCompareNat args
  (getExpr accessCompareIndex -> Just args) -> VCompareIndex args
  (getExpr accessQuantifyRatTensor -> Just args) -> VQuantifyRatTensor args
  (getExpr accessReduceAnd -> Just args) -> VReduceAndTensor args
  (getExpr accessReduceOr -> Just args) -> VReduceOrTensor args
  (getExpr accessAtTensor -> Just args) -> VBoolAt args
  (getExpr accessIf -> Just args) -> VBoolIf args
  _ -> developerError $ "ill-typed Bool expression:" <+> prettyVerbose expr

fromBoolValue :: BoolValue -> Value Builtin
fromBoolValue = \case
  VBoolLiteral y -> mkExpr accessBoolTensorLiteral (ZeroDimTensor y)
  VAnd args -> mkExpr accessAndTensor args
  VOr args -> mkExpr accessOrTensor args
  VNot args -> mkExpr accessNotTensor args
  VCompareNat args -> mkExpr accessCompareNat args
  VCompareIndex args -> mkExpr accessCompareIndex args
  VCompareRatTensor args -> toComparison args
  VQuantifyRatTensor args -> mkExpr accessQuantifyRatTensor args
  VReduceAndTensor args -> mkExpr accessReduceAnd args
  VReduceOrTensor args -> mkExpr accessReduceOr args
  VBoolIf args -> mkExpr accessIf args
  VBoolAt args -> mkExpr accessAtTensor args

fromComparison ::
  Either
    (ComparisonOp, TensorOp2Args (Value Builtin))
    (ComparisonOp, TensorReduceComparisonArgs (Value Builtin)) ->
  BoolValue
fromComparison = \case
  Left (op, args) -> VCompareRatTensor (op, args)
  Right (op, TensorReduceComparisonArgs d ds e1 e2) ->
    VCompareRatTensor (op, TensorOp2Args (IDimCons d ds) e1 e2)

toComparison :: (ComparisonOp, TensorOp2Args (Value Builtin)) -> Value Builtin
toComparison (op, TensorOp2Args dims e1 e2) = case toDimensionsValue dims of
  VDimsNil -> mkExpr accessCompareRatTensorPointwise (op, TensorOp2Args dims e1 e2)
  VDimsCons d ds -> mkExpr accessCompareRatTensorReduced (op, TensorReduceComparisonArgs d ds e1 e2)
  _ -> developerError "Unexpected tensorOp2Args for comparison"

evalCompareRatTensor :: (MonadNormBuiltin m, MonadFreeContext Builtin m, MonadReadableNameContext m) => ComparisonOp -> EvalSimple TensorOp2Args Value Builtin m
evalCompareRatTensor op args@(TensorOp2Args dims e1 e2) = case toDimensionsValue dims of
  VDimsNil -> evalCompareRatTensorPointwise op args
  VDimsCons d ds -> do
    let reduceArgs = TensorReduceComparisonArgs d ds e1 e2
    namedCtx <- getNameContext
    normaliseBuiltin namedCtx (DerivedFunction (CompareRatTensorReduced op)) (mkExpr accessSpine reduceArgs)
  _ -> developerError "Unexpected tensorOp2Args for comparison"

-------------------------------------------------------------------------------
-- Bool

-- | A view on all possible expressions that can have type `Tensor Bool ds`.
data BoolTensorValue
  = VBoolTensorLiteral (Tensor Bool)
  | VBoolConstTensor (ConstTensorArgs (Value Builtin))
  | VBoolStackTensor (StackTensorArgs (Value Builtin))
  | VBoolTensorNot (TensorOp1Args (Value Builtin))
  | VBoolTensorAnd (TensorOp2Args (Value Builtin))
  | VBoolTensorOr (TensorOp2Args (Value Builtin))
  | VBoolTensorCompareIndex (ComparisonOp, IndexComparisonArgs (Value Builtin))
  | VBoolTensorCompareNat (ComparisonOp, Op2Args (Value Builtin))
  | VBoolTensorCompareRatPointwise (ComparisonOp, TensorOp2Args (Value Builtin))
  | VBoolTensorCompareRatReduced (ComparisonOp, TensorReduceComparisonArgs (Value Builtin))
  | VBoolTensorReduceAnd (TensorReductionArgs (Value Builtin))
  | VBoolTensorReduceOr (TensorReductionArgs (Value Builtin))
  | VBoolTensorQuantifyRat (Quantifier, QuantifyRatTensorArgs (Value Builtin) (Closure Builtin))
  | VBoolTensorBoolIf (IfArgs (Value Builtin))
  | VBoolTensorAt (AtTensorArgs (Value Builtin))
  | VBoolTensorForeach (ForeachTensorArgs (Value Builtin))

toBoolTensorValue :: (HasCallStack) => Value Builtin -> BoolTensorValue
toBoolTensorValue expr = case expr of
  (getExpr accessBoolTensorLiteral -> Just t) -> VBoolTensorLiteral t
  (getExpr accessConstTensor -> Just args) -> VBoolConstTensor args
  (getExpr accessStackTensor -> Just args) -> VBoolStackTensor args
  (getExpr accessAndTensor -> Just args) -> VBoolTensorAnd args
  (getExpr accessOrTensor -> Just args) -> VBoolTensorOr args
  (getExpr accessNotTensor -> Just args) -> VBoolTensorNot args
  (getExpr accessCompareRatTensorPointwise -> Just args) -> VBoolTensorCompareRatPointwise args
  (getExpr accessCompareRatTensorReduced -> Just args) -> VBoolTensorCompareRatReduced args
  (getExpr accessCompareNat -> Just args) -> VBoolTensorCompareNat args
  (getExpr accessCompareIndex -> Just args) -> VBoolTensorCompareIndex args
  (getExpr accessQuantifyRatTensor -> Just args) -> VBoolTensorQuantifyRat args
  (getExpr accessReduceAnd -> Just args) -> VBoolTensorReduceAnd args
  (getExpr accessReduceOr -> Just args) -> VBoolTensorReduceOr args
  (getExpr accessAtTensor -> Just args) -> VBoolTensorAt args
  (getExpr accessForeachTensor -> Just args) -> VBoolTensorForeach args
  (getExpr accessIf -> Just args) -> VBoolTensorBoolIf args
  _ -> developerError $ "ill-typed BoolTensor expression:" <+> prettyVerbose expr

fromBoolTensorValue :: BoolTensorValue -> Value Builtin
fromBoolTensorValue = \case
  VBoolTensorLiteral y -> mkExpr accessBoolTensorLiteral y
  VBoolConstTensor args -> mkExpr accessConstTensor args
  VBoolStackTensor args -> mkExpr accessStackTensor args
  VBoolTensorAnd args -> mkExpr accessAndTensor args
  VBoolTensorOr args -> mkExpr accessOrTensor args
  VBoolTensorNot args -> mkExpr accessNotTensor args
  VBoolTensorCompareNat args -> mkExpr accessCompareNat args
  VBoolTensorCompareIndex args -> mkExpr accessCompareIndex args
  VBoolTensorCompareRatPointwise args -> mkExpr accessCompareRatTensorPointwise args
  VBoolTensorCompareRatReduced args -> mkExpr accessCompareRatTensorReduced args
  VBoolTensorQuantifyRat args -> mkExpr accessQuantifyRatTensor args
  VBoolTensorReduceAnd args -> mkExpr accessReduceAnd args
  VBoolTensorReduceOr args -> mkExpr accessReduceOr args
  VBoolTensorBoolIf args -> mkExpr accessIf args
  VBoolTensorAt args -> mkExpr accessAtTensor args
  VBoolTensorForeach args -> mkExpr accessForeachTensor args

-------------------------------------------------------------------------------
-- Multi-dimensional bool tensor

-- | A view on all possible expressions that can have type `Tensor Bool (d :: ds)`.
data MultiDimBoolTensorValue
  = VMultiDimBoolTensorLiteral (Tensor Bool)
  | VMultiDimBoolConstTensor (ConstTensorArgs (Value Builtin))
  | VMultiDimBoolStackTensor (StackTensorArgs (Value Builtin))
  | VPointwiseNot (TensorOp1Args (Value Builtin))
  | VPointwiseAnd (TensorOp2Args (Value Builtin))
  | VPointwiseOr (TensorOp2Args (Value Builtin))
  | VCompareRatTensorPointwise (ComparisonOp, TensorOp2Args (Value Builtin))
  | VMultiDimBoolIf (IfArgs (Value Builtin))
  | VMultiDimBoolAt (AtTensorArgs (Value Builtin))
  | VBoolForeach (ForeachTensorArgs (Value Builtin))

toMultiDimBoolTensorValue :: (HasCallStack) => Value Builtin -> MultiDimBoolTensorValue
toMultiDimBoolTensorValue expr = case expr of
  (getExpr accessBoolTensorLiteral -> Just t) -> VMultiDimBoolTensorLiteral t
  (getExpr accessConstTensor -> Just args) -> VMultiDimBoolConstTensor args
  (getExpr accessStackTensor -> Just args) -> VMultiDimBoolStackTensor args
  (getExpr accessNotTensor -> Just args) -> VPointwiseNot args
  (getExpr accessAndTensor -> Just args) -> VPointwiseAnd args
  (getExpr accessOrTensor -> Just args) -> VPointwiseOr args
  (getExpr accessCompareRatTensorPointwise -> Just args) -> VCompareRatTensorPointwise args
  (getExpr accessIf -> Just args) -> VMultiDimBoolIf args
  (getExpr accessAtTensor -> Just args) -> VMultiDimBoolAt args
  (getExpr accessForeachTensor -> Just args) -> VBoolForeach args
  _ -> developerError $ "ill-typed MultiDimBoolTensor expression:" <+> prettyVerbose expr

fromMultiDimBoolTensorValue :: MultiDimBoolTensorValue -> Value Builtin
fromMultiDimBoolTensorValue = \case
  VMultiDimBoolTensorLiteral y -> mkExpr accessBoolTensorLiteral y
  VMultiDimBoolConstTensor args -> mkExpr accessConstTensor args
  VMultiDimBoolStackTensor args -> mkExpr accessStackTensor args
  VPointwiseNot args -> mkExpr accessNotTensor args
  VPointwiseAnd args -> mkExpr accessAndTensor args
  VPointwiseOr args -> mkExpr accessOrTensor args
  VCompareRatTensorPointwise args -> mkExpr accessCompareRatTensorPointwise args
  VMultiDimBoolIf args -> mkExpr accessIf args
  VMultiDimBoolAt args -> mkExpr accessAtTensor args
  VBoolForeach args -> mkExpr accessForeachTensor args

-------------------------------------------------------------------------------
-- Tensor Rat

-- | A view on all possible expressions that can have type `Tensor Rat`.
data RatTensorValue
  = VRatTensorLiteral (Tensor Rational)
  | VNegRatTensor (TensorOp1Args (Value Builtin))
  | VAddRatTensor (TensorOp2Args (Value Builtin))
  | VSubRatTensor (TensorOp2Args (Value Builtin))
  | VMulRatTensor (TensorOp2Args (Value Builtin))
  | VDivRatTensor (TensorOp2Args (Value Builtin))
  | VMinRatTensor (TensorOp2Args (Value Builtin))
  | VMaxRatTensor (TensorOp2Args (Value Builtin))
  | VReduceAddRatTensor (TensorReductionArgs (Value Builtin))
  | VReduceMulRatTensor (TensorReductionArgs (Value Builtin))
  | VReduceMinRatTensor (TensorReductionArgs (Value Builtin))
  | VReduceMaxRatTensor (TensorReductionArgs (Value Builtin))
  | VIfRatTensor (IfArgs (Value Builtin))
  | VRatTensorBoundVar Lv
  | VRatTensorFreeVar Identifier (Spine Builtin)
  | VRatConstTensor (ConstTensorArgs (Value Builtin))
  | VRatStackTensor (StackTensorArgs (Value Builtin))
  | VRatAt (AtTensorArgs (Value Builtin))
  | VRatForeach (ForeachTensorArgs (Value Builtin))

toRatTensorValue :: (HasCallStack) => Value Builtin -> RatTensorValue
toRatTensorValue expr = case expr of
  VBoundVar lv [] -> VRatTensorBoundVar lv
  VFreeVar n spine -> VRatTensorFreeVar n spine
  (getExpr accessRatTensorLiteral -> Just t) -> VRatTensorLiteral t
  (getExpr accessNegRatTensor -> Just args) -> VNegRatTensor args
  (getExpr accessAddRatTensor -> Just args) -> VAddRatTensor args
  (getExpr accessSubRatTensor -> Just args) -> VSubRatTensor args
  (getExpr accessMulRatTensor -> Just args) -> VMulRatTensor args
  (getExpr accessDivRatTensor -> Just args) -> VDivRatTensor args
  (getExpr accessMinRatTensor -> Just args) -> VMinRatTensor args
  (getExpr accessMaxRatTensor -> Just args) -> VMaxRatTensor args
  (getExpr accessReduceAddRat -> Just args) -> VReduceAddRatTensor args
  (getExpr accessReduceMulRat -> Just args) -> VReduceMulRatTensor args
  (getExpr accessReduceMinRat -> Just args) -> VReduceMinRatTensor args
  (getExpr accessReduceMaxRat -> Just args) -> VReduceMaxRatTensor args
  (getExpr accessIf -> Just args) -> VIfRatTensor args
  (getExpr accessConstTensor -> Just args) -> VRatConstTensor args
  (getExpr accessStackTensor -> Just args) -> VRatStackTensor args
  (getExpr accessAtTensor -> Just args) -> VRatAt args
  (getExpr accessForeachTensor -> Just args) -> VRatForeach args
  _ -> illTyped
  where
    illTyped = developerError $ "ill-typed RatTensor expression:" <+> pretty (show expr) -- rettyVerbose expr

fromRatTensorValue :: RatTensorValue -> Value Builtin
fromRatTensorValue = \case
  VRatTensorBoundVar v -> VBoundVar v []
  VRatTensorFreeVar name args -> VFreeVar name args
  VRatTensorLiteral t -> mkExpr accessRatTensorLiteral t
  VNegRatTensor args -> mkExpr accessNegRatTensor args
  VAddRatTensor args -> mkExpr accessAddRatTensor args
  VSubRatTensor args -> mkExpr accessSubRatTensor args
  VMulRatTensor args -> mkExpr accessMulRatTensor args
  VDivRatTensor args -> mkExpr accessDivRatTensor args
  VMinRatTensor args -> mkExpr accessMinRatTensor args
  VMaxRatTensor args -> mkExpr accessMaxRatTensor args
  VReduceAddRatTensor args -> mkExpr accessReduceAddRat args
  VReduceMulRatTensor args -> mkExpr accessReduceMulRat args
  VReduceMinRatTensor args -> mkExpr accessReduceMinRat args
  VReduceMaxRatTensor args -> mkExpr accessReduceMaxRat args
  VIfRatTensor args -> mkExpr accessIf args
  VRatConstTensor args -> mkExpr accessConstTensor args
  VRatStackTensor args -> mkExpr accessStackTensor args
  VRatAt args -> mkExpr accessAtTensor args
  VRatForeach args -> mkExpr accessForeachTensor args

-------------------------------------------------------------------------------
-- Dim

-- | A view on all possible expressions that can have type `List Int`.
data DimensionsValue
  = VDimsNil
  | VDimsCons (Value Builtin) (Value Builtin)
  | VDimsBoundVar Lv (Spine Builtin)
  | VDimsIf (IfArgs (Value Builtin))

toDimensionsValue :: (HasCallStack) => Value Builtin -> DimensionsValue
toDimensionsValue e = case e of
  VBoundVar lv spine -> VDimsBoundVar lv spine
  (getExpr accessNil -> Just (NilArgs INatType)) -> VDimsNil
  (getExpr accessCons -> Just (ConsArgs INatType x xs)) -> VDimsCons x xs
  (getExpr accessIf -> Just args) -> VDimsIf args
  _ -> developerError $ "ill-typed Dimensions expression" <+> prettyVerbose e

fromDimensionsValue :: (HasCallStack) => DimensionsValue -> Value Builtin
fromDimensionsValue e = case e of
  VDimsBoundVar lv spine -> VBoundVar lv spine
  VDimsNil -> mkExpr accessNil (NilArgs INatType)
  VDimsCons x xs -> mkExpr accessCons (ConsArgs INatType x xs)
  VDimsIf args -> mkExpr accessIf args

-------------------------------------------------------------------------------
-- Utilities

-- | Reduces a tensor value `x` to `[x!0, x!1, ..., x!n]`
etaReduceTensor ::
  (MonadNormBuiltin m, BuiltinHasNatLiterals builtin, BuiltinHasIndexLiterals builtin, BuiltinHasTensors builtin, HasTensorLiterals Value builtin, BuiltinHasListLiterals builtin, BuiltinHasNatType builtin) =>
  VType builtin ->
  Int ->
  Value builtin ->
  Value builtin ->
  m [Value builtin]
etaReduceTensor typ dim dims tensor = do
  let mkAtArgs i =
        AtTensorArgs
          { atType = typ,
            atFirstDim = INatLiteral dim,
            atRemainingDims = dims,
            atTensor = tensor,
            atIndex = IIndexLiteral i
          }
  let mkAt i = unoptimisedEvalAtTensor (mkAtArgs i)
  traverse mkAt [0 .. (dim - 1)]

scaleValue :: Value Builtin -> ScaleConstant (Value Builtin)
scaleValue dims c value = runSilentLogger $ do
  constantTensor <- evalConstTensor $ ConstTensorArgs IRatType (IRatLiteral c) dims
  evalMulRatTensor $ TensorOp2Args dims constantTensor value

addValues :: Value Builtin -> AddConstants (Value Builtin)
addValues dims c1 c2 v1 v2 = runSilentLogger $ do
  let cv1 = scaleValue dims c1 v1
  let cv2 = scaleValue dims c2 v2
  evalAddRatTensor $ TensorOp2Args dims cv1 cv2
