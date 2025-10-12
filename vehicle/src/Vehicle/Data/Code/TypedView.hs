module Vehicle.Data.Code.TypedView
  ( TypeValue (..),
    toTypeValue,
    fromTypeValue,
    IndexValue (..),
    toIndexValue,
    NatValue (..),
    toNatValue,
    fromNatValue,
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
  )
where

import GHC.Stack (HasCallStack)
import Vehicle.Compile.Normalise.NBE (normaliseApp, normaliseBuiltin, normaliseInEnv)
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Builtin.Interface.Normalise (EvalSimple, MonadNormBuiltin, evalAtTensor, evalCompareRatTensorPointwise)
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Builtin.Standard.Normalise ()
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value
import Vehicle.Data.Tensor (Tensor, pattern ZeroDimTensor)
import Vehicle.Data.Variable.Bound.Context (NamedBoundCtx)
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Data.Variable.Free.Context (MonadFreeContext)
import Vehicle.Prelude

-------------------------------------------------------------------------------
-- Types

-- | A view on all possible expressions that can have type `List Int`.
data TypeValue
  = VUnitType
  | VBoolType
  | VIndexType (Value Builtin)
  | VNatType
  | VRatType
  | VBoolTensorType (Value Builtin)
  | VNatTensorType (Value Builtin)
  | VRatTensorType (Value Builtin)
  | VIndexTensorType (Value Builtin) (Value Builtin)
  | VListType (Value Builtin)
  | VVectorType (Value Builtin) (Value Builtin)
  | VPiType (VBinder Builtin) (Closure Builtin)
  | VBoundTypeVar Lv (Spine Builtin)

toTypeValue :: (HasCallStack) => Value Builtin -> TypeValue
toTypeValue t = case t of
  VPi binder value -> VPiType binder value
  VBoundVar lv spine -> VBoundTypeVar lv spine
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
  VUnitType -> mkType UnitType []
  VBoolType -> mkType BoolType []
  VRatType -> mkType RatType []
  VIndexType n -> mkType IndexType [explicitIrrelevant n]
  VNatType -> mkType NatType []
  VListType tElem -> mkType ListType [explicit tElem]
  VBoolTensorType ds -> mkType TensorType [explicit (fromTypeValue VBoolType), explicitIrrelevant ds]
  VRatTensorType ds -> mkType TensorType [explicit (fromTypeValue VRatType), explicitIrrelevant ds]
  VNatTensorType ds -> mkType TensorType [explicit (fromTypeValue VNatType), explicitIrrelevant ds]
  VIndexTensorType n ds -> mkType TensorType [explicit (fromTypeValue (VIndexType n)), explicitIrrelevant ds]
  VVectorType tElem d -> mkType VectorType [explicit tElem, explicitIrrelevant d]
  where
    mkType builtin = VBuiltin (BuiltinType builtin)

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
  | VReduceAndTensor (TensorOp2Args (Value Builtin))
  | VReduceOrTensor (TensorOp2Args (Value Builtin))
  | VQuantifyRatTensor Quantifier (VArg Builtin) (VBinder Builtin) (Closure Builtin)
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
  (getExpr accessQuantifyRatTensor -> Just (q, QuantifyRatTensorArgs dims (VLam binder closure))) -> VQuantifyRatTensor q dims binder closure
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
  VQuantifyRatTensor q dims binder closure -> mkExpr accessQuantifyRatTensor (q, QuantifyRatTensorArgs dims (VLam binder closure))
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
  Right (op, TensorReduceComparisonArgs d ds e1 e2) -> do
    let dims = implicitIrrelevant $ fromDimensionsValue $ VDimsCons (argExpr d) (argExpr ds)
    VCompareRatTensor (op, TensorOp2Args dims e1 e2)

toComparison :: (ComparisonOp, TensorOp2Args (Value Builtin)) -> Value Builtin
toComparison (op, TensorOp2Args dims e1 e2) = case toDimensionsValue $ argExpr dims of
  VDimsNil -> mkExpr accessCompareRatTensorPointwise (op, TensorOp2Args dims e1 e2)
  VDimsCons d ds -> mkExpr accessCompareRatTensorReduced (op, TensorReduceComparisonArgs (implicitIrrelevant d) (implicitIrrelevant ds) e1 e2)
  _ -> developerError "Unexpected tensorOp2Args for comparison"

evalCompareRatTensor :: (MonadNormBuiltin m, MonadFreeContext Builtin m, MonadNameContext m) => ComparisonOp -> EvalSimple TensorOp2Args Value Builtin m
evalCompareRatTensor op args@(TensorOp2Args dims e1 e2) = case toDimensionsValue $ argExpr dims of
  VDimsNil -> evalCompareRatTensorPointwise op args
  VDimsCons d ds -> do
    let reduceArgs = TensorReduceComparisonArgs (implicitIrrelevant d) (implicitIrrelevant ds) e1 e2
    namedCtx <- getNameContext
    normaliseBuiltin namedCtx (DerivedFunction (CompareRatTensorReduced op)) (mkExpr accessSpine reduceArgs)
  _ -> developerError "Unexpected tensorOp2Args for comparison"

-------------------------------------------------------------------------------
-- Bool
{-
data BoolTensorValue
  = VBoolTensorLiteral (Tensor Bool)
  | VBoolStackTensor (StackTensorArgs (Value Builtin))
  | VBoolVecLiteral (VecLitArgs (Value Builtin))

toBoolTensorValue :: (HasCallStack) => Value Builtin -> Maybe BoolTensorValue
toBoolTensorValue expr = case expr of
  (getExpr accessBoolTensorLiteral -> Just t) -> Just $ VBoolTensorLiteral t
  (getExpr accessStackTensor -> Just args) -> Just $ VBoolStackTensor args
  (getExpr accessVecLit -> Just args) -> Just $ VBoolVecLiteral args
  _ -> Nothing -- developerError $ "ill-typed BoolTensor expression:" <+> prettyVerbose expr

fromBoolTensorValue :: BoolTensorValue -> Value Builtin
fromBoolTensorValue = \case
  VBoolTensorLiteral y -> mkExpr accessBoolTensorLiteral y
  VBoolStackTensor args -> mkExpr accessStackTensor args
  VBoolVecLiteral args -> mkExpr accessVecLit args
-}

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
  | VBoolTensorReduceAnd (TensorOp2Args (Value Builtin))
  | VBoolTensorReduceOr (TensorOp2Args (Value Builtin))
  | VBoolTensorQuantifyRat Quantifier (VArg Builtin) (VBinder Builtin) (Closure Builtin)
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
  (getExpr accessQuantifyRatTensor -> Just (q, QuantifyRatTensorArgs dims (VLam binder closure))) -> VBoolTensorQuantifyRat q dims binder closure
  (getExpr accessReduceAnd -> Just args) -> VBoolTensorReduceAnd args
  (getExpr accessReduceOr -> Just args) -> VBoolTensorReduceOr args
  (getExpr accessAtTensor -> Just args) -> VBoolTensorAt args
  (getExpr accessForeachTensor -> Just args) -> VBoolTensorForeach args
  (getExpr accessIf -> Just args) -> VBoolTensorBoolIf args
  _ -> developerError $ "ill-typed RatTensor expression:" <+> prettyVerbose expr

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
  VBoolTensorQuantifyRat q dims binder closure -> mkExpr accessQuantifyRatTensor (q, QuantifyRatTensorArgs dims (VLam binder closure))
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
  _ -> developerError $ "ill-typed RatTensor expression:" <+> prettyVerbose expr

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
  | VReduceAddRatTensor (TensorOp2Args (Value Builtin))
  | VReduceMulRatTensor (TensorOp2Args (Value Builtin))
  | VReduceMinRatTensor (TensorOp2Args (Value Builtin))
  | VReduceMaxRatTensor (TensorOp2Args (Value Builtin))
  | VIfRatTensor (IfArgs (Value Builtin))
  | VRatTensorVar Lv
  | VNetworkApp Identifier (NetworkAppArgs (Value Builtin))
  | VRatConstTensor (ConstTensorArgs (Value Builtin))
  | VRatStackTensor (StackTensorArgs (Value Builtin))
  | VRatAt (AtTensorArgs (Value Builtin))
  | VRatForeach (ForeachTensorArgs (Value Builtin))

toRatTensorValue :: (HasCallStack) => Value Builtin -> RatTensorValue
toRatTensorValue expr = case expr of
  VBoundVar lv [] -> VRatTensorVar lv
  VFreeVar n (getExpr accessSpine -> Just args) -> VNetworkApp n args
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
    illTyped = developerError $ "ill-typed RatTensor expression:" <+> prettyVerbose expr

fromRatTensorValue :: RatTensorValue -> Value Builtin
fromRatTensorValue = \case
  VRatTensorVar v -> VBoundVar v []
  VNetworkApp name args -> VFreeVar name $ mkExpr accessSpine args
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
  (getExpr accessNil -> Just (argExpr -> INatType)) -> VDimsNil
  (getExpr accessCons -> Just (argExpr -> INatType, x, xs)) -> VDimsCons x xs
  (getExpr accessIf -> Just args) -> VDimsIf args
  _ -> developerError $ "ill-typed Dimensions expression" <+> prettyVerbose e

fromDimensionsValue :: (HasCallStack) => DimensionsValue -> Value Builtin
fromDimensionsValue e = case e of
  VDimsBoundVar lv spine -> VBoundVar lv spine
  VDimsNil -> mkExpr accessNil (implicit INatType)
  VDimsCons x xs -> mkExpr accessCons (implicit INatType, x, xs)
  VDimsIf args -> mkExpr accessIf args

-------------------------------------------------------------------------------
-- Utilities

-- | Reduces a tensor value `x` to `[x!0, x!1, ..., x!n]`
etaReduceTensor ::
  (MonadNormBuiltin m, MonadFreeContext Builtin m) =>
  NamedBoundCtx ->
  VType Builtin ->
  Int ->
  Value Builtin ->
  Value Builtin ->
  m [Value Builtin]
etaReduceTensor ctx typ dim dims tensor = do
  let mkAtArgs i =
        AtTensorArgs
          { atType = implicit typ,
            atFirstDim = implicitIrrelevant $ INatLiteral dim,
            atRemainingDims = implicitIrrelevant dims,
            atTensor = tensor,
            atIndex = IIndexLiteral i
          }
  let mkAt i = evalAtTensor ctx normaliseApp normaliseInEnv (mkAtArgs i)
  traverse mkAt [0 .. (dim - 1)]
