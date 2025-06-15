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
    RatTensorValue (..),
    toRatTensorValue,
    fromRatTensorValue,
    DimensionsValue (..),
    toDimensionsValue,
  )
where

import GHC.Stack (HasCallStack)
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value
import Vehicle.Data.DeBruijn
import Vehicle.Data.Tensor (Tensor, pattern ZeroDimTensor)
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
  | VCompareRatTensorPointwise (ComparisonOp, TensorOp2Args (Value Builtin))
  | VCompareRatTensorReduced (ComparisonOp, TensorOp2Args (Value Builtin))
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
  (getExpr accessCompareRatTensorPointwise -> Just args) -> VCompareRatTensorPointwise args
  (getExpr accessCompareRatTensorReduced -> Just args) -> VCompareRatTensorReduced args
  (getExpr accessCompareNat -> Just args) -> VCompareNat args
  (getExpr accessCompareIndex -> Just args) -> VCompareIndex args
  (getExpr accessQuantifyRatTensor -> Just (op, dims, VLam binder closure)) -> VQuantifyRatTensor op dims binder closure
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
  VCompareRatTensorPointwise args -> mkExpr accessCompareRatTensorPointwise args
  VCompareRatTensorReduced args -> mkExpr accessCompareRatTensorReduced args
  VQuantifyRatTensor q dims binder closure -> mkExpr accessQuantifyRatTensor (q, dims, VLam binder closure)
  VReduceAndTensor args -> mkExpr accessReduceAnd args
  VReduceOrTensor args -> mkExpr accessReduceOr args
  VBoolIf args -> mkExpr accessIf args
  VBoolAt args -> mkExpr accessAtTensor args

-------------------------------------------------------------------------------
-- Bool
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

{-
-- | A view on all possible expressions that can have type `Tensor Bool`.
data BoolTensorValue
  = VBoolTensorLiteral (Tensor Bool)
  | VNot (TensorOp1Args (Value Builtin))
  | VAnd (TensorOp2Args (Value Builtin))
  | VOr (TensorOp2Args (Value Builtin))
  | VCompareIndex (ComparisonOp, IndexComparisonArgs (Value Builtin))
  | VCompareNat (ComparisonOp, Op2Args (Value Builtin))
  | VCompareRatTensorPointwise (ComparisonOp, TensorOp2Args (Value Builtin))
  | VCompareRatTensorReduced (ComparisonOp, TensorOp2Args (Value Builtin))
  | VReduceAndTensor (TensorOp2Args (Value Builtin))
  | VReduceOrTensor (TensorOp2Args (Value Builtin))
  | VQuantifyRatTensor Quantifier (VArg Builtin) (VBinder Builtin) (Closure Builtin)
  | VBoolIf (IfArgs (Value Builtin))
  | VConstBoolTensor (ConstTensorArgs (Value Builtin))
  | VBoolAt (AtTensorArgs (Value Builtin))
  | VBoolForeach (ForeachArgs (Value Builtin))

toBoolTensorValue :: (HasCallStack) => Value Builtin -> BoolTensorValue
toBoolTensorValue expr = case expr of
  (getExpr accessBoolTensorLiteral -> Just t) -> VBoolTensorLiteral t
  (getExpr accessAndTensor -> Just args) -> VAnd args
  (getExpr accessOrTensor -> Just args) -> VOr args
  (getExpr accessNotTensor -> Just args) -> VNot args
  (getExpr accessCompareRatTensorPointwise -> Just args) -> VCompareRatTensorPointwise args
  (getExpr accessCompareRatTensorReduced -> Just args) -> VCompareRatTensorReduced args
  (getExpr accessCompareNat -> Just args) -> VCompareNat args
  (getExpr accessCompareIndex -> Just args) -> VCompareIndex args
  (getExpr accessQuantifyRatTensor -> Just (op, dims, VLam binder closure)) -> VQuantifyRatTensor op dims binder closure
  (getExpr accessReduceAnd -> Just args) -> VReduceAndTensor args
  (getExpr accessReduceOr -> Just args) -> VReduceOrTensor args
  (getExpr accessConstTensor -> Just args) -> VConstBoolTensor args
  (getExpr accessStackTensor -> Just args) -> VBoolStackTensor args
  (getExpr accessAtTensor -> Just args) -> VBoolAt args
  (getExpr accessForeachTensor -> Just args) -> VBoolForeach args
  (getExpr accessIf -> Just args) -> VBoolIf args
  _ -> developerError $ "ill-typed RatTensor expression:" <+> prettyVerbose expr

fromBoolTensorValue :: BoolTensorValue -> Value Builtin
fromBoolTensorValue = \case
  VBoolTensorLiteral y -> mkExpr accessBoolTensorLiteral y
  VAnd args -> mkExpr accessAndTensor args
  VOr args -> mkExpr accessOrTensor args
  VNot args -> mkExpr accessNotTensor args
  VCompareNat args -> mkExpr accessCompareNat args
  VCompareIndex args -> mkExpr accessCompareIndex args
  VCompareRatTensorPointwise args -> mkExpr accessCompareRatTensorPointwise args
  VCompareRatTensorReduced args -> mkExpr accessCompareRatTensorReduced args
  VQuantifyRatTensor q dims binder closure -> mkExpr accessQuantifyRatTensor (q, dims, VLam binder closure)
  VReduceAndTensor args -> mkExpr accessReduceAnd args
  VReduceOrTensor args -> mkExpr accessReduceOr args
  VBoolIf args -> mkExpr accessIf args
  VConstBoolTensor args -> mkExpr accessConstTensor args
  VBoolStackTensor args -> mkExpr accessStackTensor args
  VBoolAt args -> mkExpr accessAtTensor args
  VBoolForeach args -> mkExpr accessForeachTensor args
-}
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
