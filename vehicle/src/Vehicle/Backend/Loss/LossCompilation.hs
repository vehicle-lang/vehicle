module Vehicle.Backend.Loss.LossCompilation
  ( convertType,
    convertFunction,
    convertRatTensor,
    convertDims,
    convertBoundVar,
    convertVecLiteralArgs,
    convertVecForeachArgs,
    convertBoolTensorLiteral,
    convertNatComparison,
    convertIndexComparison,
    convertRatTensorPointwiseComparison,
    convertRatTensorReducedComparison,
    convertTensorReduction,
    convertStackTensor,
    convertConstTensor,
    convertAtTensor,
    convertForeachTensor,
    convertTensorOp1,
    convertTensorOp2,
    convertBoolTensor,
    convertNot,
    convertOr,
    convertAnd,
    convertReduceAnd,
    convertReduceOr,
    convertIf,
  )
where

import Vehicle.Backend.Loss.Core hiding (currentPass)
import Vehicle.Compile.Normalise.NBE (normaliseAppInEmptyFreeEnv, normaliseClosure)
import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Builtin.Interface.Normalise
import Vehicle.Data.Builtin.Loss
import Vehicle.Data.Builtin.Standard (Builtin (..))
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Value
import Vehicle.Data.DifferentiableLogic
import Vehicle.Data.Tensor (Tensor, foldMapTensor, shapeOf)
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Data.Variable.Bound.Context.Tensor
import Vehicle.Data.Variable.Bound.Level (SliceVariableLike (..), TensorVariable (..), VariableLike (..), findSliceIndices)

--------------------------------------------------------------------------------
-- Types

convertType ::
  (MonadLogic m) =>
  VType Builtin ->
  m (VType LossBuiltin)
convertType typ = logConversion typ $ case toTypeValue typ of
  VPiType binder closure -> convertPiType binder closure
  VUnitType {} -> unexpectedOperation "unit type"
  VFreeTypeVar {} -> unexpectedOperation "free var type"
  VBoolType -> convertBoolType
  VBoundTypeVar lv spine -> convertBoundVar lv spine
  VRatType -> return IRatType
  VIndexType n -> IIndexType <$> convertDim n
  VNatType -> return INatType
  VListType tElem -> IListType <$> convertType tElem
  VVectorType {} -> unsupportedOperation "VectorType"
  VBoolTensorType ds -> ITensorType <$> convertBoolType <*> convertDims ds
  VRatTensorType ds -> ITensorType IRatType <$> convertDims ds
  VNatTensorType ds -> ITensorType INatType <$> convertDims ds
  VIndexTensorType n ds -> (ITensorType . IIndexType <$> convertDim n) <*> convertDims ds

convertBoolType :: (MonadLogic m) => m (VType LossBuiltin)
convertBoolType = return IRatType

convertPiType :: (MonadLogic m) => VBinder Builtin -> Closure Builtin -> m (VType LossBuiltin)
convertPiType binder closure = do
  binder' <- traverse convertType binder
  closure' <- convertClosure convertType binder closure
  return $ VPi binder' closure'

--------------------------------------------------------------------------------
-- Dims

convertDim ::
  (MonadLogic m) =>
  Value Builtin ->
  m (Value LossBuiltin)
convertDim value = logConversion value $ case toNatValue value of
  VNatBoundVar v spine -> convertBoundVar v spine
  VNatParameter ident -> return $ VFreeVar ident []
  VNatLiteral i -> return $ mkExpr accessNatLiteral i
  VNatAdd args -> mkExpr accessAddNat <$> traverseOp2Args convertDim args
  VNatMul args -> mkExpr accessMulNat <$> traverseOp2Args convertDim args
  VNatIf {} -> unsupportedOperation "if"

convertDims ::
  (MonadLogic m) =>
  VDims Builtin ->
  m (VDims LossBuiltin)
convertDims value = logConversion value $ case toDimensionsValue value of
  VDimsNil -> return IDimNil
  VDimsCons d ds -> IDimCons <$> convertDim d <*> convertDims ds
  VDimsBoundVar lv spine -> convertBoundVar lv spine
  VDimsIf args -> convertIf args

--------------------------------------------------------------------------------
-- Variables

convertFunction ::
  (MonadLogic m) =>
  (Value Builtin -> m (Value LossBuiltin)) ->
  Value Builtin ->
  m (Value LossBuiltin)
convertFunction convertValue value = case value of
  VLam binder closure -> do
    binder' <- traverse convertType binder
    closure' <- convertClosure convertValue binder closure
    return $ VLam binder' closure'
  _ -> convertValue value

convertClosure ::
  (MonadLogic m) =>
  (Value Builtin -> m (Value LossBuiltin)) ->
  VBinder Builtin ->
  Closure Builtin ->
  m (Closure LossBuiltin)
convertClosure convertValue binder closure = do
  normBody <- normaliseClosure binder closure
  finalCtx <- getShrunkenContext
  lossBody <- addNonTensorBinderToContext binder $ do
    normLossBody <- convertFunction convertValue normBody
    return $ quote mempty (1 + boundCtxLv finalCtx) normLossBody
  return $ Closure (boundContextToEnv finalCtx) lossBody

-- | This function converts a DeBruijn level back into a loss value.
-- Crucially if the variable represents a slice of a quantified user variable
-- (e.g. X[0,1]) then it is replaced in terms of the original tensor variable
-- (e.g. X ! 0 ! 1)
convertBoundVar ::
  (MonadLogic m) =>
  Lv ->
  Spine Builtin ->
  m (Value LossBuiltin)
convertBoundVar lv = \case
  _ : _ -> unexpectedExprError currentPass "bound function variables"
  [] -> do
    maybeVars <- lookupSliceVariable lv
    case maybeVars of
      Nothing -> return $ VBoundVar lv []
      Just (parentVar, sliceVar)
        | toLv parentVar == toLv sliceVar -> return $ VBoundVar lv []
        | otherwise -> do
            let indices = findSliceIndices parentVar sliceVar
            parentLv <- lookupTensorVariableShrunkenLv (TensorVariable $ toSliceVar parentVar)
            let parentVarValue = VBoundVar parentLv []
            return $ mkIndexInto IRatType parentVarValue (shapeOf parentVar) indices

convertFreeVar ::
  (MonadLogic m) =>
  Identifier ->
  Spine Builtin ->
  m (Value LossBuiltin)
convertFreeVar name = \case
  [] -> return $ VFreeVar name []
  spine -> case getExpr accessSpine spine of
    Nothing -> unexpectedExprError currentPass "non-network args"
    Just (NetworkAppArgs arg) -> do
      args' <- NetworkAppArgs <$> convertRatTensor arg
      return $ VFreeVar name $ mkExpr accessSpine args'

--------------------------------------------------------------------------------
-- Bool

convertBoolTensor :: (MonadLogic m) => Value Builtin -> m (Value LossBuiltin)
convertBoolTensor value = logConversion value $ case toBoolTensorValue value of
  VBoolTensorLiteral bs -> convertBoolTensorLiteral bs
  VBoolConstTensor args -> convertConstTensor convertBoolTensor args
  VBoolStackTensor args -> convertStackTensor convertBoolTensor args
  VBoolTensorNot args -> convertNot =<< convertTensorOp1 convertBoolTensor args
  VBoolTensorAnd args -> convertAnd =<< convertTensorOp2 convertBoolTensor args
  VBoolTensorOr args -> convertOr =<< convertTensorOp2 convertBoolTensor args
  VBoolTensorCompareIndex args -> convertIndexComparison args
  VBoolTensorCompareNat args -> convertNatComparison args
  VBoolTensorCompareRatPointwise args -> convertRatTensorPointwiseComparison args
  VBoolTensorCompareRatReduced args -> convertRatTensorReducedComparison args
  VBoolTensorReduceAnd args -> convertReduceAnd =<< convertTensorReduction convertBoolTensor args
  VBoolTensorReduceOr args -> convertReduceOr =<< convertTensorReduction convertBoolTensor args
  VBoolTensorQuantifyRat {} -> unexpectedOperation "quantifier"
  VBoolTensorBoolIf args -> convertIf args
  VBoolTensorAt args -> convertAtTensor convertBoolTensor args
  VBoolTensorForeach args -> convertForeachTensor convertBoolTensor args

convertBoolTensorLiteral :: (MonadLogic m) => Tensor Bool -> m (Value LossBuiltin)
convertBoolTensorLiteral tensor = do
  trueExpr <- getLogicField TruthityElement
  falseExpr <- getLogicField FalsityElement

  let convertBool b = if b then trueExpr else falseExpr
  let foldLayer shape elems = do
        let dim = length elems
        let dims = implicitIrrelevant (mkDims shape)
        let args = implicit (INatLiteral dim) : dims : implicit INatType : fmap explicit elems
        VBuiltin (LossBuiltinFunction StackTensor) args
  return $ foldMapTensor convertBool foldLayer tensor

convertNot :: (MonadLogic m) => TensorOp1Args (Value LossBuiltin) -> m (Value LossBuiltin)
convertNot = convertLogicField PointwiseNegation

convertAnd :: (MonadLogic m) => TensorOp2Args (Value LossBuiltin) -> m (Value LossBuiltin)
convertAnd = convertLogicField PointwiseConjunction

convertOr :: (MonadLogic m) => TensorOp2Args (Value LossBuiltin) -> m (Value LossBuiltin)
convertOr = convertLogicField PointwiseDisjunction

convertReduceAnd :: (MonadLogic m) => TensorReductionArgs (Value LossBuiltin) -> m (Value LossBuiltin)
convertReduceAnd = convertLogicField ReduceConjunction

convertReduceOr :: (MonadLogic m) => TensorReductionArgs (Value LossBuiltin) -> m (Value LossBuiltin)
convertReduceOr = convertLogicField ReduceDisjunction

convertNatComparison :: (MonadLogic m) => (ComparisonOp, Op2Args (Value Builtin)) -> m (Value LossBuiltin)
convertNatComparison _args = unsupportedOperation "NatComparison"

convertIndexComparison :: (MonadLogic m) => (ComparisonOp, IndexComparisonArgs (Value Builtin)) -> m (Value LossBuiltin)
convertIndexComparison _args = unsupportedOperation "IndexComparison"

convertRatTensorPointwiseComparison :: (MonadLogic m) => (ComparisonOp, TensorOp2Args (Value Builtin)) -> m (Value LossBuiltin)
convertRatTensorPointwiseComparison (op, args) = do
  args' <- convertTensorOp2 convertRatTensor args
  convertLogicField (comparisonOpToField op) args'

convertRatTensorReducedComparison :: (MonadLogic m) => (ComparisonOp, TensorReduceComparisonArgs (Value Builtin)) -> m (Value LossBuiltin)
convertRatTensorReducedComparison _args = unsupportedOperation "RatTensorCompareReduced"

convertIf ::
  (MonadLogic m) =>
  IfArgs (Value Builtin) ->
  m (Value LossBuiltin)
convertIf _args = unsupportedOperation "if"

convertLogicField ::
  (MonadLogic m, IsArgs args) =>
  TensorDifferentiableLogicField ->
  args (Value LossBuiltin) ->
  m (Value LossBuiltin)
convertLogicField field args = do
  fn <- getLogicField field
  logDebugM MaxDetail $ do
    fnDoc <- prettyFriendlyInCtx fn
    return $ "subst-field" <+> pretty field <> ":" <+> fnDoc
  normaliseAppInEmptyFreeEnv mempty fn (mkExpr accessSpine args)

--------------------------------------------------------------------------------
-- Index

convertIndex ::
  (MonadLogic m) =>
  Value Builtin ->
  m (Value LossBuiltin)
convertIndex value = logConversion value $ case toIndexValue value of
  VIndexLiteral i -> return $ IIndexLiteral i
  VIndexBoundVar v spine -> convertBoundVar v spine
  VIndexIf args -> convertIf args

--------------------------------------------------------------------------------
-- Rat

convertRatTensor ::
  (MonadLogic m) =>
  Value Builtin ->
  m (Value LossBuiltin)
convertRatTensor value = logConversion value $ case toRatTensorValue value of
  VRatTensorBoundVar lv -> convertBoundVar lv mempty
  VRatTensorFreeVar name [] -> return $ VFreeVar name []
  VRatTensorFreeVar name spine -> convertFreeVar name spine
  VRatTensorLiteral t -> return $ mkExpr accessRatTensorLiteral t
  VNegRatTensor args -> mkExpr accessNegRatTensor <$> convertTensorOp1 convertRatTensor args
  VAddRatTensor args -> mkExpr accessAddRatTensor <$> convertTensorOp2 convertRatTensor args
  VSubRatTensor args -> mkExpr accessSubRatTensor <$> convertTensorOp2 convertRatTensor args
  VMulRatTensor args -> mkExpr accessMulRatTensor <$> convertTensorOp2 convertRatTensor args
  VDivRatTensor args -> mkExpr accessDivRatTensor <$> convertTensorOp2 convertRatTensor args
  VMinRatTensor args -> mkExpr accessMinRatTensor <$> convertTensorOp2 convertRatTensor args
  VMaxRatTensor args -> mkExpr accessMaxRatTensor <$> convertTensorOp2 convertRatTensor args
  VReduceAddRatTensor args -> mkExpr accessReduceAddRat <$> convertTensorReduction convertRatTensor args
  VReduceMulRatTensor args -> mkExpr accessReduceMulRat <$> convertTensorReduction convertRatTensor args
  VReduceMinRatTensor args -> mkExpr accessReduceMinRat <$> convertTensorReduction convertRatTensor args
  VReduceMaxRatTensor args -> mkExpr accessReduceMaxRat <$> convertTensorReduction convertRatTensor args
  VIfRatTensor args -> convertIf args
  VRatConstTensor args -> convertConstTensor convertRatTensor args
  VRatStackTensor args -> convertStackTensor convertRatTensor args
  VRatAt args -> convertAtTensor convertRatTensor args
  VRatForeach args -> convertForeachTensor convertRatTensor args

--------------------------------------------------------------------------------
-- Vector

-- Vector operations are converted to tensor operations.

convertVecLiteralArgs ::
  (MonadLogic m) =>
  (Value Builtin -> m (Value LossBuiltin)) ->
  (VType Builtin, VDims Builtin) ->
  VecLitArgs (Value Builtin) ->
  m (Value LossBuiltin)
convertVecLiteralArgs convertValue (elemType, dims) (VecLitArgs _typ dim xs) = do
  convertStackTensor convertValue $
    StackTensorArgs
      { stackType = elemType,
        stackFirstDim = dim,
        stackRemainingDims = dims,
        stackElements = xs
      }

convertVecForeachArgs ::
  (MonadLogic m) =>
  (Value Builtin -> m (Value LossBuiltin)) ->
  (VType Builtin, VDims Builtin) ->
  ForeachVectorArgs (Value Builtin) ->
  m (Value LossBuiltin)
convertVecForeachArgs convertValue (elemType, dims) (ForeachVectorArgs _typ dim xs) =
  convertForeachTensor convertValue $
    ForeachTensorArgs
      { foreachTensorType = elemType,
        foreachTensorFirstDim = dim,
        foreachTensorRemainingDims = dims,
        foreachTensorFn = xs
      }

--------------------------------------------------------------------------------
-- Tensor

convertTensorOp1 ::
  (MonadLogic m) =>
  (Value Builtin -> m (Value LossBuiltin)) ->
  TensorOp1Args (Value Builtin) ->
  m (TensorOp1Args (Value LossBuiltin))
convertTensorOp1 go (TensorOp1Args dims xs) =
  TensorOp1Args <$> convertDims dims <*> go xs

convertTensorOp2 ::
  (MonadLogic m) =>
  (Value Builtin -> m (Value LossBuiltin)) ->
  TensorOp2Args (Value Builtin) ->
  m (TensorOp2Args (Value LossBuiltin))
convertTensorOp2 go (TensorOp2Args dims xs ys) =
  TensorOp2Args <$> convertDims dims <*> go xs <*> go ys

convertTensorReduction ::
  (MonadLogic m) =>
  (Value Builtin -> m (Value LossBuiltin)) ->
  TensorReductionArgs (Value Builtin) ->
  m (TensorReductionArgs (Value LossBuiltin))
convertTensorReduction go (TensorReductionArgs dims e xs) =
  TensorReductionArgs <$> convertDims dims <*> go e <*> go xs

convertAtTensor ::
  (MonadLogic m) =>
  (Value Builtin -> m (Value LossBuiltin)) ->
  AtTensorArgs (Value Builtin) ->
  m (Value LossBuiltin)
convertAtTensor convertValue (AtTensorArgs typ dim dims xs i) = do
  type' <- convertType typ
  dim' <- convertDim dim
  dims' <- convertDims dims
  xs' <- convertValue xs
  i' <- convertIndex i
  return $ mkExpr accessAtTensor $ AtTensorArgs type' dim' dims' xs' i'

convertStackTensor ::
  (MonadLogic m) =>
  (Value Builtin -> m (Value LossBuiltin)) ->
  StackTensorArgs (Value Builtin) ->
  m (Value LossBuiltin)
convertStackTensor convertValue (StackTensorArgs typ dim dims xs) = do
  type' <- convertType typ
  dim' <- convertDim dim
  dims' <- convertDims dims
  xs' <- traverse convertValue xs
  return $ mkExpr accessStackTensor $ StackTensorArgs type' dim' dims' xs'

convertConstTensor ::
  (MonadLogic m) =>
  (Value Builtin -> m (Value LossBuiltin)) ->
  ConstTensorArgs (Value Builtin) ->
  m (Value LossBuiltin)
convertConstTensor convertValue (ConstTensorArgs typ value dims) = do
  type' <- convertType typ
  value' <- convertValue value
  dims' <- convertDims dims
  evalConstTensor $ ConstTensorArgs type' value' dims'

convertForeachTensor ::
  (MonadLogic m) =>
  (Value Builtin -> m (Value LossBuiltin)) ->
  ForeachTensorArgs (Value Builtin) ->
  m (Value LossBuiltin)
convertForeachTensor convertValue (ForeachTensorArgs t dim dims fn) = do
  t' <- convertType t
  dim' <- convertDim dim
  dims' <- convertDims dims
  fn' <- convertFunction convertValue fn
  return $ mkExpr accessForeachTensor $ ForeachTensorArgs t' dim' dims' fn'

--------------------------------------------------------------------------------
-- Utils

currentPass :: Doc a
currentPass = "logic translation"

logConversion ::
  (MonadLogger m, MonadReadableNameContext m) =>
  Value Builtin ->
  m (Value LossBuiltin) ->
  m (Value LossBuiltin)
logConversion e action = do
  logDebugM MaxDetail $ do
    inputDoc <- prettyFriendlyInCtx e
    return $ "enter-loss" <+> ":" <+> inputDoc
  incrCallDepth

  result <- action

  decrCallDepth
  logDebugM MaxDetail $ do
    outputDoc <- prettyFriendlyInCtx result
    return $ "exit-loss" <+> ": " <+> outputDoc

  return result
