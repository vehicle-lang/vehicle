module Vehicle.Backend.Loss.LossCompilation
  ( convertType,
    convertFunction,
    convertRatTensor,
    convertDims,
    convertBoundVar,
    convertVecLiteral,
    convertVecForeach,
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
import Vehicle.Data.Variable.Bound.Level (findSliceIndices)

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
  VVectorType tElem d -> IVectorType <$> convertType tElem <*> convertDim d
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
    (originalLv, maybeVars) <- lookupVariableInNestedCtx lv
    let var = VBoundVar originalLv []
    case maybeVars of
      Nothing -> return var
      Just (parentVar, sliceVar) -> do
        let indices = findSliceIndices parentVar sliceVar
        return $ mkIndexInto IRatType var (shapeOf parentVar) indices

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
  VBoolTensorQuantifyRecord {} -> unexpectedOperation "quantifier"
  VBoolTensorIf args -> convertIf args
  VBoolTensorAt args -> convertAtTensor convertBoolTensor args
  VBoolTensorForeach args -> convertForeachTensor convertBoolTensor args

convertBoolTensorLiteral :: (MonadLogic m) => Tensor Bool -> m (Value LossBuiltin)
convertBoolTensorLiteral tensor = do
  trueExpr <- getLogicFieldValue TruthityElement
  falseExpr <- getLogicFieldValue FalsityElement

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

{-
  -- This is horrendously unsound, and ill-typed but works for now.
  -- Really, we should compiling these to masking operations.
  -- However, that requires that we switch to normalisation by need...
  convertRatTensorPointwiseComparison (op, TensorOp2Args IDimNil (convertToRat x) (convertToRat y))
  where
    convertToRat :: Value Builtin -> Value Builtin
    convertToRat v = case toIndexValue v of
      VIndexLiteral value _ -> IRatTensor $ ZeroDimTensor (Finite $ toRational value)
      _ -> v
-}
convertRatTensorPointwiseComparison :: (MonadLogic m) => (ComparisonOp, TensorOp2Args (Value Builtin)) -> m (Value LossBuiltin)
convertRatTensorPointwiseComparison (op, args) = do
  args' <- convertTensorOp2 convertRatTensor args
  convertLogicField (comparisonOpToField op) args'

convertRatTensorReducedComparison :: (MonadLogic m) => (ComparisonOp, TensorReduceComparisonArgs (Value Builtin)) -> m (Value LossBuiltin)
convertRatTensorReducedComparison (op, TensorReduceComparisonArgs dim dims xs ys) = do
  -- Can't go via the definition in the standard library because we currently refold `reduceAnd` into the comparison.
  -- Can remove this hack once we get unified comparisons up and working.
  let fullDims = ICons INatType dim dims
  lPointwise <- convertRatTensorPointwiseComparison (op, TensorOp2Args fullDims xs ys)
  lFullDims <- convertDims fullDims
  convertReduceAnd $ TensorReductionArgs lFullDims lPointwise

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
  fn <- getLogicFieldValue field
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
  VIndexLiteral i dim -> IIndexLiteral i <$> convertDim dim
  VIndexBoundVar v spine -> convertBoundVar v spine
  VIndexParameter ident -> return $ VFreeVar ident []
  VIndexIf args -> convertIf args
  VIndexAtVector args -> convertAtVector convertIndex args

--------------------------------------------------------------------------------
-- Rat

convertRatTensor ::
  (MonadLogic m) =>
  Value Builtin ->
  m (Value LossBuiltin)
convertRatTensor value = logConversion value $ case toRatTensorValue value of
  VRatTensorBoundVar lv -> convertBoundVar lv mempty
  VRatTensorNetworkApp name args -> convertFreeVar name (mkExpr accessSpine args)
  VDatasetOrParameter name -> convertFreeVar name []
  VRatTensorLiteral t -> return $ mkExpr accessRatTensorLiteral t
  VNegRatTensor args -> mkExpr accessNegRatTensor <$> convertTensorOp1 convertRatTensor args
  VLogRatTensor args -> mkExpr accessLogRatTensor <$> convertTensorOp1 convertRatTensor args
  VExpRatTensor args -> mkExpr accessExpRatTensor <$> convertTensorOp1 convertRatTensor args
  VAddRatTensor args -> mkExpr accessAddRatTensor <$> convertTensorOp2 convertRatTensor args
  VSubRatTensor args -> mkExpr accessSubRatTensor <$> convertTensorOp2 convertRatTensor args
  VMulRatTensor args -> mkExpr accessMulRatTensor <$> convertTensorOp2 convertRatTensor args
  VDivRatTensor args -> mkExpr accessDivRatTensor <$> convertTensorOp2 convertRatTensor args
  VMinRatTensor args -> mkExpr accessMinRatTensor <$> convertTensorOp2 convertRatTensor args
  VMaxRatTensor args -> mkExpr accessMaxRatTensor <$> convertTensorOp2 convertRatTensor args
  VPowRatTensor args -> mkExpr accessPowRatTensor <$> convertTensorOp2 convertRatTensor args
  VReduceAddRatTensor args -> mkExpr accessReduceAddRat <$> convertTensorReduction convertRatTensor args
  VReduceMulRatTensor args -> mkExpr accessReduceMulRat <$> convertTensorReduction convertRatTensor args
  VReduceMinRatTensor args -> mkExpr accessReduceMinRat <$> convertTensorReduction convertRatTensor args
  VReduceMaxRatTensor args -> mkExpr accessReduceMaxRat <$> convertTensorReduction convertRatTensor args
  VIfRatTensor args -> convertIf args
  VRatConstTensor args -> convertConstTensor convertRatTensor args
  VRatStackTensor args -> convertStackTensor convertRatTensor args
  VRatAtTensor args -> convertAtTensor convertRatTensor args
  VRatAtVector args -> convertAtVector (convertVector convertRatTensor) args
  VRatForeach args -> convertForeachTensor convertRatTensor args
  VRatTensorTranspose args -> convertTranspose convertRatTensor args
  VRatRecordAcc {} -> developerError "Record accesses in loss functions are not supported yet"

--------------------------------------------------------------------------------
-- Vector

convertVector ::
  (MonadLogic m) =>
  (Value Builtin -> m (Value LossBuiltin)) ->
  Value Builtin ->
  m (Value LossBuiltin)
convertVector convertElem value = do
  case toVectorValue value of
    VVectorBoundVar lv spine -> convertBoundVar lv spine
    VVectorDataset ident -> return $ VFreeVar ident []
    VVectorLiteral args -> convertVecLiteral convertElem args
    VVectorIf args -> convertIf args
    VVectorForeach args -> convertVecForeach convertElem args

convertVecLiteral ::
  (MonadLogic m) =>
  (Value Builtin -> m (Value LossBuiltin)) ->
  VectorLitArgs (Value Builtin) ->
  m (Value LossBuiltin)
convertVecLiteral convert (VectorLitArgs typ dim xs) = do
  typ' <- convertType typ
  dim' <- convertDim dim
  xs' <- traverse convert xs
  return $ mkExpr accessVecLit $ VectorLitArgs typ' dim' xs'

convertVecForeach ::
  (MonadLogic m) =>
  (Value Builtin -> m (Value LossBuiltin)) ->
  ForeachVectorArgs (Value Builtin) ->
  m (Value LossBuiltin)
convertVecForeach convert (ForeachVectorArgs typ dim xs) = do
  typ' <- convertType typ
  dim' <- convertDim dim
  xs' <- convertFunction convert xs
  return $ mkExpr accessForeachVector $ ForeachVectorArgs typ' dim' xs'

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
convertTensorReduction go (TensorReductionArgs dims xs) =
  TensorReductionArgs <$> convertDims dims <*> go xs

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

convertTranspose ::
  (MonadLogic m) =>
  (Value Builtin -> m (Value LossBuiltin)) ->
  TransposeArgs (Value Builtin) ->
  m (Value LossBuiltin)
convertTranspose convertValue (TransposeArgs t ds xs) = do
  t' <- convertType t
  ds' <- convertDims ds
  xs' <- convertValue xs
  return $ mkExpr accessTranspose $ TransposeArgs t' ds' xs'
convertAtVector ::
  (MonadLogic m) =>
  (Value Builtin -> m (Value LossBuiltin)) ->
  AtVectorArgs (Value Builtin) ->
  m (Value LossBuiltin)
convertAtVector convertVec (AtVectorArgs typ dim xs i) = do
  type' <- convertType typ
  dim' <- convertDim dim
  xs' <- convertVec xs
  i' <- convertIndex i
  return $ mkExpr accessAtVector $ AtVectorArgs type' dim' xs' i'

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
