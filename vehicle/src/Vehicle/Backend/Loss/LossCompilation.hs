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
    convertTemporalOp1,
    convertTemporalOp2,
    convertGlobally,
    convertFinally,
    convertUntil,
    convertNot,
    convertOr,
    convertAnd,
    convertReduceAnd,
    convertReduceOr,
    convertIf,
    logConversion,
  )
where

import Data.Proxy (Proxy (..))
import Vehicle.Backend.Loss.Core hiding (currentPass)
import Vehicle.Compile.Normalise.NBE (evalApp, normaliseAppInEmptyFreeEnv, normaliseClosure)
import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print ()
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
import Vehicle.Data.Variable.Free.Context (runFreshFreeContextT)

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
  VTimeType -> unsupportedOperation "TimeType"

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
convertClosure convertValue binder closure = case closure of
  ExprClosure {} -> do
    normBody <- normaliseClosure binder closure
    finalCtx <- getShrunkenContext
    lossBody <- addNonTensorBinderToContext binder $ do
      normLossBody <- convertFunction convertValue normBody
      return $ quote mempty (1 + boundCtxLv finalCtx) normLossBody
    return $ ExprClosure (boundContextToEnv finalCtx) lossBody
  ValueClosure binderLv body -> do
    finalCtx <- getShrunkenContext
    -- Relabel the body's binder refs from construction-time `binderLv` to
    -- the slot the new addNonTensorBinderToContext will create.
    let newLv = boundCtxLv finalCtx
    let body' = relabelLvInValue binderLv newLv body
    lossBody <- addNonTensorBinderToContext binder $ do
      normLossBody <- convertFunction convertValue body'
      return $ quote mempty (1 + newLv) normLossBody
    return $ ExprClosure (boundContextToEnv finalCtx) lossBody

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

-- `convertBoolTensor`, the dispatcher, lives in `Vehicle.Backend.Loss.Domain`
-- to avoid a cycle: it dispatches `VBoolTensorQuantifyRat` to
-- `compileQuantifier`, which uses the helpers in this module.

convertBoolTensorLiteral :: (MonadLogic m) => Tensor Bool -> m (Value LossBuiltin)
convertBoolTensorLiteral tensor = do
  trueExpr <- getLogicField TruthityElement
  falseExpr <- getLogicField FalsityElement

  let convertBool b = if b then trueExpr else falseExpr
  -- Build the tensor structure via well-formed Stack args (element type first,
  -- then outer dim, then remaining dims) so `evalStackTensor` can fold
  -- fully-literal stacks into canonical tensor literals. The prior arg ordering
  -- left every Stack in non-canonical form, which then blocked `getConstValue`
  -- from spotting the trueElement (=0 for DL2) and stopped the `True AND X`
  -- identity drop in `evalAddRatTensor`.
  let foldLayer shape elems =
        let dim = length elems
            remDims = implicitIrrelevant (mkDims shape)
            args = implicit IRatType : implicit (INatLiteral dim) : remDims : fmap explicit elems
         in VBuiltin (LossBuiltinFunction StackTensor) args
  evalLossStackTree $ foldMapTensor convertBool foldLayer tensor

-- | Recursively normalise nested Stack constructors so literal stacks fold
-- to canonical tensor literals.
evalLossStackTree :: (MonadLogic m) => Value LossBuiltin -> m (Value LossBuiltin)
evalLossStackTree v = case getExpr accessStackTensor v of
  Just (StackTensorArgs t d ds xs) -> do
    xs' <- traverse evalLossStackTree xs
    evalStackTensor (StackTensorArgs t d ds xs')
  Nothing -> return v

convertNot :: (MonadLogic m) => TensorOp1Args (Value LossBuiltin) -> m (Value LossBuiltin)
convertNot = convertLogicField PointwiseNegation

convertAnd :: (MonadLogic m) => TensorOp2Args (Value LossBuiltin) -> m (Value LossBuiltin)
convertAnd = convertLogicField PointwiseConjunction

convertOr :: (MonadLogic m) => TensorOp2Args (Value LossBuiltin) -> m (Value LossBuiltin)
convertOr = convertLogicField PointwiseDisjunction

convertReduceAnd :: (MonadLogic m) => TensorReductionArgs (Value LossBuiltin) -> m (Value LossBuiltin)
convertReduceAnd = reduceWithTrivialSingleton ReduceConjunction

convertReduceOr :: (MonadLogic m) => TensorReductionArgs (Value LossBuiltin) -> m (Value LossBuiltin)
convertReduceOr = reduceWithTrivialSingleton ReduceDisjunction

-- | Reducing over a singleton axis (`Cons 1 Nil`) is algebraically an
-- identity: `OR [x] = AND [x] = x`. Substituting the logic's reduction
-- (e.g. DL2's `reduceMul falseElement`) here would multiply the body by the
-- sentinel `falseElement = 1e6`, blowing up the loss. Skip the wrap.
reduceWithTrivialSingleton ::
  (MonadLogic m) =>
  TensorDifferentiableLogicField ->
  TensorReductionArgs (Value LossBuiltin) ->
  m (Value LossBuiltin)
reduceWithTrivialSingleton field (TensorReductionArgs _keepDs reduceDs e xs) =
  case getDims reduceDs of
    Just [1] -> return xs
    _ -> convertLogicField field (TotalReductionArgs reduceDs e xs)

-- Emitted as opaque IR nodes; runtime semantics come from the DL record's
-- pointwise{Conjunction,Disjunction} + {true,false}Element fields, packaged
-- as JLogicMetadata by `Vehicle.Backend.Loss.JSON.convertLogicMetadata`.

convertGlobally :: (MonadLogic m) => TemporalOp1Args (Value LossBuiltin) -> m (Value LossBuiltin)
convertGlobally args = return $ mkExpr (accessTemporalLoss1 Globally) args

convertFinally :: (MonadLogic m) => TemporalOp1Args (Value LossBuiltin) -> m (Value LossBuiltin)
convertFinally args = return $ mkExpr (accessTemporalLoss1 Finally) args

convertUntil :: (MonadLogic m) => TemporalOp2Args (Value LossBuiltin) -> m (Value LossBuiltin)
convertUntil args = return $ mkExpr (accessTemporalLoss2 Until) args

-- | Accessor for unary temporal operators (Globally, Finally) in the loss IR.
accessTemporalLoss1 :: TemporalOperator -> Accessor (Value LossBuiltin) (TemporalOp1Args (Value LossBuiltin))
accessTemporalLoss1 op =
  Access
    { getExpr = \case
        VBuiltin (LossBuiltinFunction (Temporal op')) spine | op == op' -> getExpr accessSpine spine
        _ -> Nothing,
      mkExpr = \args -> VBuiltin (LossBuiltinFunction (Temporal op)) (mkExpr accessSpine args)
    }

-- | Accessor for binary temporal operators (Until) in the loss IR.
accessTemporalLoss2 :: TemporalOperator -> Accessor (Value LossBuiltin) (TemporalOp2Args (Value LossBuiltin))
accessTemporalLoss2 op =
  Access
    { getExpr = \case
        VBuiltin (LossBuiltinFunction (Temporal op')) spine | op == op' -> getExpr accessSpine spine
        _ -> Nothing,
      mkExpr = \args -> VBuiltin (LossBuiltinFunction (Temporal op)) (mkExpr accessSpine args)
    }

convertNatComparison :: (MonadLogic m) => (ComparisonOp, Op2Args (Value Builtin)) -> m (Value LossBuiltin)
convertNatComparison _args = unsupportedOperation "NatComparison"

convertIndexComparison :: (MonadLogic m) => (ComparisonOp, IndexComparisonArgs (Value Builtin)) -> m (Value LossBuiltin)
convertIndexComparison _args = unsupportedOperation "IndexComparison"

convertRatTensorPointwiseComparison :: (MonadLogic m) => (ComparisonOp, TensorOp2Args (Value Builtin)) -> m (Value LossBuiltin)
convertRatTensorPointwiseComparison (op, args) = do
  args' <- convertTensorOp2 convertRatTensor args
  convertLogicField (comparisonOpToField op) args'

convertRatTensorReducedComparison :: (MonadLogic m) => (ComparisonOp, TensorReduceComparisonArgs (Value Builtin)) -> m (Value LossBuiltin)
convertRatTensorReducedComparison (op, TensorReduceComparisonArgs d ds e1 e2) = do
  -- Decompose into: reduceAnd True (e1 <op>. e2)
  -- 1. Pointwise comparison on the full [d :: ds] dimensions
  let fullDims = IDimCons d ds
  compArgs <- convertTensorOp2 convertRatTensor (TensorOp2Args fullDims e1 e2)
  compResult <- convertLogicField (comparisonOpToField op) compArgs
  -- 2. Wrap in reduceAnd
  truthId <- getLogicField TruthityElement
  convertReduceAnd (TensorReductionArgs IDimNil (tensorOp2Dims compArgs) truthId compResult)

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
  VIndexLiteral i dim -> IIndexLiteral i <$> convertDim dim
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
  VPowRatTensor args -> mkExpr accessPowRatTensor <$> convertTensorOp2 convertRatTensor args
  VExpRatTensor args -> mkExpr accessExpRatTensor <$> convertTensorOp1 convertRatTensor args
  VLogRatTensor args -> mkExpr accessLogRatTensor <$> convertTensorOp2 convertRatTensor args
  VReduceAddRatTensor args -> mkExpr accessReduceAddRat <$> convertTensorReduction convertRatTensor args
  VReduceMulRatTensor args -> mkExpr accessReduceMulRat <$> convertTensorReduction convertRatTensor args
  VReduceMinRatTensor args -> mkExpr accessReduceMinRat <$> convertTensorReduction convertRatTensor args
  VReduceMaxRatTensor args -> mkExpr accessReduceMaxRat <$> convertTensorReduction convertRatTensor args
  VIfRatTensor args -> convertIf args
  VRatConstTensor args -> convertConstTensor convertRatTensor args
  VRatStackTensor args -> convertStackTensor convertRatTensor args
  VRatAt args -> convertAtTensor convertRatTensor args
  VRatForeach args -> convertForeachTensor convertRatTensor args
  VRatTensorRollout args -> convertRollout args
  VRatTensorTranspose args -> convertTranspose convertRatTensor args

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
convertTensorReduction go (TensorReductionArgs keepDims reduceDims e xs) =
  TensorReductionArgs <$> convertDims keepDims <*> convertDims reduceDims <*> go e <*> go xs

convertTemporalOp1 ::
  (MonadLogic m) =>
  (Value Builtin -> m (Value LossBuiltin)) ->
  TemporalOp1Args (Value Builtin) ->
  m (TemporalOp1Args (Value LossBuiltin))
convertTemporalOp1 go (TemporalOp1Args ds a b x) =
  TemporalOp1Args <$> convertDims ds <*> convertTimeBound a <*> convertTimeBound b <*> go x

convertTemporalOp2 ::
  (MonadLogic m) =>
  (Value Builtin -> m (Value LossBuiltin)) ->
  TemporalOp2Args (Value Builtin) ->
  m (TemporalOp2Args (Value LossBuiltin))
convertTemporalOp2 go (TemporalOp2Args ds a b x y) =
  TemporalOp2Args <$> convertDims ds <*> convertTimeBound a <*> convertTimeBound b <*> go x <*> go y

-- | A temporal bound must reduce to a literal before reaching the loss
-- backend; emit it as a Nat literal (Time and Nat share runtime semantics).
convertTimeBound :: (MonadLogic m) => Value Builtin -> m (Value LossBuiltin)
convertTimeBound value = case value of
  ITimeLiteral n -> return $ mkExpr accessNatLiteral n
  INatLiteral n -> return $ mkExpr accessNatLiteral n
  _ -> unsupportedOperation "non-literal temporal bound (compile-time reduction failed)"

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
convertForeachTensor convertValue args@(ForeachTensorArgs t dim dims fn) = do
  -- If the body contains an `IndexComparison`, the loss compiler cannot
  -- emit it symbolically. Materialise the foreach over its literal dim so
  -- per-iteration substitution reduces `(IIndexLiteral _ _ != IIndexLiteral _ _)`
  -- to a Bool literal before descending.
  if containsIndexComparison fn
    then do
      ctx <- getNameContext
      materialised <-
        runFreshFreeContextT (Proxy @Builtin) $
          unoptimisedEvalForeachTensor ctx evalApp args
      convertValue materialised
    else do
      t' <- convertType t
      dim' <- convertDim dim
      dims' <- convertDims dims
      fn' <- convertFunction convertValue fn
      return $ mkExpr accessForeachTensor $ ForeachTensorArgs t' dim' dims' fn'

-- | Conservatively check whether a value's syntax tree contains an
-- `IndexComparison` builtin. Used by `convertForeachTensor` to decide
-- whether to materialise the foreach for the loss compiler.
containsIndexComparison :: Value Builtin -> Bool
containsIndexComparison v
  | Just _ <- getExpr accessCompareIndex v = True
  | otherwise = case v of
      VBuiltin _ spine -> any (containsIndexComparison . argExpr) spine
      VBoundVar _ spine -> any (containsIndexComparison . argExpr) spine
      VFreeVar _ spine -> any (containsIndexComparison . argExpr) spine
      VLam _ closure -> case closure of
        ExprClosure _ _ -> False -- opaque; over-approximate as no-match
        ValueClosure _ body -> containsIndexComparison body
      _ -> False

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

convertRollout ::
  (MonadLogic m) =>
  RolloutArgs (Value Builtin) ->
  m (Value LossBuiltin)
convertRollout (RolloutArgs sType aType sDims aDims n ctrl dyn s0) = do
  sType' <- convertType sType
  aType' <- convertType aType
  sDims' <- convertDims sDims
  aDims' <- convertDims aDims
  n' <- convertTimeBound n
  ctrl' <- convertFunction convertRatTensor ctrl
  dyn' <- convertFunction convertRatTensor dyn
  s0' <- convertRatTensor s0
  return $ mkExpr accessRollout $ RolloutArgs sType' aType' sDims' aDims' n' ctrl' dyn' s0'

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
