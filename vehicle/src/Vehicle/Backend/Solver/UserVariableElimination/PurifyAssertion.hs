module Vehicle.Backend.Solver.UserVariableElimination.PurifyAssertion
  ( purifyAssertion,
  )
where

import Vehicle.Compile.Normalise.Builtin
import Vehicle.Compile.Normalise.RewriteRules (forceAndRewriteTensor)
import Vehicle.Compile.Normalise.TypedValue
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Unblock
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.BooleanExpr (IfTree (..), forIfTreeListM, forIfTreeM)
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Code.Interface
import Vehicle.Data.Tensor (ExtendedRatTensor)
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Data.Variable.Free.Context (MonadFreeContext)

--------------------------------------------------------------------------------
-- Purification

type MonadPurify m =
  ( MonadLogger m,
    MonadFreeContext Builtin m,
    MonadNameContext m
  )

-- | Takes in a comparison over real tensors that possibly contains further
-- boolean structure via `if`s and returns a tree of the possible assertions.
purifyAssertion ::
  ( MonadPurify m
  ) =>
  UnblockingActions m ->
  ComparisonOp ->
  TensorComparisonArgs (Thunk Builtin) ->
  m (IfTree (Thunk Builtin) (ComparisonOp, TensorComparisonArgs (Thunk Builtin)))
purifyAssertion actions op (TensorComparisonArgs pDims rDims e1 e2) = do
  let purifyFn = purifyExpr actions 0
  e1' <- purifyFn e1
  e2' <- purifyFn e2
  forIfTreeM e1' $ \e1'' ->
    forIfTreeM e2' $ \e2'' ->
      return $ IfLeaf (op, TensorComparisonArgs pDims rDims e1'' e2'')

-- | The number of dimensions above the dimensions of the expression that
-- we're currently trying to compile. We track this so that we only
-- reduce bound hierarchical variables if we are currently at a larger
-- dimension than we're targeting.
type IncreasedDimensions = Int

purifyExpr ::
  (MonadPurify m) =>
  UnblockingActions m ->
  IncreasedDimensions ->
  Thunk Builtin ->
  m (IfTree (Thunk Builtin) (Thunk Builtin))
purifyExpr actions incrDims value = do
  showPurifyEntry incrDims value
  forcedValue <- forceAndRewriteTensor value
  let purifyFn = case toRatTensorValue forcedValue of
        VRatTensorLiteral t -> purifyRatTensor t
        VNegRatTensor args -> purifyTensorOp1 accessNegRatTensor args
        VLogRatTensor args -> purifyTensorOp1 accessLogRatTensor args
        VExpRatTensor args -> purifyTensorOp1 accessExpRatTensor args
        VAddRatTensor args -> purifyTensorOp2 accessAddRatTensor args
        VSubRatTensor args -> purifyTensorOp2 accessSubRatTensor args
        VMulRatTensor args -> purifyTensorOp2 accessMulRatTensor args
        VDivRatTensor args -> purifyTensorOp2 accessDivRatTensor args
        VPowRatTensor args -> purifyTensorOp2 accessPowRatTensor args
        VRatConstTensor args -> purifyConstTensor args
        VRatStackTensor args -> purifyStackTensor args
        VIfRatTensor args -> purifyIf args
        VMinRatTensor args -> purifyMinMax True args
        VMaxRatTensor args -> purifyMinMax False args
        VReduceAddRatTensor args -> purifyReduceTensor evalReduceAddRatTensor args
        VReduceMulRatTensor args -> purifyReduceTensor evalReduceMulRatTensor args
        VReduceMinRatTensor args -> purifyReduceTensor evalReduceMinRatTensor args
        VReduceMaxRatTensor args -> purifyReduceTensor evalReduceMaxRatTensor args
        VRatAtTensor args -> purifyAtTensor args
        VRatForeach args -> purifyForeachTensor args
        VRatTensorTranspose args -> purifyTransposeTensor args
        VRatTensorBoundVar v spine -> purifyBoundVar v spine
        VNetworkApplication ident args -> purifyNetworkVar ident args
        VRatTensorRecordAcc typ record fieldName spine -> purifyRecordAcc typ record fieldName spine
        VRatAtVector args -> purifyAtVector args
        VParameterOrDataset {} -> developerError "datasets and parameters should have been eliminated"
  showPurifyExit =<< purifyFn actions incrDims

type PurifyFn m =
  (MonadPurify m) =>
  UnblockingActions m ->
  IncreasedDimensions ->
  m (IfTree (Thunk Builtin) (Thunk Builtin))

purifyRatTensor ::
  ExtendedRatTensor ->
  PurifyFn m
purifyRatTensor tensor _actions _incrDims = do
  return $ IfLeaf $ Forced $ IRatTensor tensor

purifyBoundVar ::
  Lv ->
  UnforcedSpine Builtin ->
  PurifyFn m
purifyBoundVar v spine actions@UnblockingActions {..} incrDims
  | not (null spine) = unexpectedExprError "purification" "BoundVar with spine"
  | incrDims > 0 = unblockBoundVar (purifyExpr actions incrDims) v []
  | otherwise = return $ IfLeaf $ Forced $ VBoundVar v []

purifyNetworkVar ::
  Identifier ->
  NetworkAppArgs (Thunk Builtin) ->
  PurifyFn m
purifyNetworkVar ident args actions@UnblockingActions {..} incrDims = do
  unblockNetworkApp (purifyExpr actions incrDims) (unblockRecordValue actions) ident args

purifyConstTensor ::
  ConstTensorArgs (Thunk Builtin) ->
  PurifyFn m
purifyConstTensor args actions _incrDims = do
  unblockConstTensor (purifyExpr actions 0) actions args

purifyStackTensor ::
  StackTensorArgs (Thunk Builtin) ->
  PurifyFn m
purifyStackTensor args actions incrDims
  | incrDims > 1 = do
      xs' <- traverse (purifyExpr actions (incrDims - 1)) $ stackElements args
      forIfTreeListM xs' $ \xs'' ->
        IfLeaf
          <$> forceEvaluation accessStackTensor evalStackTensor (args {stackElements = xs''})
  | otherwise = return $ IfLeaf $ Forced $ mkExpr accessStackTensor args

purifyMinMax ::
  Bool ->
  TensorOp2Args (Thunk Builtin) ->
  (MonadPurify m) =>
  PurifyFn m
purifyMinMax isMin (TensorOp2Args ds xs ys) actions incrDims = do
  let typ = Forced $ ITensorType (Forced IRatType) ds
  let conditionArgs = TensorComparisonArgs (Forced IDimNil) ds xs ys
  let condition = Forced $ mkExpr accessCompareRatTensor (if isMin then Le else Ge, conditionArgs)
  let expanded = Forced $ mkExpr accessIf $ IfArgs typ condition xs ys
  purifyExpr actions incrDims expanded

purifyTensorOp1 ::
  TensorOp1Accessor ForcedValue Thunk Builtin ->
  TensorOp1Args (Thunk Builtin) ->
  PurifyFn m
purifyTensorOp1 accessOp args actions incrDims = do
  unblockTensorOp1
    (purifyExpr actions incrDims)
    (return . Forced . mkExpr accessOp)
    args

purifyTensorOp2 ::
  TensorOp2Accessor ForcedValue Thunk Builtin ->
  TensorOp2Args (Thunk Builtin) ->
  PurifyFn m
purifyTensorOp2 accessOp args actions incrDims = do
  unblockTensorOp2
    (purifyExpr actions incrDims)
    (return . Forced . mkExpr accessOp)
    args

purifyIf :: IfArgs (Thunk Builtin) -> PurifyFn m
purifyIf args actions incrDims = do
  unblockIf (purifyExpr actions incrDims) args

purifyReduceTensor ::
  EvalSimple ForcedValue Thunk TensorReductionArgs Builtin m ->
  TensorReductionArgs (Thunk Builtin) ->
  PurifyFn m
purifyReduceTensor eval args actions incrDims = do
  unblockReduceTensor
    (purifyExpr actions (incrDims + 1))
    (forceEval eval)
    args

purifyTransposeTensor :: TransposeTensorArgs (Thunk Builtin) -> PurifyFn m
purifyTransposeTensor args actions _incrDims = do
  dimsSize <- getDimsSize $ transposeDims args
  unblockTransposeTensor
    (purifyExpr actions dimsSize)
    args

purifyRecordAcc ::
  Thunk Builtin ->
  Thunk Builtin ->
  FieldName ->
  UnforcedSpine Builtin ->
  PurifyFn m
purifyRecordAcc typ record fieldName spine actions _incrDims = do
  unblockRecordAcc actions typ record fieldName spine

purifyForeachTensor :: ForeachTensorArgs (Thunk Builtin) -> PurifyFn m
purifyForeachTensor args actions _incrDims = do
  unblockForeachTensor actions args

purifyAtTensor :: AtTensorArgs (Thunk Builtin) -> PurifyFn m
purifyAtTensor args actions incrDims = do
  unblockAtTensor
    (purifyExpr actions incrDims)
    (purifyExpr actions (incrDims + 1))
    (unblockIndexValue actions)
    args

purifyAtVector :: AtVectorArgs (Thunk Builtin) -> PurifyFn m
purifyAtVector args actions _incrDims = do
  unblockAtVector
    (unblockVectorValue actions)
    (unblockIndexValue actions)
    args

getDimsSize :: (MonadPurify m) => Thunk Builtin -> m Int
getDimsSize dims = do
  let err = developerError "Unknown transpose dims"
  either err length <$> getDimsExprs dims

--------------------------------------------------------------------------------
-- Utilities

showPurifyEntry :: forall m. (MonadPurify m) => IncreasedDimensions -> Thunk Builtin -> m ()
showPurifyEntry incrDims e = do
  ctx <- getNameContext
  -- logDebug MaxDetail $ "purify-entry" <+> prettyVerbose e
  logDebug MaxDetail $ "purify-entry" <> parens (pretty incrDims) <> ":" <+> prettyFriendly (WithContext e ctx)
  incrCallDepth

showPurifyExit :: (MonadPurify m) => IfTree (Thunk Builtin) (Thunk Builtin) -> m (IfTree (Thunk Builtin) (Thunk Builtin))
showPurifyExit e = do
  ctx <- getNameContext
  decrCallDepth
  -- logDebug MaxDetail $ "purify-exit " <+> prettyVerbose e
  logDebug MaxDetail $ "purify-exit:" <+> prettyFriendly (WithContext e ctx)
  return e
