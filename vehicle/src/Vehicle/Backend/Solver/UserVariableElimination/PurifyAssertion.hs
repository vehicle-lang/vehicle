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
import Vehicle.Data.Code.BooleanExpr (IfTree (..), forIfTreeM)
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Code.Interface
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
  let purifyFn = purifyRatTensorExpr actions 0
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

purifyRatTensorExpr ::
  (MonadPurify m) =>
  UnblockingActions m ->
  IncreasedDimensions ->
  Thunk Builtin ->
  m (IfTree (Thunk Builtin) (Thunk Builtin))
purifyRatTensorExpr actions@UnblockingActions {..} incrDims value = do
  showPurifyEntry value
  forcedValue <- forceAndRewriteTensor value
  showPurifyExit =<< case toRatTensorValue forcedValue of
    -- Pure operations
    VRatTensorLiteral {} -> return $ IfLeaf $ Forced forcedValue
    VNegRatTensor args -> purifyTensorOp1 (recPurify incrDims) accessNegRatTensor args
    VAddRatTensor args -> purifyTensorOp2 (recPurify incrDims) accessAddRatTensor args
    VSubRatTensor args -> purifyTensorOp2 (recPurify incrDims) accessSubRatTensor args
    VMulRatTensor args -> purifyTensorOp2 (recPurify incrDims) accessMulRatTensor args
    VDivRatTensor args -> purifyTensorOp2 (recPurify incrDims) accessDivRatTensor args
    VPowRatTensor args -> purifyTensorOp2 (recPurify incrDims) accessPowRatTensor args
    VLogRatTensor args -> purifyTensorOp1 (recPurify incrDims) accessLogRatTensor args
    VExpRatTensor args -> purifyTensorOp1 (recPurify incrDims) accessExpRatTensor args
    -- Recursively purify
    VRatConstTensor args -> unblockConstTensor actions args
    VRatStackTensor args -> purifyStackTensor args
    VIfRatTensor args -> unblockIf (recPurify incrDims) args
    VMinRatTensor args -> recPurify incrDims =<< purifyMinMax True args
    VMaxRatTensor args -> recPurify incrDims =<< purifyMinMax False args
    VReduceAddRatTensor args -> unblockReduceTensor (recPurify (incrDims + 1)) (forceEval evalReduceAddRatTensor) args
    VReduceMulRatTensor args -> unblockReduceTensor (recPurify (incrDims + 1)) (forceEval evalReduceMulRatTensor) args
    VReduceMinRatTensor args -> unblockReduceTensor (recPurify (incrDims + 1)) (forceEval evalReduceMinRatTensor) args
    VReduceMaxRatTensor args -> unblockReduceTensor (recPurify (incrDims + 1)) (forceEval evalReduceMaxRatTensor) args
    VRatAtTensor args -> unblockAtTensor (recPurify incrDims) (recPurify (incrDims + 1)) (unblockIndexValue actions) args
    VRatForeach args -> unblockForeachTensor args
    VRatTensorBoundVar v
      | incrDims == 0 -> return $ IfLeaf $ Forced $ VBoundVar v []
      | otherwise -> recPurify incrDims =<< unblockRatTensorBoundVar v
    VNetworkApplication n args -> unblockNetworkApp (recPurify incrDims) (unblockRecordValue actions) n args
    VRatTensorRecordAcc typ record fieldName spine -> unblockRecordAcc actions typ record fieldName spine
    VRatAtVector args -> unblockAtVector (unblockVectorValue actions) (unblockIndexValue actions) args
    VParameterOrDataset {} -> developerError "datasets and parameters should have been eliminated"
  where
    recPurify = purifyRatTensorExpr actions

purifyMinMax ::
  (MonadPurify m) =>
  Bool ->
  TensorOp2Args (Thunk Builtin) ->
  m (Thunk Builtin)
purifyMinMax isMin (TensorOp2Args ds xs ys) = do
  let typ = Forced $ ITensorType (Forced IRatType) ds
  let conditionArgs = TensorComparisonArgs (Forced IDimNil) ds xs ys
  let condition = Forced $ mkExpr accessCompareRatTensor (if isMin then Le else Ge, conditionArgs)
  return $ Forced $ mkExpr accessIf $ IfArgs typ condition xs ys

purifyTensorOp1 ::
  TypeUnblockingFunction (Thunk Builtin) m ->
  TensorOp1Accessor ForcedValue Thunk Builtin ->
  OperationUnblockingFunction TensorOp1Args (Thunk Builtin) m
purifyTensorOp1 unblock accessOp (TensorOp1Args ds xs) = do
  xs' <- unblock xs
  forIfTreeM xs' $ \xs'' ->
    return $
      IfLeaf $
        Forced $
          mkExpr accessOp $
            TensorOp1Args ds xs''

purifyTensorOp2 ::
  TypeUnblockingFunction (Thunk Builtin) m ->
  TensorOp2Accessor ForcedValue Thunk Builtin ->
  OperationUnblockingFunction TensorOp2Args (Thunk Builtin) m
purifyTensorOp2 unblock accessOp (TensorOp2Args ds xs ys) = do
  xs' <- unblock xs
  ys' <- unblock ys
  forIfTreeM xs' $ \xs'' ->
    forIfTreeM ys' $ \ys'' ->
      return $
        IfLeaf $
          Forced $
            mkExpr accessOp $
              TensorOp2Args ds xs'' ys''

purifyStackTensor :: OperationUnblockingFunction StackTensorArgs (Thunk Builtin) m
purifyStackTensor args = return $ IfLeaf $ Forced $ mkExpr accessStackTensor args

--------------------------------------------------------------------------------
-- Utilities

showPurifyEntry :: forall m. (MonadPurify m) => Thunk Builtin -> m ()
showPurifyEntry e = do
  ctx <- getNameContext
  -- logDebug MaxDetail $ "purify-entry" <+> prettyVerbose e
  logDebug MaxDetail $ "purify-entry:" <+> prettyFriendly (WithContext e ctx)
  incrCallDepth

showPurifyExit :: (MonadPurify m) => IfTree (Thunk Builtin) (Thunk Builtin) -> m (IfTree (Thunk Builtin) (Thunk Builtin))
showPurifyExit e = do
  ctx <- getNameContext
  decrCallDepth
  -- logDebug MaxDetail $ "purify-exit " <+> prettyVerbose e
  logDebug MaxDetail $ "purify-exit:" <+> prettyFriendly (WithContext e ctx)
  return e
