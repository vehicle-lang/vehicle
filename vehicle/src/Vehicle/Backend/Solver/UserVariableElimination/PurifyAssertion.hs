module Vehicle.Backend.Solver.UserVariableElimination.PurifyAssertion
  ( purifyAssertion,
  )
where

import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Unblock
import Vehicle.Data.Builtin.Interface.Normalise
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.BooleanExpr (IfTree (..), forIfTreeM)
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Value
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Data.Variable.Free.Context (MonadFreeContext)

--------------------------------------------------------------------------------
-- Purification

type MonadPurify m =
  ( MonadLogger m,
    MonadFreeContext Builtin m,
    MonadReadableNameContext m
  )

-- | Takes in a comparison over real tensors that possibly contains further
-- boolean structure via `if`s and returns a tree of the possible assertions.
purifyAssertion ::
  ( MonadLogger m,
    MonadFreeContext Builtin m,
    MonadReadableNameContext m
  ) =>
  UnblockingActions m ->
  ComparisonOp ->
  TensorOp2Args (Value Builtin) ->
  m (IfTree (Value Builtin) (ComparisonOp, TensorOp2Args (Value Builtin)))
purifyAssertion actions op (TensorOp2Args dims e1 e2) = do
  let purifyFn = purifyRatTensorExpr actions 0
  e1' <- purifyFn e1
  e2' <- purifyFn e2
  forIfTreeM e1' $ \e1'' ->
    forIfTreeM e2' $ \e2'' ->
      return $ IfLeaf (op, TensorOp2Args dims e1'' e2'')

-- | The number of dimensions above the dimensions of the expression that
-- we're currently trying to compile. We track this so that we only
-- reduce bound hierarchical variables if we are currently at a larger
-- dimension than we're targeting.
type IncreasedDimensions = Int

purifyRatTensorExpr ::
  (MonadPurify m) =>
  UnblockingActions m ->
  IncreasedDimensions ->
  Value Builtin ->
  m (IfTree (Value Builtin) (Value Builtin))
purifyRatTensorExpr actions@UnblockingActions {..} incrDims expr = do
  showPurifyEntry expr
  showPurifyExit =<< case toRatTensorValue expr of
    -- Pure operations
    VRatTensorLiteral {} -> return $ IfLeaf expr
    VNegRatTensor args -> purifyTensorOp1 (recPurify incrDims) VNegRatTensor args
    VAddRatTensor args -> purifyTensorOp2 (recPurify incrDims) VAddRatTensor args
    VSubRatTensor args -> purifyTensorOp2 (recPurify incrDims) VSubRatTensor args
    VMulRatTensor args -> purifyTensorOp2 (recPurify incrDims) VMulRatTensor args
    VDivRatTensor args -> purifyTensorOp2 (recPurify incrDims) VDivRatTensor args
    VPowRatTensor args -> purifyTensorOp2 (recPurify incrDims) VPowRatTensor args
    VLogRatTensor args -> purifyTensorOp1 (recPurify incrDims) VLogRatTensor args
    VExpRatTensor args -> purifyTensorOp1 (recPurify incrDims) VExpRatTensor args
    -- Recursively purify
    VRatConstTensor args -> purifyConstTensor args
    VRatStackTensor args -> purifyStackTensor (recPurify (incrDims - 1)) args
    VIfRatTensor args -> unblockIf (recPurify incrDims) args
    VMinRatTensor args -> unblockMinRatTensor (recPurify incrDims) args
    VMaxRatTensor args -> unblockMaxRatTensor (recPurify incrDims) args
    VReduceAddRatTensor args -> unblockReduceTensor (recPurify (incrDims + 1)) evalReduceAddRatTensor args
    VReduceMulRatTensor args -> unblockReduceTensor (recPurify (incrDims + 1)) evalReduceMulRatTensor args
    VReduceMinRatTensor args -> unblockReduceTensor (recPurify (incrDims + 1)) evalReduceMinRatTensor args
    VReduceMaxRatTensor args -> unblockReduceTensor (recPurify (incrDims + 1)) evalReduceMaxRatTensor args
    VRatAtTensor args -> unblockAtTensor (recPurify (incrDims + 1)) args
    VRatForeach args -> unblockForeachTensor args
    VRatTensorTranspose args -> unblockTranspose (recPurify incrDims) args
    VRatTensorBoundVar v
      | incrDims == 0 -> return $ IfLeaf $ fromRatTensorValue $ VRatTensorBoundVar v
      | otherwise -> recPurify incrDims =<< unblockRatTensorBoundVar v
    VRatTensorNetworkApp n args -> unblockNetworkApp (recPurify incrDims) (unblockRecordValue actions) n args
    VRatRecordAcc typ value fieldName _ -> unblockRecordAcc (recPurify incrDims) typ value fieldName
    VRatAtVector args -> unblockAtVector (recPurify (incrDims + 1)) args
    VDatasetOrParameter {} -> developerError "datasets and parameters should have been eliminated"
  where
    recPurify = purifyRatTensorExpr actions

purifyTensorOp1 ::
  TypeUnblockingFunction (Value Builtin) m ->
  (TensorOp1Args (Value Builtin) -> RatTensorValue) ->
  OperationUnblockingFunction TensorOp1Args (Value Builtin) m
purifyTensorOp1 unblock mkOp1 (TensorOp1Args ds xs) = do
  xs' <- unblock xs
  forIfTreeM xs' $ \xs'' ->
    return $
      IfLeaf $
        fromRatTensorValue $
          mkOp1 $
            TensorOp1Args ds xs''

purifyTensorOp2 ::
  TypeUnblockingFunction (Value Builtin) m ->
  (TensorOp2Args (Value Builtin) -> RatTensorValue) ->
  OperationUnblockingFunction TensorOp2Args (Value Builtin) m
purifyTensorOp2 unblock mkOp2 (TensorOp2Args ds xs ys) = do
  xs' <- unblock xs
  ys' <- unblock ys
  forIfTreeM xs' $ \xs'' ->
    forIfTreeM ys' $ \ys'' ->
      return $
        IfLeaf $
          fromRatTensorValue $
            mkOp2 $
              TensorOp2Args ds xs'' ys''

purifyConstTensor ::
  OperationUnblockingFunction ConstTensorArgs (Value Builtin) m
purifyConstTensor args = do
  return $ IfLeaf $ fromRatTensorValue $ VRatConstTensor args

purifyStackTensor ::
  TypeUnblockingFunction (Value Builtin) m ->
  OperationUnblockingFunction StackTensorArgs (Value Builtin) m
purifyStackTensor _unblock args = do
  return $ IfLeaf $ fromRatTensorValue $ VRatStackTensor args

--------------------------------------------------------------------------------
-- Utilities

showPurifyEntry :: forall m. (MonadPurify m) => Value Builtin -> m ()
showPurifyEntry e = do
  ctx <- getNameContext
  -- logDebug MaxDetail $ "purify-entry" <+> prettyVerbose e
  logDebug MaxDetail $ "purify-entry:" <+> prettyFriendly (WithContext e ctx)
  incrCallDepth

showPurifyExit :: (MonadPurify m) => IfTree (Value Builtin) (Value Builtin) -> m (IfTree (Value Builtin) (Value Builtin))
showPurifyExit e = do
  ctx <- getNameContext
  decrCallDepth
  -- logDebug MaxDetail $ "purify-exit " <+> prettyVerbose e
  logDebug MaxDetail $ "purify-exit:" <+> prettyFriendly (WithContext e ctx)
  return e
