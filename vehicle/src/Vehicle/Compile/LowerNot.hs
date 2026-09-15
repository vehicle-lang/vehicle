module Vehicle.Compile.LowerNot
  ( lowerNot,
    negateQuantifierBody,
    negateRecordQuantifierBody,
  )
where

import Vehicle.Compile.Normalise.Force (forceThunk)
import Vehicle.Compile.Normalise.Quote (unnormalise)
import Vehicle.Compile.Normalise.TypedValue
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Compile.Unblock (UnblockingActions, unblockBoolExpr)
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Code.Interface
import Vehicle.Data.Tensor (mapTensor)
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Data.Variable.Free.Context (MonadFreeContext)

--------------------------------------------------------------------------------
-- Not elimination

type MonadDropNot m =
  ( MonadLogger m,
    MonadNameContext m,
    MonadFreeContext Builtin m
  )

-- | Pushes a `Not` into a boolean expression.
-- TODO: can uses of this be removed now that is part of `RewriteRules`?
lowerNot ::
  forall m.
  (MonadDropNot m) =>
  UnblockingActions m ->
  TensorOp1Args (Thunk Builtin) ->
  m (Thunk Builtin)
lowerNot actions (TensorOp1Args dims value) = do
  forcedValue <- forceThunk value
  result <- case toBoolTensorValue forcedValue of
    -- Base cases
    VBoolTensorLiteral b -> return $ Forced $ mkExpr accessBoolTensorLiteral (mapTensor not b)
    VBoolTensorNot args -> return $ tensorOp1Arg args
    VBoolTensorCompareIndex (op, args) -> return $ Forced $ mkExpr accessCompareIndex (neg op, args)
    VBoolTensorCompareNat (op, args) -> return $ Forced $ mkExpr accessCompareNat (neg op, args)
    VBoolTensorCompareRatTensor (op, args) -> negateCompareRatTensorArgs op args
    VBoolTensorQuantifyRat (q, args) -> return $ Forced $ mkExpr accessQuantifyRatTensor (neg q, negateQuantifierBody args)
    VBoolTensorQuantifyRecord (q, args) -> return $ Forced $ mkExpr accessQuantifyRecord (neg q, negateRecordQuantifierBody args)
    -- Recursive cases
    VBoolConstTensor args -> Forced . mkExpr accessConstTensor <$> negateConstTensorArgs args
    VBoolStackTensor args -> Forced . mkExpr accessStackTensor <$> negateStackTensorArgs args
    VBoolTensorOr args -> Forced . mkExpr accessAndTensor <$> negateOp2Args args
    VBoolTensorAnd args -> Forced . mkExpr accessOrTensor <$> negateOp2Args args
    VBoolTensorImplies args -> Forced <$> negateImplication args
    VBoolTensorIf args -> Forced . mkExpr accessIf <$> negateIfArgs dims args
    VBoolTensorReduceOr args -> Forced . mkExpr accessReduceAnd <$> negateReductionArgs args
    VBoolTensorReduceAnd args -> Forced . mkExpr accessReduceOr <$> negateReductionArgs args
    VBoolTensorTensorAt args -> Forced . mkExpr accessAtTensor <$> negateAtTensorArgs args
    VBoolTensorForeach args -> Forced . mkExpr accessForeachTensor <$> negateForeachArgs args
    VBoolTensorFoldList {} -> unblockAndNegate forcedValue
    VBoolTensorVectorAt {} -> unblockAndNegate forcedValue

  logDebugM MaxDetail $ do
    ctx <- getNameContext
    return $ "push-not:" <+> prettyFriendly (WithContext result ctx)

  return result
  where
    unblockAndNegate :: ForcedValue Builtin -> m (Thunk Builtin)
    unblockAndNegate v = do
      result <- unblockBoolExpr actions (Forced v)
      lowerNot actions $ TensorOp1Args dims result

    negateThunk :: Thunk Builtin -> Thunk Builtin -> m (Thunk Builtin)
    negateThunk ds v = lowerNot actions $ TensorOp1Args ds v

    negateImplication :: TensorOp2Args (Thunk Builtin) -> m (ForcedValue Builtin)
    negateImplication (TensorOp2Args ds x y) = do
      negY <- negateThunk dims y
      return $ mkExpr accessAndTensor $ TensorOp2Args ds x negY

    negateOp2Args :: TensorOp2Args (Thunk Builtin) -> m (TensorOp2Args (Thunk Builtin))
    negateOp2Args args = traverseTensorOp2Args (negateThunk (tensorOp2Dims args)) args

    negateCompareRatTensorArgs :: ComparisonOp -> TensorComparisonArgs (Thunk Builtin) -> m (Thunk Builtin)
    negateCompareRatTensorArgs op (TensorComparisonArgs pDims rDims xs ys) = do
      fpDims <- forceThunk pDims
      frDims <- forceThunk rDims
      case (fpDims, frDims) of
        (IDimNil, _) -> do
          let pointwiseComparison = Forced $ mkExpr accessCompareRatTensor (neg op, TensorComparisonArgs (Forced fpDims) (Forced frDims) xs ys)
          return $ Forced $ mkExpr accessReduceOr $ TensorReductionArgs dims pointwiseComparison
        (_, IDimNil) -> return $ Forced $ mkExpr accessCompareRatTensor (neg op, TensorComparisonArgs pDims rDims xs ys)
        _ -> developerError "negation of mixed comparisons not yet implemented"

    negateReductionArgs :: TensorReductionArgs (Thunk Builtin) -> m (TensorReductionArgs (Thunk Builtin))
    negateReductionArgs args = traverseReductionArgs (negateThunk (tensorReductionDims args)) args

    negateIfArgs :: Thunk Builtin -> IfArgs (Thunk Builtin) -> m (IfArgs (Thunk Builtin))
    negateIfArgs ds = traverseIfArgBranches (negateThunk ds)

    negateConstTensorArgs :: ConstTensorArgs (Thunk Builtin) -> m (ConstTensorArgs (Thunk Builtin))
    negateConstTensorArgs = traverseConstTensorValue (negateThunk (Forced IDimNil))

    negateStackTensorArgs :: StackTensorArgs (Thunk Builtin) -> m (StackTensorArgs (Thunk Builtin))
    negateStackTensorArgs args = traverseStackTensorElements (negateThunk (stackRemainingDims args)) args

    negateAtTensorArgs :: AtTensorArgs (Thunk Builtin) -> m (AtTensorArgs (Thunk Builtin))
    negateAtTensorArgs args@AtTensorArgs {..} =
      traverseAtTensorArg (negateThunk $ Forced (IDimCons atFirstDim atRemainingDims)) args

    negateForeachArgs ::
      (MonadDropNot m) =>
      ForeachTensorArgs (Thunk Builtin) ->
      m (ForeachTensorArgs (Thunk Builtin))
    negateForeachArgs (ForeachTensorArgs t d ds fn) = do
      forcedFn <- forceThunk fn
      (binder, Closure env body) <- case forcedFn of
        VLam binder closure -> return (binder, closure)
        _ -> developerError "Malformed foreachTensor"
      lv <- getBinderDepth
      let ds' = unnormalise lv ds
      let newBody = mkExpr accessNotTensor $ TensorOp1Args ds' body
      let newFn = Forced $ VLam binder (Closure env newBody)
      return $ ForeachTensorArgs t d ds newFn

negateQuantifierBody ::
  QuantifyRatTensorArgs (Thunk Builtin) (Closure Builtin) ->
  QuantifyRatTensorArgs (Thunk Builtin) (Closure Builtin)
negateQuantifierBody (QuantifyRatTensorArgs pDims bDims binder (Closure env body)) = do
  let newBody = mkExpr accessNotTensor $ TensorOp1Args IDimNil body
  QuantifyRatTensorArgs
    { quantifyPointwiseDims = pDims,
      quantifyBaseDims = bDims,
      quantifyBinder = binder,
      quantifyBody = Closure env newBody
    }

negateRecordQuantifierBody ::
  QuantifyRecordArgs (Thunk Builtin) (Closure Builtin) ->
  QuantifyRecordArgs (Thunk Builtin) (Closure Builtin)
negateRecordQuantifierBody (QuantifyRecordArgs typ binder (Closure env body)) = do
  let newBody = mkExpr accessNotTensor $ TensorOp1Args IDimNil body
  QuantifyRecordArgs
    { quantifyRecordType = typ,
      quantifyRecordBinder = binder,
      quantifyRecordBody = Closure env newBody
    }
