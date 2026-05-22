module Vehicle.Compile.LowerNot
  ( lowerNot,
    negateQuantifierBody,
  )
where

import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Value
import Vehicle.Data.Tensor (mapTensor)
import Vehicle.Data.Variable.Bound.Context.Name

--------------------------------------------------------------------------------
-- Not elimination

type MonadDropNot m =
  ( MonadLogger m,
    MonadReadableNameContext m
  )

-- | Tries to push in a `Not` as far as possible into a boolean expression.
-- If it is not possible to push it all the way through, it calls the continuation.
lowerNot ::
  forall m.
  (MonadDropNot m) =>
  (Value Builtin -> m (Value Builtin)) ->
  TensorOp1Args (Value Builtin) ->
  m (Value Builtin)
lowerNot onBlocked (TensorOp1Args _ arg) = do
  result <- go arg
  ctx <- getNameContext
  logDebug MaxDetail $ "push-not:" <+> prettyFriendly (WithContext result ctx)
  return result
  where
    go :: Value Builtin -> m (Value Builtin)
    go e = case toBoolTensorValue e of
      ----------------
      -- Base cases --
      ----------------
      VBoolTensorLiteral b -> return $ fromBoolTensorValue $ VBoolTensorLiteral (mapTensor not b)
      VBoolTensorNot args -> return $ tensorOp1Arg args
      VBoolTensorCompareIndex (op, args) -> return $ fromBoolTensorValue $ VBoolTensorCompareIndex (neg op, args)
      VBoolTensorCompareNat (op, args) -> return $ fromBoolTensorValue $ VBoolTensorCompareNat (neg op, args)
      VBoolTensorCompareRatPointwise (op, args) -> return $ fromBoolTensorValue $ VBoolTensorCompareRatPointwise (neg op, args)
      VBoolTensorCompareRatReduced (op, args) -> return $ fromBoolTensorValue $ VBoolTensorCompareRatReduced (neg op, args)
      -- We can't actually lower the `not` through the body of the quantifier as
      -- it is not yet unnormalised. However, it's fine to stop here as we'll
      -- simply continue to normalise it once we re-encounter it again after
      -- normalising the quantifier.
      VBoolTensorQuantifyRat (q, args) -> fromBoolValue . VQuantifyRatTensor . (neg q,) <$> negateQuantifierBody args
      ---------------------
      -- Inductive cases --
      ---------------------
      VBoolConstTensor args -> fromBoolTensorValue . VBoolConstTensor <$> traverseConstTensorValue go args
      VBoolStackTensor args -> fromBoolTensorValue . VBoolStackTensor <$> traverseStackTensorElements go args
      VBoolTensorOr args -> fromBoolTensorValue . VBoolTensorAnd <$> traverseTensorOp2Args go args
      VBoolTensorAnd args -> fromBoolTensorValue . VBoolTensorOr <$> traverseTensorOp2Args go args
      VBoolTensorBoolIf args -> fromBoolTensorValue . VBoolTensorBoolIf <$> traverseIfArgBranches go args
      VBoolTensorReduceOr args -> fromBoolTensorValue . VBoolTensorReduceAnd <$> traverseReductionArgs go args
      VBoolTensorReduceAnd args -> fromBoolTensorValue . VBoolTensorReduceOr <$> traverseReductionArgs go args
      VBoolTensorAt args -> fromBoolTensorValue . VBoolTensorAt <$> traverseAtTensorArg go args
      -- STL De Morgan: not(G P) = F (not P); not(F P) = G (not P). Exact
      -- when the signal covers the interval; vehicle-stl's padded regime
      -- can drift slightly because the rewrite uses the disjunction
      -- identity as the mask whereas the original pads pessimistically.
      VBoolTensorGlobally args -> fromBoolTensorValue . VBoolTensorFinally <$> traverseTemporalOp1Args go args
      VBoolTensorFinally args -> fromBoolTensorValue . VBoolTensorGlobally <$> traverseTemporalOp1Args go args
      -- No primitive dual of Until without Release; wrap in Not after one
      -- last unblock attempt.
      VBoolTensorUntil (TemporalOp2Args dims _ _ _ _) -> do
        e' <- onBlocked e
        return $ fromBoolTensorValue $ VBoolTensorNot (TensorOp1Args dims e')
      VBoolTensorForeach args -> fromBoolTensorValue . VBoolTensorForeach <$> negateForeachArgs args

negateQuantifierBody ::
  (MonadReadableNameContext m) =>
  QuantifyRatTensorArgs (Value Builtin) (Closure Builtin) ->
  m (QuantifyRatTensorArgs (Value Builtin) (Closure Builtin))
negateQuantifierBody (QuantifyRatTensorArgs dims binder (Closure env body)) = do
  lv <- getBinderDepth
  let dims' = quote mempty lv dims
  let newBody = mkExpr accessNotTensor $ TensorOp1Args dims' body
  return $ QuantifyRatTensorArgs dims binder (Closure env newBody)

negateForeachArgs ::
  (MonadReadableNameContext m) =>
  ForeachTensorArgs (Value Builtin) ->
  m (ForeachTensorArgs (Value Builtin))
negateForeachArgs (ForeachTensorArgs t dim dims fn) = do
  (binder, Closure env body) <- case fn of
    VLam binder closure -> return (binder, closure)
    _ -> developerError "Malformed foreachTensor"
  lv <- getBinderDepth
  let dims' = quote mempty lv dims
  let newBody = mkExpr accessNotTensor $ TensorOp1Args dims' body
  return $ ForeachTensorArgs t dim dims (VLam binder (Closure env newBody))
