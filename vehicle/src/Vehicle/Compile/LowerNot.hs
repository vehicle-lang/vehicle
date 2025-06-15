module Vehicle.Compile.LowerNot
  ( lowerNot,
    notClosure,
  )
where

import Vehicle.Compile.Error (MonadCompile)
import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Prelude (Expr (..), LoggingLevel (..), Lv, explicit, implicitIrrelevant, logDebug, normApp)
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Builtin.Standard ()
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Value
import Vehicle.Prelude (GenericArg (..))

--------------------------------------------------------------------------------
-- Not elimination

type MonadDropNot m =
  ( MonadCompile m
  )

-- | Tries to push in a `Not` as far as possible into a boolean expression.
-- If it is not possible to push it all the way through, it calls the continuation.
lowerNot ::
  forall m.
  (MonadDropNot m) =>
  Lv ->
  (Value Builtin -> m (Value Builtin)) ->
  TensorOp1Args (Value Builtin) ->
  m (Value Builtin)
lowerNot lv onBlocked (TensorOp1Args _ arg) = do
  logDebug MaxDetail "push-not"
  go arg
  where
    go :: Value Builtin -> m (Value Builtin)
    go e = case toBoolValue e of
      ----------------
      -- Base cases --
      ----------------
      VNot (TensorOp1Args _dims x) -> return x
      VBoolLiteral b -> return $ fromBoolValue $ VBoolLiteral (not b)
      VCompareIndex (op, args) -> return $ fromBoolValue $ VCompareIndex (neg op, args)
      VCompareNat (op, args) -> return $ fromBoolValue $ VCompareNat (neg op, args)
      VCompareRatTensorReduced (op, args) ->
        return $ fromBoolValue $ VCompareRatTensorReduced (neg op, args)
      VCompareRatTensorPointwise (op, args) ->
        return $ fromBoolValue $ VCompareRatTensorPointwise (neg op, args)
      -- We can't actually lower the `not` through the body of the quantifier as
      -- it is not yet unnormalised. However, it's fine to stop here as we'll
      -- simply continue to normalise it once we re-encounter it again after
      -- normalising the quantifier.
      VQuantifyRatTensor q dims binder closure -> do
        let negatedClosure = notClosure lv dims closure
        return $ fromBoolValue $ VQuantifyRatTensor (neg q) dims binder negatedClosure
      ---------------------
      -- Inductive cases --
      ---------------------
      VOr args -> fromBoolValue . VAnd <$> traverseTensorOp2Args go args
      VAnd args -> fromBoolValue . VOr <$> traverseTensorOp2Args go args
      VBoolIf args -> fromBoolValue . VBoolIf <$> traverseIfArgBranches go args
      VReduceOrTensor args -> fromBoolValue . VReduceAndTensor <$> traverseTensorOp2Args go args
      VReduceAndTensor args -> fromBoolValue . VReduceOrTensor <$> traverseTensorOp2Args go args
      -------------------
      -- Blocked cases --
      -------------------
      VBoolAt {} -> onBlocked e

notClosure :: Lv -> VArg Builtin -> Closure Builtin -> Closure Builtin
notClosure lv dims (Closure env body) = do
  let dims' = implicitIrrelevant $ quote mempty lv $ argExpr dims
  let newBody = normApp (Builtin mempty (BuiltinFunction Not)) [dims', explicit body]
  Closure env newBody
