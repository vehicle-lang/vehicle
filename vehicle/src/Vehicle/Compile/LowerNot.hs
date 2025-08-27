module Vehicle.Compile.LowerNot
  ( lowerNot,
    notClosure,
  )
where

import Vehicle.Compile.Error (MonadCompile)
import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Builtin.Standard ()
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Value
import Vehicle.Syntax.Tensor (mapTensor)

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
  NamedBoundCtx ->
  (Value Builtin -> m (Value Builtin)) ->
  TensorOp1Args (Value Builtin) ->
  m (Value Builtin)
lowerNot ctx onBlocked (TensorOp1Args _ arg) = do
  result <- go arg
  logDebug MaxDetail $ "push-not" <+> prettyFriendly (WithContext result ctx)
  return result
  where
    go :: Value Builtin -> m (Value Builtin)
    go e = do
      logDebug MaxDetail $ prettyFriendly (WithContext e ctx)
      case toBoolTensorValue e of
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
        VBoolTensorQuantifyRat q dims binder closure -> do
          let negatedClosure = notClosure (boundCtxLv ctx) dims closure
          return $ fromBoolValue $ VQuantifyRatTensor (neg q) dims binder negatedClosure
        ---------------------
        -- Inductive cases --
        ---------------------
        VBoolConstTensor args -> fromBoolTensorValue . VBoolConstTensor <$> traverseConstTensorValue go args
        VBoolStackTensor args -> fromBoolTensorValue . VBoolStackTensor <$> traverseStackTensorElements go args
        VBoolTensorOr args -> fromBoolTensorValue . VBoolTensorAnd <$> traverseTensorOp2Args go args
        VBoolTensorAnd args -> fromBoolTensorValue . VBoolTensorOr <$> traverseTensorOp2Args go args
        VBoolTensorBoolIf args -> fromBoolTensorValue . VBoolTensorBoolIf <$> traverseIfArgBranches go args
        VBoolTensorReduceOr args -> fromBoolTensorValue . VBoolTensorReduceAnd <$> traverseTensorOp2Args go args
        VBoolTensorReduceAnd args -> fromBoolTensorValue . VBoolTensorReduceOr <$> traverseTensorOp2Args go args
        VBoolTensorAt args -> fromBoolTensorValue . VBoolTensorAt <$> traverseAtTensorArg go args
        VBoolTensorForeach {} -> onBlocked e

notClosure :: Lv -> VArg Builtin -> Closure Builtin -> Closure Builtin
notClosure lv dims (Closure env body) = do
  let dims' = implicitIrrelevant $ quote mempty lv $ argExpr dims
  let newBody = normApp (Builtin mempty (BuiltinFunction Not)) [dims', explicit body]
  Closure env newBody
