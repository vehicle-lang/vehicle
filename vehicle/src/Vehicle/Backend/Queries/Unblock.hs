module Vehicle.Backend.Queries.Unblock
  ( unblockBoolExpr,
    tryPurifyAssertion,
    UnblockingActions (..),
    unblockRatTensorValue,
  )
where

import Control.Monad (when)
import Vehicle.Compile.Context.Free (MonadFreeContext, getFreeEnv)
import Vehicle.Compile.Context.Name (MonadNameContext, getNameContext)
import Vehicle.Compile.Error
import Vehicle.Compile.LiftIf
import Vehicle.Compile.Normalise.NBE (evalApp)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Data.Builtin.Interface.Normalise
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Value

--------------------------------------------------------------------------------
-- Unblocking
--------------------------------------------------------------------------------

type MonadUnblock m = (MonadCompile m, MonadFreeContext Builtin m, MonadNameContext m)

type MonadPurify m = MonadUnblock m

data UnblockingActions m = UnblockingActions
  { unblockRatTensorBoundVar :: Lv -> m (Value Builtin),
    unblockNetworkApp :: Name -> NetworkAppArgs (Value Builtin) -> m (Value Builtin)
  }

-- | Lifts all `if`s in the provided expression `e` to the top-level, while
-- preserving the guarantee that the expression is normalised as much as
-- possible.
unblockBoolExpr ::
  (MonadUnblock m) =>
  UnblockingActions m ->
  Value Builtin ->
  m (Value Builtin)
unblockBoolExpr actions expr = do
  ctx <- getNameContext
  let exprDoc = prettyFriendly (WithContext expr ctx)
  logDebug MaxDetail $ line <> "unblocking" <+> squotes exprDoc
  incrCallDepth

  unblockedExpr <- unblockBoolTensorValue actions expr
  let unblockedExprDoc = prettyFriendly (WithContext unblockedExpr ctx)

  when (layoutAsString exprDoc == layoutAsString unblockedExprDoc) $
    developerError $
      "Failed to unblock expression:" <+> exprDoc

  decrCallDepth
  return unblockedExpr

--------------------------------------------------------------------------------
-- Purification

tryPurifyAssertion ::
  (MonadPurify m) =>
  UnblockingActions m ->
  ComparisonOp ->
  TensorOp2Args (Value Builtin) ->
  m (Either (Value Builtin) (TensorOp2Args (Value Builtin)))
tryPurifyAssertion actions op args = do
  unblockedExpr <- unblockTensorOp2 (unblockRatTensorValue actions VarLevel) (evalCompareRatTensor op) args

  logDebugM MaxDetail $ do
    ctx <- getNameContext
    let unblockedAssertionDoc = prettyVerbose (WithContext unblockedExpr ctx)
    return ("result:" <+> unblockedAssertionDoc)

  case findImpurity unblockedExpr of
    Right newArgs -> do
      logDebug MaxDetail "status: pure"
      return $ Right newArgs
    Left impurity -> do
      logDebug MaxDetail "status: impure"
      Left <$> eliminateImpurities impurity

data Impurity
  = LiftedIf (IfArgs (Value Builtin))
  | LiftedMinMax (Bool, TensorOp2Args (Value Builtin)) ComparisonOp (Value Builtin)
  | ReducedComparison (Value Builtin)

findImpurity :: Value Builtin -> Either Impurity (TensorOp2Args (Value Builtin))
findImpurity expr = case toBoolValue expr of
  VBoolIf args -> Left $ LiftedIf args
  VCompareRatTensor (op, args) -> maybe (Right args) Left $ findMinMaxImpurity op args
  -- The purification may have caused the comparison itself to reduce to comparisons
  -- over its elements and/or tensor literals.
  _ -> Left $ ReducedComparison expr
  where
    findMinMaxImpurity :: ComparisonOp -> TensorOp2Args (Value Builtin) -> Maybe Impurity
    findMinMaxImpurity op (TensorOp2Args _ e1 e2) = case (toRatTensorValue e1, toRatTensorValue e2) of
      (VMinRatTensor args, _) -> Just $ LiftedMinMax (True, args) op e2
      (_, VMinRatTensor args) -> Just $ LiftedMinMax (True, args) (flipOrder op) e1
      (VMaxRatTensor args, _) -> Just $ LiftedMinMax (False, args) op e2
      (_, VMaxRatTensor args) -> Just $ LiftedMinMax (False, args) (flipOrder op) e1
      _ -> Nothing

eliminateImpurities :: (MonadPurify m) => Impurity -> m (Value Builtin)
eliminateImpurities impurity = do
  case impurity of
    LiftedIf args -> unfoldIf args
    LiftedMinMax (isMin, TensorOp2Args dims e1 e2) op value -> do
      let comparison1 = fromBoolValue $ VCompareRatTensor (op, TensorOp2Args dims e1 value)
      let comparison2 = fromBoolValue $ VCompareRatTensor (op, TensorOp2Args dims e2 value)
      let logicalArgs = TensorOp2Args dims comparison1 comparison2
      if op == Le || op == Lt
        then (if isMin then evalOr else evalAnd) logicalArgs
        else
          if op == Ge || op == Gt
            then (if isMin then evalAnd else evalOr) logicalArgs
            else developerError $ "Support for min/max with" <+> pretty op <+> "not yet implemented"
    ReducedComparison expr -> return expr

--------------------------------------------------------------------------------
-- Main unblocking functions

type UnblockingFunction m = (MonadUnblock m) => Value Builtin -> m (Value Builtin)

unblockBoolTensorValue :: UnblockingActions m -> UnblockingFunction m
unblockBoolTensorValue actions expr = do
  showEntry expr
  showExit =<< case toBoolValue expr of
    -- Already unblocked
    VBoolLiteral {} -> return expr
    VAnd {} -> return expr
    VOr {} -> return expr
    VNot {} -> return expr
    VBoolIf {} -> return expr
    VQuantifyRatTensor {} -> return expr
    -- Recursively unblock
    VReduceAndTensor args -> unblockReduceTensor unblock evalReduceAndTensor args
    VReduceOrTensor args -> unblockReduceTensor unblock evalReduceOrTensor args
    VCompareIndex (op, args) -> unblockIndexOp2 (evalCompareIndex op) args
    VCompareNat (op, args) -> unblockOp2 return (evalCompareNat op) args
    VCompareRatTensor (op, args) -> unblockTensorOp2 (unblockRatTensorValue actions VarLevel) (evalCompareRatTensor op) args
    VBoolAt args -> unblockAtTensor unblock args
  where
    unblock = unblockBoolTensorValue actions

data Depth = VarLevel | NonVarLevel
  deriving (Eq)

unblockRatTensorValue :: (MonadPurify m) => UnblockingActions m -> Depth -> Value Builtin -> m (Value Builtin)
unblockRatTensorValue actions@UnblockingActions {..} lv expr = do
  showEntry expr
  showExit =<< case toRatTensorValue expr of
    -- Rational operators
    VRatTensorLiteral {} -> return expr
    VIfRatTensor {} -> return expr
    VMinRatTensor {} -> return expr
    VMaxRatTensor {} -> return expr
    -- Recursively purify
    VNegRatTensor args -> unblockTensorOp1 (unblock lv) evalNegRatTensor args
    VAddRatTensor args -> unblockTensorOp2 (unblock lv) evalAddRatTensor args
    VSubRatTensor args -> unblockTensorOp2 (unblock lv) evalSubRatTensor args
    VMulRatTensor args -> unblockTensorOp2 (unblock lv) evalMulRatTensor args
    VDivRatTensor args -> unblockTensorOp2 (unblock lv) evalDivRatTensor args
    VReduceAddRatTensor args -> unblockReduceTensor (unblock NonVarLevel) evalReduceAddRatTensor args
    VReduceMulRatTensor args -> unblockReduceTensor (unblock NonVarLevel) evalReduceMulRatTensor args
    VReduceMinRatTensor args -> unblockReduceTensor (unblock NonVarLevel) evalReduceMinRatTensor args
    VReduceMaxRatTensor args -> unblockReduceTensor (unblock NonVarLevel) evalReduceMaxRatTensor args
    VRatTensorVar v
      | lv == VarLevel -> return expr
      | otherwise -> unblockRatTensorBoundVar v
    VNetworkApp n args -> unblock lv =<< unblockNetworkApp (nameOf n) args
    VRatConstTensor args -> unblockConstTensor args
    VRatStackTensor args -> unblockStackTensor (unblock NonVarLevel) args
    VRatAt args -> unblockAtTensor (unblock NonVarLevel) args
    VRatForeach args -> unblockForeachTensor args
  where
    unblock = unblockRatTensorValue actions

unblockDimensionsValue :: UnblockingFunction m
unblockDimensionsValue expr = case toDimensionsValue expr of
  VDimsNil {} -> return expr
  VDimsCons {} -> return expr
  VDimsIf {} -> return expr
  VDimsBoundVar {} -> unexpectedExprError currentPass (prettyVerbose expr)

unblockIndexValue :: UnblockingFunction m
unblockIndexValue expr = case toIndexValue expr of
  VIndexLiteral {} -> return expr
  VIndexIf {} -> return expr
  VIndexBoundVar {} -> unexpectedExprError currentPass (prettyVerbose expr)

unblockNatValue :: UnblockingFunction m
unblockNatValue expr = case toNatValue expr of
  VNatLiteral {} -> return expr
  VNatIf {} -> return expr
  VNatAdd args -> unblockOp2 unblockNatValue evalAddNat args
  VNatMul args -> unblockOp2 unblockNatValue evalMulNat args
  VNatBoundVar {} -> unexpectedExprError currentPass (prettyVerbose expr)
  VNatParameter {} -> unexpectedExprError currentPass (prettyVerbose expr)

--------------------------------------------------------------------------------
-- Unblocking individual operations

unblockOp2 ::
  (MonadUnblock m) =>
  UnblockingFunction m ->
  EvalSimple Op2Args Value Builtin m ->
  Op2Args (Value Builtin) ->
  m (Value Builtin)
unblockOp2 unblock evalFn (Op2Args x y) = do
  x' <- unblock x
  y' <- unblock y
  liftIf x' $ \x'' ->
    liftIf y' $ \y'' -> do
      evalFn $ Op2Args x'' y''

unblockIndexOp2 ::
  (MonadUnblock m) =>
  EvalSimple IndexComparisonArgs Value Builtin m ->
  IndexComparisonArgs (Value Builtin) ->
  m (Value Builtin)
unblockIndexOp2 evalFn (IndexCompArgs n1 n2 x y) = do
  x' <- unblockIndexValue x
  y' <- unblockIndexValue y
  liftIf x' $ \x'' ->
    liftIf y' $ \y'' -> do
      evalFn $ IndexCompArgs n1 n2 x'' y''

unblockTensorOp1 ::
  (MonadUnblock m) =>
  UnblockingFunction m ->
  EvalSimple TensorOp1Args Value Builtin m ->
  TensorOp1Args (Value Builtin) ->
  m (Value Builtin)
unblockTensorOp1 unblock evalFn (TensorOp1Args ds xs) = do
  xs' <- unblock xs
  liftIf xs' $ \xs'' -> do
    evalFn (TensorOp1Args ds xs'')

unblockTensorOp2 ::
  (MonadUnblock m) =>
  UnblockingFunction m ->
  EvalSimple TensorOp2Args Value Builtin m ->
  TensorOp2Args (Value Builtin) ->
  m (Value Builtin)
unblockTensorOp2 unblock evalFn (TensorOp2Args ds xs ys) = do
  xs' <- unblock xs
  ys' <- unblock ys
  liftIf xs' $ \xs'' ->
    liftIf ys' $ \ys'' -> do
      evalFn $ TensorOp2Args ds xs'' ys''

unblockReduceTensor ::
  (MonadUnblock m) =>
  UnblockingFunction m ->
  EvalSimple TensorReductionArgs Value Builtin m ->
  TensorReductionArgs (Value Builtin) ->
  m (Value Builtin)
unblockReduceTensor unblock evalFn (TensorOp2Args ds e xs) = do
  xs' <- unblock xs
  liftIf xs' $ \xs'' ->
    evalFn $ TensorOp2Args ds e xs''

unblockConstTensor ::
  (MonadUnblock m) =>
  ConstTensorArgs (Value Builtin) ->
  m (Value Builtin)
unblockConstTensor (ConstTensorArgs tElem value dims) = do
  dims' <- unblockDimensionsValue dims
  liftIf dims' $ \dims'' -> do
    evalConstTensor $ ConstTensorArgs tElem value dims''

unblockStackTensor ::
  (MonadUnblock m) =>
  UnblockingFunction m ->
  StackTensorArgs (Value Builtin) ->
  m (Value Builtin)
unblockStackTensor unblock (StackTensorArgs tElem d ds xss) = do
  d' <- unblockNatValue d
  xss' <- traverse unblock xss
  liftIf d' $ \d'' ->
    liftIfValues xss' $ \xss'' ->
      evalStackTensor $ StackTensorArgs tElem d'' ds xss''

unblockAtTensor ::
  (MonadUnblock m) =>
  UnblockingFunction m ->
  AtTensorArgs (Value Builtin) ->
  m (Value Builtin)
unblockAtTensor unblock (AtTensorArgs tElem d ds xs i) = do
  xs' <- unblock xs
  i' <- unblockIndexValue i
  liftIf xs' $ \xs'' ->
    liftIf i' $ \i'' -> do
      evalAtTensor $ AtTensorArgs tElem d ds xs'' i''

unblockForeachTensor ::
  (MonadUnblock m) =>
  ForeachTensorArgs (Value Builtin) ->
  m (Value Builtin)
unblockForeachTensor (ForeachTensorArgs tElem d ds fn) = do
  d' <- unblockNatValue d
  liftIf d' $ \d'' -> do
    freeEnv <- getFreeEnv
    evalForeachTensor (evalApp freeEnv) $ ForeachTensorArgs tElem d'' ds fn

--------------------------------------------------------------------------------
-- Unblocking operations

currentPass :: Doc a
currentPass = "unblocking"

showEntry :: forall m. (MonadUnblock m) => Value Builtin -> m ()
showEntry e = do
  ctx <- getNameContext
  -- logDebug MaxDetail $ "unblock-entry" <+> prettyVerbose e
  logDebug MaxDetail $ "unblock-entry" <+> prettyFriendly (WithContext e ctx)
  incrCallDepth

showExit :: forall m. (MonadUnblock m) => Value Builtin -> m (Value Builtin)
showExit e = do
  ctx <- getNameContext
  decrCallDepth
  -- logDebug MaxDetail $ "unblock-exit " <+> prettyVerbose e
  logDebug MaxDetail $ "unblock-exit " <+> prettyFriendly (WithContext e ctx)
  return e
