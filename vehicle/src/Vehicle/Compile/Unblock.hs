module Vehicle.Compile.Unblock
  ( unblockBoolExpr,
    UnblockingActions (..),
    OperationUnblockingFunction,
    TypeUnblockingFunction,
    unblockRatTensorValue,
    unblockRecordValue,
    unblockIf,
    unblockAtTensor,
    unblockAtVector,
    unblockTranspose,
    unblockForeachTensor,
    unblockReduceTensor,
    unblockMinRatTensor,
    unblockMaxRatTensor,
    unblockTensorOp2,
    unblockTensorOp1,
    unblockRecordAcc,
  )
where

import Vehicle.Compile.LiftIf (unfoldIf)
import Vehicle.Compile.Normalise.NBE (eval, evalApp, evalRecordAcc)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Data.Builtin.Interface.Normalise
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.BooleanExpr (IfTree (..), elimIfTree, forIfTreeM)
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Value
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Data.Variable.Free.Context.Class

--------------------------------------------------------------------------------
-- Unblocking
--------------------------------------------------------------------------------

type MonadUnblock m =
  ( MonadLogger m,
    MonadFreeContext Builtin m,
    MonadReadableNameContext m
  )

type MonadPurify m = MonadUnblock m

data UnblockingActions m = UnblockingActions
  { unblockRatTensorBoundVar ::
      Lv ->
      m (Value Builtin),
    unblockNetworkApp ::
      TypeUnblockingFunction (Value Builtin) m ->
      TypeUnblockingFunction (Value Builtin) m ->
      Identifier ->
      OperationUnblockingFunction NetworkAppArgs (Value Builtin) m,
    unblockDatasetOrParameter ::
      Identifier ->
      m (Value Builtin),
    unblockRecordBoundVar ::
      Lv ->
      m (Value Builtin)
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
  exprDoc <- prettyFriendlyInCtx expr
  logCompilerSection MaxDetail ("unblocking" <+> exprDoc) $ do
    ifTree <- unblockBoolTensorValue actions expr
    let elimIf c x y = unfoldIf $ IfArgs IBoolType c x y
    elimIfTree elimIf return ifTree

--------------------------------------------------------------------------------
-- Main unblocking functions

type TypeUnblockingFunction a m = (MonadUnblock m) => Value Builtin -> m (IfTree (Value Builtin) a)

unblockBoolTensorValue :: UnblockingActions m -> TypeUnblockingFunction (Value Builtin) m
unblockBoolTensorValue actions expr = showEntry expr $
  case toBoolTensorValue expr of
    -- Already unblocked
    VBoolTensorLiteral {} -> return $ IfLeaf expr
    VBoolStackTensor {} -> return $ IfLeaf expr
    VBoolConstTensor {} -> return $ IfLeaf expr
    VBoolTensorQuantifyRat {} -> return $ IfLeaf expr
    VBoolTensorQuantifyRecord {} -> return $ IfLeaf expr
    VBoolTensorAnd args -> unblockTensorOp2 unblock evalAnd args
    VBoolTensorOr args -> unblockTensorOp2 unblock evalOr args
    VBoolTensorNot args -> unblockTensorOp1 unblock evalNot args
    VBoolTensorCompareRatReduced {} -> return $ IfLeaf expr
    VBoolTensorCompareRatPointwise (op, args) -> unblockTensorOp2 (unblockRatTensorValue actions) (evalCompareRatPointwise op) args
    -- Recursively unblock
    VBoolTensorIf args -> unblockIf unblock args
    VBoolTensorReduceAnd args -> unblockReduceTensor unblock unoptimisedEvalReduceAndTensor args
    VBoolTensorReduceOr args -> unblockReduceTensor unblock evalReduceOrTensor args
    VBoolTensorCompareIndex (op, args) -> unblockIndexOp2 (evalCompareIndex op) args
    VBoolTensorCompareNat (op, args) -> unblockOp2 unblockNatValue (evalCompareNat op) args
    VBoolTensorAt args -> unblockAtTensor unblock args
    VBoolTensorForeach args -> unblockForeachTensor args
  where
    unblock = unblockBoolTensorValue actions

unblockRatTensorValue ::
  (MonadPurify m) =>
  UnblockingActions m ->
  TypeUnblockingFunction (Value Builtin) m
unblockRatTensorValue actions@UnblockingActions {..} expr = showEntry expr $ do
  case toRatTensorValue expr of
    -- Rational operators
    VRatTensorLiteral {} -> return $ IfLeaf expr
    VRatConstTensor {} -> return $ IfLeaf expr
    VRatStackTensor {} -> return $ IfLeaf expr
    -- Recursively purify
    VIfRatTensor args -> unblockIf unblock args
    VNegRatTensor args -> unblockTensorOp1 unblock evalNegRatTensor args
    VLogRatTensor args -> unblockTensorOp1 unblock evalLogRatTensor args
    VExpRatTensor args -> unblockTensorOp1 unblock evalExpRatTensor args
    VAddRatTensor args -> unblockTensorOp2 unblock evalAddRatTensor args
    VSubRatTensor args -> unblockTensorOp2 unblock evalSubRatTensor args
    VMulRatTensor args -> unblockTensorOp2 unblock evalMulRatTensor args
    VDivRatTensor args -> unblockTensorOp2 unblock evalDivRatTensor args
    VPowRatTensor args -> unblockTensorOp2 unblock evalPowRatTensor args
    VReduceAddRatTensor args -> unblockReduceTensor unblock evalReduceAddRatTensor args
    VReduceMulRatTensor args -> unblockReduceTensor unblock evalReduceMulRatTensor args
    VReduceMinRatTensor args -> unblockReduceTensor unblock evalReduceMinRatTensor args
    VReduceMaxRatTensor args -> unblockReduceTensor unblock evalReduceMaxRatTensor args
    VMinRatTensor args -> unblockMinRatTensor unblock args
    VMaxRatTensor args -> unblockMaxRatTensor unblock args
    VRatTensorBoundVar v -> unblock =<< unblockRatTensorBoundVar v
    VRatTensorNetworkApp n args -> unblockNetworkApp unblock (unblockRecordValue actions) n args
    VDatasetOrParameter ident -> unblock =<< unblockDatasetOrParameter ident
    VRatAtTensor args -> unblockAtTensor unblock args
    VRatAtVector args -> unblockAtVector unblock args
    VRatForeach args -> unblockForeachTensor args
    VRatTensorTranspose args -> unblockTranspose unblock args
    VRatRecordAcc typ value fieldName _ -> unblockRecordAcc (unblockRecordValue actions) typ value fieldName
  where
    unblock = unblockRatTensorValue actions

unblockRecordValue ::
  UnblockingActions m ->
  TypeUnblockingFunction (Value Builtin) m
unblockRecordValue actions@UnblockingActions {..} expr = showEntry expr $ do
  case toRecordValue expr of
    VRecordLiteral {} -> return $ IfLeaf expr
    VRecordNetworkApp n args -> unblockNetworkApp unblockTensor unblockRecord n args
    VRecordBoundVar v -> unblockRecord =<< unblockRecordBoundVar v
  where
    unblockTensor = unblockRatTensorValue actions
    unblockRecord = unblockRecordValue actions

unblockIndexValue :: TypeUnblockingFunction (Value Builtin) m
unblockIndexValue expr = showEntry expr $ case toIndexValue expr of
  VIndexLiteral {} -> return $ IfLeaf expr
  VIndexIf args -> unblockIf unblockIndexValue args
  VIndexAtVector args -> unblockAtVector unblockVectorValue args
  VIndexBoundVar {} -> unexpectedExprError currentPass (prettyVerbose expr)
  VIndexParameter {} -> unexpectedExprError currentPass (prettyVerbose expr)

unblockNatValue :: TypeUnblockingFunction (Value Builtin) m
unblockNatValue expr = showEntry expr $ case toNatValue expr of
  VNatLiteral {} -> return $ IfLeaf expr
  VNatIf ifArgs -> unblockIf unblockNatValue ifArgs
  VNatAdd args -> unblockOp2 unblockNatValue evalAddNat args
  VNatMul args -> unblockOp2 unblockNatValue evalMulNat args
  VNatBoundVar {} -> unexpectedExprError currentPass (prettyVerbose expr)
  VNatParameter {} -> unexpectedExprError currentPass (prettyVerbose expr)

unblockVectorValue :: TypeUnblockingFunction (Value Builtin) m
unblockVectorValue expr = showEntry expr $ case toVectorValue expr of
  VVectorLiteral {} -> return $ IfLeaf expr
  VVectorIf args -> unblockIf unblockVectorValue args
  VVectorForeach args -> unblockForeachVector args
  VVectorBoundVar {} -> unexpectedExprError currentPass (prettyVerbose expr)
  VVectorDataset {} -> unexpectedExprError currentPass (prettyVerbose expr)

--------------------------------------------------------------------------------
-- Unblocking individual operations

type OperationUnblockingFunction args a m =
  (MonadUnblock m) => args (Value Builtin) -> m (IfTree (Value Builtin) a)

unblockIf ::
  TypeUnblockingFunction a m ->
  OperationUnblockingFunction IfArgs a m
unblockIf unblock (IfArgs _ c x y) =
  IfTree c <$> unblock x <*> unblock y

unblockOp2 ::
  (MonadUnblock m) =>
  TypeUnblockingFunction (Value Builtin) m ->
  EvalSimple Op2Args Value Builtin m ->
  OperationUnblockingFunction Op2Args (Value Builtin) m
unblockOp2 unblock evalFn (Op2Args x y) = do
  x' <- unblock x
  y' <- unblock y
  forIfTreeM x' $ \x'' ->
    forIfTreeM y' $ \y'' ->
      IfLeaf <$> do
        evalFn $ Op2Args x'' y''

unblockIndexOp2 ::
  (MonadUnblock m) =>
  EvalSimple IndexComparisonArgs Value Builtin m ->
  OperationUnblockingFunction IndexComparisonArgs (Value Builtin) m
unblockIndexOp2 evalFn (IndexComparisonArgs n1 n2 x y) = do
  x' <- unblockIndexValue x
  y' <- unblockIndexValue y
  forIfTreeM x' $ \x'' ->
    forIfTreeM y' $ \y'' ->
      IfLeaf <$> do
        evalFn $ IndexComparisonArgs n1 n2 x'' y''

unblockTensorOp1 ::
  (MonadUnblock m) =>
  TypeUnblockingFunction (Value Builtin) m ->
  EvalSimple TensorOp1Args Value Builtin m ->
  OperationUnblockingFunction TensorOp1Args (Value Builtin) m
unblockTensorOp1 unblock evalFn (TensorOp1Args ds xs) = do
  xs' <- unblock xs
  forIfTreeM xs' $ \xs'' -> IfLeaf <$> evalFn (TensorOp1Args ds xs'')

unblockTensorOp2 ::
  (MonadUnblock m) =>
  TypeUnblockingFunction (Value Builtin) m ->
  EvalSimple TensorOp2Args Value Builtin m ->
  OperationUnblockingFunction TensorOp2Args (Value Builtin) m
unblockTensorOp2 unblock evalFn (TensorOp2Args ds xs ys) = do
  xs' <- unblock xs
  ys' <- unblock ys
  forIfTreeM xs' $ \xs'' ->
    forIfTreeM ys' $ \ys'' -> IfLeaf <$> evalFn (TensorOp2Args ds xs'' ys'')

unblockReduceTensor ::
  (MonadUnblock m) =>
  TypeUnblockingFunction (Value Builtin) m ->
  (TensorReductionArgs (Value Builtin) -> m a) ->
  OperationUnblockingFunction TensorReductionArgs a m
unblockReduceTensor unblock evalFn (TensorReductionArgs ds xs) = do
  xs' <- unblock xs
  forIfTreeM xs' $ \xs'' ->
    IfLeaf <$> do
      evalFn $ TensorReductionArgs ds xs''

unblockAtTensor ::
  (MonadUnblock m) =>
  TypeUnblockingFunction (Value Builtin) m ->
  OperationUnblockingFunction AtTensorArgs (Value Builtin) m
unblockAtTensor unblock (AtTensorArgs tElem d ds xs i) = do
  xs' <- unblock xs
  i' <- unblockIndexValue i
  forIfTreeM xs' $ \xs'' ->
    forIfTreeM i' $ \i'' ->
      IfLeaf <$> do
        nameCtx <- getNameContext
        evalAtTensor nameCtx evalApp eval $ AtTensorArgs tElem d ds xs'' i''

unblockAtVector ::
  (MonadUnblock m) =>
  TypeUnblockingFunction (Value Builtin) m ->
  OperationUnblockingFunction AtVectorArgs (Value Builtin) m
unblockAtVector unblock (AtVectorArgs tElem d xs i) = do
  xs' <- unblock xs
  i' <- unblockIndexValue i
  forIfTreeM xs' $ \xs'' ->
    forIfTreeM i' $ \i'' ->
      IfLeaf <$> do
        evalAtVector $ AtVectorArgs tElem d xs'' i''

unblockRecordAcc ::
  (MonadUnblock m) =>
  TypeUnblockingFunction (Value Builtin) m ->
  VType Builtin ->
  Value Builtin ->
  FieldName ->
  m (IfTree (Value Builtin) (Value Builtin))
unblockRecordAcc unblock typ value fieldName = do
  value' <- unblock value
  forIfTreeM value' $ \value'' ->
    IfLeaf <$> do
      evalRecordAcc typ value'' fieldName

unblockTranspose ::
  (MonadUnblock m) =>
  TypeUnblockingFunction (Value Builtin) m ->
  OperationUnblockingFunction TransposeArgs (Value Builtin) m
unblockTranspose unblock (TransposeArgs t ds xs) = do
  xs' <- unblock xs
  forIfTreeM xs' $ \xs'' ->
    IfLeaf <$> evalTranspose (TransposeArgs t ds xs'')

unblockForeachTensor ::
  (MonadUnblock m) =>
  OperationUnblockingFunction ForeachTensorArgs (Value Builtin) m
unblockForeachTensor (ForeachTensorArgs tElem d ds fn) = do
  d' <- unblockNatValue d
  forIfTreeM d' $ \d'' ->
    IfLeaf <$> do
      nameCtx <- getNameContext
      unoptimisedEvalForeachTensor nameCtx evalApp $ ForeachTensorArgs tElem d'' ds fn

unblockRatTensorExtrema ::
  ComparisonOp ->
  TypeUnblockingFunction (Value Builtin) m ->
  OperationUnblockingFunction TensorOp2Args (Value Builtin) m
unblockRatTensorExtrema op unblock (TensorOp2Args ds x y) = do
  x' <- unblock x
  y' <- unblock y
  forIfTreeM x' $ \x'' ->
    forIfTreeM y' $ \y'' -> do
      let cArgs = TensorOp2Args ds x'' y''
      let c = fromBoolValue $ VCompareRatTensor (op, cArgs)
      return $ IfTree c (IfLeaf x'') (IfLeaf y'')

unblockMinRatTensor ::
  TypeUnblockingFunction (Value Builtin) m ->
  OperationUnblockingFunction TensorOp2Args (Value Builtin) m
unblockMinRatTensor = unblockRatTensorExtrema Le

unblockMaxRatTensor ::
  TypeUnblockingFunction (Value Builtin) m ->
  OperationUnblockingFunction TensorOp2Args (Value Builtin) m
unblockMaxRatTensor = unblockRatTensorExtrema Ge

unblockForeachVector ::
  (MonadUnblock m) =>
  OperationUnblockingFunction ForeachVectorArgs (Value Builtin) m
unblockForeachVector (ForeachVectorArgs tElem d fn) = do
  d' <- unblockNatValue d
  forIfTreeM d' $ \d'' ->
    IfLeaf <$> do
      nameCtx <- getNameContext
      evalForeachVector nameCtx evalApp eval $ ForeachVectorArgs tElem d'' fn

--------------------------------------------------------------------------------
-- Unblocking operations

currentPass :: Doc a
currentPass = "unblocking"

showEntry :: forall m. (MonadUnblock m) => Value Builtin -> m (IfTree (Value Builtin) (Value Builtin)) -> m (IfTree (Value Builtin) (Value Builtin))
showEntry input resultFn = do
  logDebugM MaxDetail $ do
    ctx <- getNameContext
    let doc = prettyFriendly (WithContext input ctx)
    return $ "unblock-entry:" <+> doc
  incrCallDepth

  result <- resultFn
  decrCallDepth
  logDebugM MaxDetail $ do
    ctx <- getNameContext
    -- let doc = prettyVerbose result
    let doc = prettyFriendly (WithContext result ctx)
    return $ "unblock-exit:" <+> doc

  return result
