module Vehicle.Compile.Unblock
  ( unblockBoolExpr,
    UnblockingActions (..),
    OperationUnblockingFunction,
    TypeUnblockingFunction,
    unblockRatTensorValue,
    unblockIndexValue,
    unblockRecordValue,
    unblockVectorValue,
    unblockIf,
    unblockAtTensor,
    unblockAtVector,
    unblockTransposeTensor,
    unblockForeachTensor,
    unblockReduceTensor,
    unblockMinRatTensor,
    unblockMaxRatTensor,
    unblockConstTensor,
    unblockTensorOp2,
    unblockTensorOp1,
    unblockRecordAcc,
    noUnblocking,
    forceEval,
  )
where

import Control.Monad.Except (MonadError (..))
import GHC.Stack (HasCallStack)
import Vehicle.Compile.Error (BlockingReason (..))
import Vehicle.Compile.LiftIf (unfoldIf)
import Vehicle.Compile.Normalise.Builtin
import Vehicle.Compile.Normalise.Core
import Vehicle.Compile.Normalise.Force
import Vehicle.Compile.Normalise.RewriteRules (forceAndRewriteTensor)
import Vehicle.Compile.Normalise.TypedValue
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.BooleanExpr (IfTree (..), elimIfTree, forIfTreeM)
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Code.Interface
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Data.Variable.Free.Context.Class

--------------------------------------------------------------------------------
-- Unblocking
--------------------------------------------------------------------------------

type MonadUnblock m =
  ( MonadLogger m,
    MonadFreeContext Builtin m,
    MonadNameContext m
  )

type MonadPurify m = MonadUnblock m

data UnblockingActions m = UnblockingActions
  { -- | How to handle a bound variable. The bound variable can be of any type.
    unblockBoundVar ::
      TypeUnblockingFunction (Thunk Builtin) m ->
      Lv ->
      UnforcedSpine Builtin ->
      m (IfTree (Thunk Builtin) (Thunk Builtin)),
    -- | How to handle a network application.
    unblockNetworkApp ::
      TypeUnblockingFunction (Thunk Builtin) m ->
      TypeUnblockingFunction (Thunk Builtin) m ->
      Identifier ->
      OperationUnblockingFunction NetworkAppArgs (Thunk Builtin) m,
    -- | How to handle a dataset or parameter. The dataset or parameter can be any type.
    unblockDatasetOrParameter ::
      TypeUnblockingFunction (Thunk Builtin) m ->
      Identifier ->
      m (IfTree (Thunk Builtin) (Thunk Builtin))
  }

noUnblocking :: (MonadError BlockingReason m) => UnblockingActions m
noUnblocking =
  UnblockingActions
    { unblockBoundVar = \_ v _ -> return $ IfLeaf $ Forced $ VBoundVar v [],
      unblockNetworkApp = \_ _ ident _args -> throwError $ BlockingNetwork ident,
      unblockDatasetOrParameter = \_ ident -> throwError $ BlockingDatasetOrParameter ident
    }

-- | Lifts all `if`s in the provided expression `e` to the top-level, while
-- preserving the guarantee that the expression is normalised as much as
-- possible.
unblockBoolExpr ::
  (MonadUnblock m) =>
  UnblockingActions m ->
  Thunk Builtin ->
  m (Thunk Builtin)
unblockBoolExpr actions expr = do
  exprDoc <- prettyFriendlyInCtx expr
  logCompilerSection MaxDetail ("unblocking" <+> exprDoc) $ do
    ifTree <- unblockBoolTensorValue actions expr
    let elimIf c x y = unfoldIf $ IfArgs (Forced IBoolType) c x y
    elimIfTree elimIf return ifTree

--------------------------------------------------------------------------------
-- Main unblocking functions

type TypeUnblockingFunction a m =
  (MonadUnblock m) =>
  Thunk Builtin ->
  m (IfTree (Thunk Builtin) a)

unblockBoolTensorValue :: UnblockingActions m -> TypeUnblockingFunction (Thunk Builtin) m
unblockBoolTensorValue actions value = showEntry value $ do
  forcedValue <- forceAndRewriteTensor value
  case toBoolTensorValue forcedValue of
    -- Already unblocked
    VBoolTensorLiteral {} -> return $ IfLeaf $ Forced forcedValue
    VBoolStackTensor {} -> return $ IfLeaf $ Forced forcedValue
    VBoolTensorQuantifyRat {} -> return $ IfLeaf $ Forced forcedValue
    VBoolTensorQuantifyRecord {} -> return $ IfLeaf $ Forced forcedValue
    -- Recursively unblock
    VBoolConstTensor args -> unblockConstTensor unblock actions args
    VBoolTensorCompareRatTensor (op, args) -> unblockCompareRatTensor actions op args
    VBoolTensorAnd args -> unblockTensorOp2 unblock (forceEval evalAnd) args
    VBoolTensorOr args -> unblockTensorOp2 unblock (forceEval evalOr) args
    VBoolTensorNot args -> unblockTensorOp1 unblock (forceEval evalNot) args
    VBoolTensorImplies args -> unblock $ elimImplies args
    VBoolTensorIf args -> unblockIf unblock args
    VBoolTensorReduceAnd args -> unblockReduceTensor unblock (forceEval evalReduceAndTensor) args
    VBoolTensorReduceOr args -> unblockReduceTensor unblock (forceEval evalReduceOrTensor) args
    VBoolTensorCompareIndex (op, args) -> unblockIndexOp2 (unblockIndexValue actions) (evalCompareIndex op) args
    VBoolTensorCompareNat (op, args) -> unblockOp2 (unblockNatValue actions) (evalCompareNat op) args
    VBoolTensorTensorAt args -> unblockAtTensor (return . IfLeaf) unblock (unblockIndexValue actions) args
    VBoolTensorVectorAt args -> unblockAtVector unblock (unblockIndexValue actions) args
    VBoolTensorForeach args -> unblockForeachTensor actions args
    VBoolTensorFoldList args -> unblockFoldList actions args
  where
    unblock = unblockBoolTensorValue actions

unblockRatTensorValue ::
  (MonadPurify m) =>
  UnblockingActions m ->
  TypeUnblockingFunction (Thunk Builtin) m
unblockRatTensorValue actions@UnblockingActions {..} expr =
  showEntry expr $ do
    forcedValue <- forceThunk expr
    case toRatTensorValue forcedValue of
      -- Rational operators
      VRatTensorLiteral {} -> return $ IfLeaf expr
      VRatConstTensor {} -> return $ IfLeaf expr
      VRatStackTensor {} -> return $ IfLeaf expr
      -- Recursively purify
      VIfRatTensor args -> unblockIf unblock args
      VNegRatTensor args -> unblockTensorOp1 unblock (forceEval evalNegRatTensor) args
      VLogRatTensor args -> unblockTensorOp1 unblock (forceEval evalLogRatTensor) args
      VExpRatTensor args -> unblockTensorOp1 unblock (forceEval evalExpRatTensor) args
      VAddRatTensor args -> unblockTensorOp2 unblock (forceEval evalAddRatTensor) args
      VSubRatTensor args -> unblockTensorOp2 unblock (forceEval evalSubRatTensor) args
      VMulRatTensor args -> unblockTensorOp2 unblock (forceEval evalMulRatTensor) args
      VDivRatTensor args -> unblockTensorOp2 unblock (forceEval evalDivRatTensor) args
      VPowRatTensor args -> unblockTensorOp2 unblock (forceEval evalPowRatTensor) args
      VReduceAddRatTensor args -> unblockReduceTensor unblock (forceEval evalReduceAddRatTensor) args
      VReduceMulRatTensor args -> unblockReduceTensor unblock (forceEval evalReduceMulRatTensor) args
      VReduceMinRatTensor args -> unblockReduceTensor unblock (forceEval evalReduceMinRatTensor) args
      VReduceMaxRatTensor args -> unblockReduceTensor unblock (forceEval evalReduceMaxRatTensor) args
      VMinRatTensor args -> unblockMinRatTensor unblock args
      VMaxRatTensor args -> unblockMaxRatTensor unblock args
      VRatTensorBoundVar v spine -> unblockBoundVar unblock v spine
      VNetworkApplication n args -> unblockNetworkApp unblock (unblockRecordValue actions) n args
      VParameterOrDataset ident -> unblockDatasetOrParameter unblock ident
      VRatAtTensor args -> unblockAtTensor (return . IfLeaf) unblock (unblockIndexValue actions) args
      VRatAtVector args -> unblockAtVector (unblockVectorValue actions) (unblockIndexValue actions) args
      VRatForeach args -> unblockForeachTensor actions args
      VRatTensorTranspose args -> unblockTransposeTensor unblock args
      VRatTensorRecordAcc typ value fieldName args -> unblockRecordAcc actions typ value fieldName args
  where
    unblock = unblockRatTensorValue actions

unblockRecordValue ::
  UnblockingActions m ->
  TypeUnblockingFunction (Thunk Builtin) m
unblockRecordValue actions@UnblockingActions {..} expr = showEntry expr $ do
  forcedValue <- forceThunk expr
  case toRecordValue forcedValue of
    VRecordRecord {} -> return $ IfLeaf expr
    VRecordBoundVar v spine -> unblockBoundVar unblockRecord v spine
    VRecordNetworkApp n args -> unblockNetworkApp (unblockRatTensorValue actions) unblockRecord n args
    VRecordMeta {} -> unexpectedExprError currentPass "record meta"
    VRecordBuiltin b spine -> case VBuiltin b spine of
      (getExpr accessIf -> Just args) -> unblockIf unblockRecord args
      _ -> unexpectedExprError currentPass (pretty b <+> "record")
    VRecordRecordAcc typ record field spine -> unblockRecordAcc actions typ record field spine
  where
    unblockRecord = unblockRecordValue actions

unblockIndexValue ::
  UnblockingActions m ->
  TypeUnblockingFunction (Thunk Builtin) m
unblockIndexValue actions value = showEntry value $ do
  forcedValue <- forceThunk value
  case toIndexValue forcedValue of
    VIndexLiteral {} -> return $ IfLeaf value
    VIndexParameter ident -> unblockDatasetOrParameter actions (unblockIndexValue actions) ident
    VIndexIf args -> unblockIf (unblockIndexValue actions) args
    VIndexAtVector args -> unblockAtVector (unblockVectorValue actions) (unblockIndexValue actions) args
    VIndexRecordAcc typ record field spine -> unblockRecordAcc actions typ record field spine
    VIndexBoundVar v spine -> unblockBoundVar actions (unblockIndexValue actions) v spine

unblockNatValue ::
  UnblockingActions m ->
  TypeUnblockingFunction (Thunk Builtin) m
unblockNatValue actions value = showEntry value $ do
  forcedValue <- forceThunk value
  case toNatValue forcedValue of
    VNatLiteral {} -> return $ IfLeaf value
    VNatIf ifArgs -> unblockIf (unblockNatValue actions) ifArgs
    VNatAdd args -> unblockOp2 (unblockNatValue actions) evalAddNat args
    VNatMul args -> unblockOp2 (unblockNatValue actions) evalMulNat args
    VNatBoundVar v spine -> unblockBoundVar actions (unblockNatValue actions) v spine
    VNatParameter ident -> unblockDatasetOrParameter actions (unblockNatValue actions) ident

unblockVectorValue ::
  UnblockingActions m ->
  TypeUnblockingFunction (Thunk Builtin) m
unblockVectorValue actions value = showEntry value $ do
  forcedValue <- forceThunk value
  case toVectorValue forcedValue of
    VVectorLiteral {} -> return $ IfLeaf $ Forced forcedValue
    VVectorIf args -> unblockIf (unblockVectorValue actions) args
    VVectorAt args -> unblockAtVector (unblockVectorValue actions) (unblockVectorValue actions) args
    VVectorForeach args -> unblockForeachVector actions args
    VVectorBoundVar v spine -> unblockBoundVar actions (unblockVectorValue actions) v spine
    VVectorDataset ident -> unblockDatasetOrParameter actions (unblockVectorValue actions) ident
    VVectorRecordAcc typ record field spine -> unblockRecordAcc actions typ record field spine

unblockListValue ::
  UnblockingActions m ->
  TypeUnblockingFunction (Thunk Builtin) m
unblockListValue actions value = showEntry value $ do
  forcedValue <- forceThunk value
  case toListValue forcedValue of
    VListNil {} -> return $ IfLeaf value
    VListCons {} -> return $ IfLeaf value
    VListMap args -> unblockMapList actions args
    VListIf args -> unblockIf (unblockListValue actions) args
    VListBoundVar v spine -> unblockBoundVar actions (unblockListValue actions) v spine
    VListDataset ident -> unblockDatasetOrParameter actions (unblockListValue actions) ident
    VListRecordAcc typ record field spine -> unblockRecordAcc actions typ record field spine

--------------------------------------------------------------------------------
-- Unblocking individual operations

type OperationUnblockingFunction args a m =
  (MonadUnblock m) => args (Thunk Builtin) -> m (IfTree (Thunk Builtin) a)

unblockIf ::
  TypeUnblockingFunction a m ->
  OperationUnblockingFunction IfArgs a m
unblockIf unblock (IfArgs _ c x y) = do
  IfTree c <$> unblock x <*> unblock y

unblockOp2 ::
  (MonadUnblock m) =>
  TypeUnblockingFunction (Thunk Builtin) m ->
  EvalSimple ForcedValue Thunk Op2Args Builtin m ->
  OperationUnblockingFunction Op2Args (Thunk Builtin) m
unblockOp2 unblock evalFn (Op2Args x y) = do
  x' <- unblock x
  y' <- unblock y
  forIfTreeM x' $ \x'' ->
    forIfTreeM y' $ \y'' ->
      IfLeaf <$> do
        forceEval evalFn $ Op2Args x'' y''

unblockIndexOp2 ::
  (MonadUnblock m) =>
  TypeUnblockingFunction (Thunk Builtin) m ->
  EvalSimple ForcedValue Thunk IndexComparisonArgs Builtin m ->
  OperationUnblockingFunction IndexComparisonArgs (Thunk Builtin) m
unblockIndexOp2 unblock evalFn (IndexComparisonArgs n1 n2 x y) = do
  x' <- unblock x
  y' <- unblock y
  forIfTreeM x' $ \x'' ->
    forIfTreeM y' $ \y'' ->
      IfLeaf <$> do
        forceEval evalFn $ IndexComparisonArgs n1 n2 x'' y''

unblockTensorOp1 ::
  (MonadUnblock m) =>
  TypeUnblockingFunction (Thunk Builtin) m ->
  (TensorOp1Args (Thunk Builtin) -> m (Thunk Builtin)) ->
  OperationUnblockingFunction TensorOp1Args (Thunk Builtin) m
unblockTensorOp1 unblock evalFn (TensorOp1Args ds xs) = do
  xs' <- unblock xs
  forIfTreeM xs' $ \xs'' ->
    IfLeaf
      <$> evalFn (TensorOp1Args ds xs'')

unblockTensorOp2 ::
  (MonadUnblock m) =>
  TypeUnblockingFunction (Thunk Builtin) m ->
  (TensorOp2Args (Thunk Builtin) -> m (Thunk Builtin)) ->
  OperationUnblockingFunction TensorOp2Args (Thunk Builtin) m
unblockTensorOp2 unblock evalFn (TensorOp2Args ds xs ys) = do
  xs' <- unblock xs
  ys' <- unblock ys
  forIfTreeM xs' $ \xs'' ->
    forIfTreeM ys' $ \ys'' -> do
      IfLeaf
        <$> evalFn (TensorOp2Args ds xs'' ys'')

unblockCompareRatTensor ::
  (MonadUnblock m) =>
  UnblockingActions m ->
  ComparisonOp ->
  OperationUnblockingFunction TensorComparisonArgs (Thunk Builtin) m
unblockCompareRatTensor actions op (TensorComparisonArgs pDims rDims xs ys) = do
  xs' <- unblockRatTensorValue actions xs
  ys' <- unblockRatTensorValue actions ys
  forIfTreeM xs' $ \xs'' ->
    forIfTreeM ys' $ \ys'' -> do
      IfLeaf
        <$> forceEval (evalCompareRatTensor op) (TensorComparisonArgs pDims rDims xs'' ys'')

unblockTransposeTensor ::
  (MonadUnblock m) =>
  TypeUnblockingFunction (Thunk Builtin) m ->
  OperationUnblockingFunction TransposeTensorArgs (Thunk Builtin) m
unblockTransposeTensor unblock (TransposeTensorArgs t ds xs) = do
  xs' <- unblock xs
  forIfTreeM xs' $ \xs'' ->
    IfLeaf <$> forceEvaluation accessTransposeTensor evalTransposeTensor (TransposeTensorArgs t ds xs'')

unblockReduceTensor ::
  (MonadUnblock m) =>
  TypeUnblockingFunction (Thunk Builtin) m ->
  (TensorReductionArgs (Thunk Builtin) -> m (Thunk Builtin)) ->
  OperationUnblockingFunction TensorReductionArgs (Thunk Builtin) m
unblockReduceTensor unblockArg evalFn (TensorReductionArgs ds xs) = do
  xs' <- unblockArg xs
  forIfTreeM xs' $ \xs'' ->
    IfLeaf <$> do
      evalFn $ TensorReductionArgs ds xs''

unblockAtTensor ::
  (MonadUnblock m) =>
  TypeUnblockingFunction (Thunk Builtin) m ->
  TypeUnblockingFunction (Thunk Builtin) m ->
  TypeUnblockingFunction (Thunk Builtin) m ->
  OperationUnblockingFunction AtTensorArgs (Thunk Builtin) m
unblockAtTensor unblock unblockTensor unblockIndex (AtTensorArgs tElem d ds xs i) = do
  xs' <- unblockTensor xs
  i' <- unblockIndex i
  forIfTreeM xs' $ \xs'' ->
    forIfTreeM i' $ \i'' ->
      unblock
        =<< forceEval evalAtTensor (AtTensorArgs tElem d ds xs'' i'')

unblockAtVector ::
  (MonadUnblock m) =>
  TypeUnblockingFunction (Thunk Builtin) m ->
  TypeUnblockingFunction (Thunk Builtin) m ->
  OperationUnblockingFunction AtVectorArgs (Thunk Builtin) m
unblockAtVector unblockVector unblockIndex (AtVectorArgs tElem d xs i) = do
  xs' <- unblockVector xs
  i' <- unblockIndex i
  forIfTreeM xs' $ \xs'' ->
    forIfTreeM i' $ \i'' ->
      IfLeaf <$> do
        forceEval evalAtVector $ AtVectorArgs tElem d xs'' i''

unblockRecordAcc ::
  (MonadUnblock m) =>
  UnblockingActions m ->
  UnforcedType Builtin ->
  Thunk Builtin ->
  FieldName ->
  UnforcedSpine Builtin ->
  m (IfTree (Thunk Builtin) (Thunk Builtin))
unblockRecordAcc actions typ value fieldName args = do
  value' <- unblockRecordValue actions value
  forIfTreeM value' $ \value'' ->
    IfLeaf <$> do
      Forced <$> forceRecordAcc typ value'' fieldName args

unblockForeachTensor ::
  (MonadUnblock m) =>
  UnblockingActions m ->
  OperationUnblockingFunction ForeachTensorArgs (Thunk Builtin) m
unblockForeachTensor actions (ForeachTensorArgs tElem d ds fn) = do
  d' <- unblockNatValue actions d
  forIfTreeM d' $ \d'' ->
    IfLeaf <$> do
      let result = forceEval evalForeachTensor
      result $ ForeachTensorArgs tElem d'' ds fn

unblockRatTensorExtrema ::
  ComparisonOp ->
  TypeUnblockingFunction (Thunk Builtin) m ->
  OperationUnblockingFunction TensorOp2Args (Thunk Builtin) m
unblockRatTensorExtrema op unblock (TensorOp2Args ds x y) = do
  x' <- unblock x
  y' <- unblock y
  forIfTreeM x' $ \x'' ->
    forIfTreeM y' $ \y'' -> do
      let cArgs = TensorComparisonArgs (Forced IDimNil) ds x'' y''
      let c = Forced $ mkExpr accessCompareRatTensor (op, cArgs)
      return $ IfTree c (IfLeaf x'') (IfLeaf y'')

unblockMinRatTensor ::
  TypeUnblockingFunction (Thunk Builtin) m ->
  OperationUnblockingFunction TensorOp2Args (Thunk Builtin) m
unblockMinRatTensor = unblockRatTensorExtrema Le

unblockMaxRatTensor ::
  TypeUnblockingFunction (Thunk Builtin) m ->
  OperationUnblockingFunction TensorOp2Args (Thunk Builtin) m
unblockMaxRatTensor = unblockRatTensorExtrema Ge

unblockForeachVector ::
  (MonadUnblock m) =>
  UnblockingActions m ->
  OperationUnblockingFunction ForeachVectorArgs (Thunk Builtin) m
unblockForeachVector actions (ForeachVectorArgs tElem d fn) = do
  d' <- unblockNatValue actions d
  forIfTreeM d' $ \d'' ->
    IfLeaf <$> do
      forceEval evalForeachVector $ ForeachVectorArgs tElem d'' fn

unblockMapList ::
  (MonadUnblock m) =>
  UnblockingActions m ->
  OperationUnblockingFunction MapListArgs (Thunk Builtin) m
unblockMapList actions (MapListArgs t1 t2 f xs) = do
  xs' <- unblockListValue actions xs
  forIfTreeM xs' $ \xs'' ->
    IfLeaf <$> do
      forceEval evalMapList $ MapListArgs t1 t2 f xs''

unblockFoldList ::
  (MonadUnblock m) =>
  UnblockingActions m ->
  OperationUnblockingFunction FoldListArgs (Thunk Builtin) m
unblockFoldList actions (FoldListArgs t1 t2 f e xs) = do
  xs' <- unblockListValue actions xs
  forIfTreeM xs' $ \xs'' ->
    IfLeaf <$> do
      forceEval evalFoldList $ FoldListArgs t1 t2 f e xs''

unblockConstTensor ::
  (MonadUnblock m) =>
  TypeUnblockingFunction (Thunk Builtin) m ->
  UnblockingActions m ->
  OperationUnblockingFunction ConstTensorArgs (Thunk Builtin) m
unblockConstTensor unblockValue actions (ConstTensorArgs t x ds) = do
  x' <- unblockValue x
  ds' <- unblockListValue actions ds
  forIfTreeM x' $ \x'' ->
    forIfTreeM ds' $ \ds'' ->
      IfLeaf <$> do
        forceEvaluation accessConstTensor evalConstTensor $ ConstTensorArgs t x'' ds''

--------------------------------------------------------------------------------
-- Unblocking operations

forceEval ::
  (MonadNorm Builtin m, HasCallStack) =>
  EvalSimple ForcedValue Thunk args Builtin m ->
  args (Thunk Builtin) ->
  m (Thunk Builtin)
forceEval evalFn args = do
  evalResult <- evalFn args
  case evalResult of
    Evaluated result -> return result
    Unevaluable {} -> developerError "Unblocking evaluation results in unevaluable result"

currentPass :: Doc a
currentPass = "unblocking"

showEntry :: forall m. (MonadUnblock m) => Thunk Builtin -> m (IfTree (Thunk Builtin) (Thunk Builtin)) -> m (IfTree (Thunk Builtin) (Thunk Builtin))
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
