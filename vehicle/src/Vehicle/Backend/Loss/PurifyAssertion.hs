{-# LANGUAGE CPP #-}

module Vehicle.Backend.Loss.PurifyAssertion
  ( tryPurifyRatTensorComparison,
    purifyNatComparison,
    purifyIndexComparison,
    purifyNotEqualRatTensorComparison,
    unblockingActions,
    BlockingReason (..),
  )
where

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif

import Control.Monad (liftM2)
import Control.Monad.Except (MonadError (..), runExceptT)
import Vehicle.Compile.Constants.ForcedValue (TensorValueLinearExpr)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Builtin (evalCompareIndex, evalCompareNat, evalCompareRatTensor, evalConstTensor, forceEvaluation)
import Vehicle.Compile.Normalise.Force
import Vehicle.Compile.Normalise.RewriteRules (forceAndRewriteTensor)
import Vehicle.Compile.Normalise.TypedValue
import Vehicle.Compile.Prelude
import Vehicle.Compile.Unblock (TypeUnblockingFunction, UnblockingActions (..), unblockRatTensorValue)
import Vehicle.Data.Assertion (Assertion, comparisonToAssertion)
import Vehicle.Data.Builtin.Interface (Accessor (..), applyAccessor)
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.BooleanExpr (IfTree (..), forIfTreeM, mapIfTreeLeaves)
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.MaybeTrivial (MaybeTrivial (..))
import Vehicle.Data.Real (ExtendedRational (..))
import Vehicle.Data.Tensor (Tensor (..))
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Data.Variable.Bound.Context.Tensor
import Vehicle.Data.Variable.Free.Context (MonadFreeContext)

-- | Monad purify
type MonadPurifyAssertion m =
  ( MonadLogger m,
    MonadFreeContext Builtin m,
    MonadTensorBoundContext m
  )

-- | Goes through a comparison of naturals and attempts to evaluate it. If we can
-- reduce it to a boolean literal then we can often simplify the final tree structure.
purifyNatComparison ::
  (MonadPurifyAssertion m) =>
  (ComparisonOp, Op2Args (Thunk Builtin)) ->
  m (MaybeTrivial (Thunk Builtin))
purifyNatComparison (op, args) = do
  -- TODO: should actually traverse the whole expression trying to force evaluation
  result <- forceThunk =<< forceEvaluation (applyAccessor accessCompareNat op) (evalCompareNat op) args
  return $ case result of
    IBoolLiteral b -> Trivial b
    _ -> NonTrivial $ Forced result

-- | Goes through a comparison of indices and attempts to evaluate it. If we can
-- reduce it to a boolean literal then we can often simplify the final tree structure.
purifyIndexComparison ::
  (MonadPurifyAssertion m) =>
  (ComparisonOp, IndexComparisonArgs (Thunk Builtin)) ->
  m (MaybeTrivial (Thunk Builtin))
purifyIndexComparison (op, args) = do
  -- TODO: should actually traverse the whole expression trying to force evaluation
  result <- forceThunk =<< forceEvaluation (applyAccessor accessCompareIndex op) (evalCompareIndex op) args
  return $ case result of
    IBoolLiteral b -> Trivial b
    _ -> NonTrivial $ Forced result

-- | Goes through a comparison of indices and attempts to evaluate it. If we can
-- reduce it to a boolean literal then we can often simplify the final tree structure.
purifyNotEqualRatTensorComparison ::
  (MonadPurifyAssertion m) =>
  TensorComparisonArgs (Thunk Builtin) ->
  m (MaybeTrivial (Thunk Builtin))
purifyNotEqualRatTensorComparison args = do
  -- TODO: should actually traverse the whole expression trying to force evaluation
  result <- forceThunk =<< forceEvaluation (applyAccessor accessCompareRatTensor Ne) (evalCompareRatTensor Ne) args
  return $ case result of
    IBoolLiteral b -> Trivial b
    _ -> NonTrivial $ Forced result

-- | Takes a tensor level comparison and returns a tree of possible assertions
-- generated from it over the hierarchical tensor variables in scope.
--
--  e.g. `x <= if x > 0 then 2 else 3` -> IfTree (x > 0) (0 <= -x + 2) (0 <= -x + 3)
--
-- The first component of each leaf is the raw value, and the second optional component
-- is the compiled linear expression that could be used to construct the domain of a bound variable.
tryPurifyRatTensorComparison ::
  (MonadPurifyAssertion m) =>
  ComparisonOp ->
  TensorComparisonArgs (Thunk Builtin) ->
  m (IfTree (Thunk Builtin) (MaybeTrivial (Thunk Builtin, Maybe (Assertion (TensorValueLinearExpr Builtin)))))
tryPurifyRatTensorComparison op (TensorComparisonArgs _pDims rDims e1 e2) = do
  e1' <- compileLinearExpr rDims e1
  e2' <- compileLinearExpr rDims e2
  forIfTreeM e1' $ \e1'' ->
    forIfTreeM e2' $ \e2'' -> do
      IfLeaf <$> do
        let args = TensorComparisonArgs (Forced IDimNil) rDims (value e1'') (value e2'')
        let val = Forced $ mkExpr accessCompareRatTensor (op, args)
        maybeSolvedVal <- sequence $ liftA2 (comparisonToAssertion op) (valueAsLinearExpr e1'') (valueAsLinearExpr e2'')
        return $ case maybeSolvedVal of
          Nothing -> NonTrivial (val, Nothing)
          Just (Trivial b) -> Trivial b
          Just (NonTrivial le) -> NonTrivial (val, Just le)

--------------------------------------------------------------------------------
-- Compiling linear expressions

data Result = Result
  { value :: Thunk Builtin,
    valueAsLinearExpr :: Maybe (TensorValueLinearExpr Builtin)
  }

type BranchingResult = IfTree (Thunk Builtin) Result

compileLinearExpr ::
  forall m.
  (MonadPurifyAssertion m) =>
  UnforcedDims Builtin ->
  Thunk Builtin ->
  m BranchingResult
compileLinearExpr dims expr =
  logEntryAndExit expr $ do
    forcedValue <- forceAndRewriteTensor expr
    case toRatTensorValue forcedValue of
      ---------------------
      -- Handlable cases --
      ---------------------
      VRatTensorLiteral {} -> compileAsConstantExpr dims expr
      VParameterOrDataset {} -> compileAsConstantExpr dims expr
      VRatTensorBoundVar var spine -> compileRatTensorVar dims var spine
      VNegRatTensor args -> compileNegRatTensor recCompile args
      VAddRatTensor args -> compileAddRatTensor recCompile args
      VSubRatTensor args -> compileSubRatTensor recCompile args
      VMulRatTensor args -> compileMulRatTensor recCompile args
      VDivRatTensor args -> compileDivRatTensor recCompile args
      VIfRatTensor args -> compileIf recCompile args
      -----------------------------
      -- Potentially unblockable --
      -----------------------------
      VPowRatTensor {} -> tryUnblock
      VLogRatTensor {} -> tryUnblock
      VExpRatTensor {} -> tryUnblock
      VMinRatTensor {} -> tryUnblock
      VMaxRatTensor {} -> tryUnblock
      VReduceAddRatTensor {} -> tryUnblock
      VReduceMulRatTensor {} -> tryUnblock
      VReduceMinRatTensor {} -> tryUnblock
      VReduceMaxRatTensor {} -> tryUnblock
      VRatAtTensor {} -> tryUnblock
      VRatForeach {} -> tryUnblock
      VRatTensorRecordAcc {} -> tryUnblock
      VRatAtVector {} -> tryUnblock
      VRatTensorTranspose {} -> tryUnblock
      ------------------------
      -- Definitely blocked --
      ------------------------
      VNetworkApplication {} -> blocked
      --------------------
      -- Unsure blocked --
      --------------------
      -- Very unsure about these two. We could try to use `unblockRatTensor`
      -- but that at the moment returns `VRatStackTensor` as is...
      VRatConstTensor {} -> compileAsConstantExpr dims expr
      VRatStackTensor {} -> compileAsConstantExpr dims expr
  where
    ------------------------
    -- Helper definitions --
    ------------------------

    tryUnblock :: m BranchingResult
    tryUnblock = tryAndUnblock dims expr

    blocked :: m BranchingResult
    blocked = return $ IfLeaf $ Result expr Nothing

    recCompile :: Thunk Builtin -> m BranchingResult
    recCompile = compileLinearExpr dims

compileAsConstantExpr ::
  (MonadPurifyAssertion m) =>
  UnforcedDims Builtin ->
  Thunk Builtin ->
  m BranchingResult
compileAsConstantExpr dims value = do
  return $
    IfLeaf $
      Result
        { value = value,
          valueAsLinearExpr = Just $ constantExpr $ TensorValue dims value
        }

compileRatTensorVar ::
  (MonadPurifyAssertion m) =>
  UnforcedDims Builtin ->
  Lv ->
  UnforcedSpine Builtin ->
  m BranchingResult
compileRatTensorVar dims lv spine = do
  valueAsLinearExpr <- case spine of
    _ : _ -> return Nothing
    [] -> do
      maybeSliceVar <- lookupSliceVariableInNestedCtx lv
      case maybeSliceVar of
        Nothing -> return Nothing
        Just sliceVar -> do
          zeroTensor <- forceEvaluation accessConstTensor evalConstTensor $ ConstTensorArgs (Forced IRatType) (Forced $ IRatLiteral 0) dims
          return $ Just $ singletonVarExpr (TensorValue dims zeroTensor) sliceVar

  return $
    IfLeaf $
      Result
        { value = Forced $ VBoundVar lv [],
          valueAsLinearExpr = valueAsLinearExpr
        }

compileNegRatTensor ::
  (MonadPurifyAssertion m) =>
  TypeUnblockingFunction Result m ->
  TensorOp1Args (Thunk Builtin) ->
  m BranchingResult
compileNegRatTensor recCompile =
  compileTensorOp1 recCompile (mkExpr accessNegRatTensor) (scaleExpr (-1))

compileAddRatTensor ::
  (MonadPurifyAssertion m) =>
  TypeUnblockingFunction Result m ->
  TensorOp2Args (Thunk Builtin) ->
  m BranchingResult
compileAddRatTensor recCompile =
  compileTensorOp2 recCompile (mkExpr accessAddRatTensor) (addLinearExprs 1 1)

compileSubRatTensor ::
  (MonadPurifyAssertion m) =>
  TypeUnblockingFunction Result m ->
  TensorOp2Args (Thunk Builtin) ->
  m BranchingResult
compileSubRatTensor recCompile =
  compileTensorOp2 recCompile (mkExpr accessSubRatTensor) (addLinearExprs 1 (-1))

compileMulRatTensor ::
  (MonadPurifyAssertion m) =>
  TypeUnblockingFunction Result m ->
  TensorOp2Args (Thunk Builtin) ->
  m BranchingResult
compileMulRatTensor recCompile =
  compileTensorOp2 recCompile (mkExpr accessMulRatTensor) multiplyLinearExprs

compileDivRatTensor ::
  (MonadPurifyAssertion m) =>
  TypeUnblockingFunction Result m ->
  TensorOp2Args (Thunk Builtin) ->
  m BranchingResult
compileDivRatTensor recCompile =
  compileTensorOp2 recCompile (mkExpr accessDivRatTensor) divideLinearExprs

compileTensorOp1 ::
  (MonadPurifyAssertion m) =>
  TypeUnblockingFunction Result m ->
  (TensorOp1Args (Thunk Builtin) -> ForcedValue Builtin) ->
  (TensorValueLinearExpr Builtin -> m (TensorValueLinearExpr Builtin)) ->
  TensorOp1Args (Thunk Builtin) ->
  m BranchingResult
compileTensorOp1 compile evalFn evalLinearExpr (TensorOp1Args ds xs) = do
  xs' <- compile xs
  forIfTreeM xs' $ \result ->
    IfLeaf <$> do
      let newValue = Forced $ evalFn $ TensorOp1Args ds (value result)
      newLinearExpr <- traverse evalLinearExpr (valueAsLinearExpr result)
      return $
        Result
          { value = newValue,
            valueAsLinearExpr = newLinearExpr
          }

compileTensorOp2 ::
  (MonadPurifyAssertion m) =>
  TypeUnblockingFunction Result m ->
  (TensorOp2Args (Thunk Builtin) -> ForcedValue Builtin) ->
  (TensorValueLinearExpr Builtin -> TensorValueLinearExpr Builtin -> Maybe (m (TensorValueLinearExpr Builtin))) ->
  TensorOp2Args (Thunk Builtin) ->
  m BranchingResult
compileTensorOp2 compile evalFn evalLinearExpr (TensorOp2Args ds xs ys) = do
  xs' <- compile xs
  ys' <- compile ys
  forIfTreeM xs' $ \rxs'' ->
    forIfTreeM ys' $ \rys'' ->
      IfLeaf <$> do
        let newValue = Forced $ evalFn $ TensorOp2Args ds (value rxs'') (value rys'')
        let maybeLinearExprFn = liftM2 evalLinearExpr (valueAsLinearExpr rxs'') (valueAsLinearExpr rys'')
        newLinearExpr <- maybe (return Nothing) sequence maybeLinearExprFn
        return $
          Result
            { value = newValue,
              valueAsLinearExpr = newLinearExpr
            }

compileIf ::
  (MonadPurifyAssertion m) =>
  TypeUnblockingFunction Result m ->
  IfArgs (Thunk Builtin) ->
  m BranchingResult
compileIf compile (IfArgs _t c x y) = do
  x' <- compile x
  y' <- compile y
  -- TODO: if x' and y' contain no constraints then we don't actually
  -- have to branch??
  return $ IfTree c x' y'

--------------------------------------------------------------------------------
-- Unblocking

tryAndUnblock ::
  (MonadPurifyAssertion m) =>
  UnforcedDims Builtin ->
  Thunk Builtin ->
  m BranchingResult
tryAndUnblock dims expr = do
  callDepth <- getCallDepth
  errorOrResult <- runExceptT $ unblockRatTensorValue unblockingActions expr
  case errorOrResult of
    Left (BlockingNetwork ident) -> do
      setCallDepth callDepth
      logDebug MaxDetail $ "contains network" <+> quotePretty ident <+> "so cannot be constraint"
      return $ IfLeaf $ Result expr Nothing
    Left BlockingDatasetOrParameter {} -> compileAsConstantExpr dims expr
    Right unblocked -> forIfTreeM unblocked $ \unblockedExpr ->
      compileLinearExpr dims unblockedExpr

unblockingActions ::
  (MonadPurifyAssertion m, MonadError BlockingReason m) =>
  UnblockingActions m
unblockingActions =
  UnblockingActions
    { unblockBoundVar = purifyBoundVar,
      unblockNetworkApp = \_ _ ident _ -> throwError $ BlockingNetwork ident,
      unblockDatasetOrParameter = \_ ident -> throwError $ BlockingDatasetOrParameter ident
    }

purifyBoundVar ::
  (MonadPurifyAssertion m) =>
  TypeUnblockingFunction (Thunk Builtin) m ->
  Lv ->
  UnforcedSpine Builtin ->
  m (IfTree (Thunk Builtin) (Thunk Builtin))
purifyBoundVar unblock lv spine = case spine of
  _ : _ -> unexpectedExprError "purification" "bound var with non-empty spine"
  [] -> do
    maybeSliceVar <- lookupSliceVariableInNestedCtx lv
    case maybeSliceVar of
      Nothing -> return $ IfLeaf $ Forced $ VBoundVar lv []
      Just sliceVar -> unblock =<< replaceTensorVariableWithStackedChildren sliceVar

--------------------------------------------------------------------------------
-- Utility functions

isFiniteConstant :: DimensionedTensorValue Builtin -> Maybe Rational
isFiniteConstant = \case
  TensorValue _ (Forced (IRatTensor (ConstantTensor _ (Finite c1)))) -> Just c1
  _ -> Nothing

addLinearExprs ::
  (MonadNorm Builtin m) =>
  Coefficient ->
  Coefficient ->
  TensorValueLinearExpr Builtin ->
  TensorValueLinearExpr Builtin ->
  Maybe (m (TensorValueLinearExpr Builtin))
addLinearExprs c1 c2 le1 le2 = Just $ addExprsUnsafe c1 c2 le1 le2

multiplyLinearExprs ::
  (MonadNorm Builtin m) =>
  TensorValueLinearExpr Builtin ->
  TensorValueLinearExpr Builtin ->
  Maybe (m (TensorValueLinearExpr Builtin))
multiplyLinearExprs le1 le2 = case (isConstant le1, isConstant le2) of
  (Just (isFiniteConstant -> Just c1), _) -> Just $ scaleExpr c1 le2
  (_, Just (isFiniteConstant -> Just c2)) -> Just $ scaleExpr c2 le1
  (Just (TensorValue dims c1), Just (TensorValue _ c2)) -> Just $ do
    let value = Forced $ mkExpr accessMulRatTensor $ TensorOp2Args dims c1 c2
    return $ constantExpr $ TensorValue dims value
  _ -> Nothing

divideLinearExprs ::
  (MonadNorm Builtin m) =>
  TensorValueLinearExpr Builtin ->
  TensorValueLinearExpr Builtin ->
  Maybe (m (TensorValueLinearExpr Builtin))
divideLinearExprs le1 le2 = case (isConstant le1, isConstant le2) of
  (_, Just (isFiniteConstant -> Just c2)) -> Just $ scaleExpr (1 / c2) le1
  (Just (TensorValue dims c1), Just (TensorValue _ c2)) -> Just $ do
    let value = Forced $ mkExpr accessDivRatTensor $ TensorOp2Args dims c1 c2
    return $ constantExpr $ TensorValue dims value
  _ -> Nothing

logEntryAndExit ::
  (MonadPurifyAssertion m) =>
  Thunk Builtin ->
  m BranchingResult ->
  m BranchingResult
logEntryAndExit start action = do
  logDebugM MaxDetail $ do
    doc <- prettyFriendlyInCtx start
    return $ "enter-assertion:" <+> doc
  incrCallDepth
  result <- action
  decrCallDepth
  logDebugM MaxDetail $ do
    doc <- prettyFriendlyInCtx $ mapIfTreeLeaves valueAsLinearExpr result
    return $ "exit-assertion:" <+> doc
  return result
