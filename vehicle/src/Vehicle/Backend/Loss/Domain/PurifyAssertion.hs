{-# LANGUAGE CPP #-}

module Vehicle.Backend.Loss.Domain.PurifyAssertion
  ( tryPurifyAssertion,
    unblockingActions,
  )
where

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif

import Control.Monad (join)
import Control.Monad.Except (MonadError (..), runExceptT)
import Vehicle.Compile.Constants.Value (TensorValueLinearExpr)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Unblock (TypeUnblockingFunction, UnblockingActions (..), unblockRatTensorValue)
import Vehicle.Data.Assertion (Assertion, comparisonToAssertion)
import Vehicle.Data.Builtin.Interface.Normalise (evalConstTensor)
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.BooleanExpr (IfTree (..), forIfTreeM, mapIfTreeLeaves)
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Value
import Vehicle.Data.MaybeTrivial (MaybeTrivial)
import Vehicle.Data.Real (ExtendedRational (..))
import Vehicle.Data.Tensor (Tensor (..))
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Data.Variable.Bound.Context.Tensor
import Vehicle.Data.Variable.Free.Context (MonadFreeContext)

-- | Monad purify
type MonadPurifyAssertion m =
  ( MonadLogger m,
    MonadFreeContext Builtin m,
    MonadReadableTensorBoundContext m
  )

-- | Takes a tensor level comparison and returns a tree of possible assertions
-- generated from it over the hierarchical tensor variables in scope.
--
--  e.g. `x <= if x > 0 then 2 else 3` -> IfTree (x > 0) (0 <= -x + 2) (0 <= -x + 3)
--
-- The first component of each leaf is the raw value, and the second optional component
-- is the compiled linear expression that could be used to construct the domain of a bound variable.
tryPurifyAssertion ::
  (MonadPurifyAssertion m) =>
  ComparisonOp ->
  TensorOp2Args (Value Builtin) ->
  m (IfTree (Value Builtin) (Value Builtin, Maybe (MaybeTrivial (Assertion (TensorValueLinearExpr Builtin)))))
tryPurifyAssertion op (TensorOp2Args dims e1 e2) = do
  e1' <- compileLinearExpr dims e1
  e2' <- compileLinearExpr dims e2
  forIfTreeM e1' $ \e1'' ->
    forIfTreeM e2' $ \e2'' -> do
      IfLeaf <$> do
        let val = fromBoolValue (VCompareRatTensor (op, TensorOp2Args dims (value e1'') (value e2'')))
        solvedVal <- sequence $ liftA2 (comparisonToAssertion op) (valueAsLinearExpr e1'') (valueAsLinearExpr e2'')
        return (val, solvedVal)

--------------------------------------------------------------------------------
-- Compiling linear expressions

data Result = Result
  { value :: Value Builtin,
    valueAsLinearExpr :: Maybe (TensorValueLinearExpr Builtin)
  }

type BranchingResult = IfTree (Value Builtin) Result

compileLinearExpr ::
  forall m.
  (MonadPurifyAssertion m) =>
  VDims Builtin ->
  Value Builtin ->
  m BranchingResult
compileLinearExpr dims expr =
  logEntryAndExit expr $ case toRatTensorValue expr of
    ---------------------
    -- Handlable cases --
    ---------------------
    VRatTensorLiteral {} -> compileAsConstantExpr dims expr
    VDatasetOrParameter {} -> compileAsConstantExpr dims expr
    VRatTensorBoundVar var -> compileRatTensorVar dims var
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
    VRatRecordAcc {} -> tryUnblock
    VRatAtVector {} -> tryUnblock
    ------------------------
    -- Definitely blocked --
    ------------------------
    VRatTensorNetworkApp {} -> blocked
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

    recCompile :: Value Builtin -> m BranchingResult
    recCompile = compileLinearExpr dims

compileAsConstantExpr ::
  (MonadPurifyAssertion m) =>
  VDims Builtin ->
  Value Builtin ->
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
  VDims Builtin ->
  Lv ->
  m BranchingResult
compileRatTensorVar dims lv = do
  valueAsLinearExpr <- do
    (_, maybeSliceVar) <- lookupVariableInNestedCtx lv
    case maybeSliceVar of
      Nothing -> return Nothing
      Just (_, sliceVar) -> do
        zeroTensor <- evalConstTensor $ ConstTensorArgs IRatType (IRatLiteral 0) dims
        return $ Just $ singletonVarExpr (TensorValue dims zeroTensor) sliceVar

  return $
    IfLeaf $
      Result
        { value = VBoundVar lv [],
          valueAsLinearExpr = valueAsLinearExpr
        }

compileNegRatTensor ::
  (MonadPurifyAssertion m) =>
  TypeUnblockingFunction Result m ->
  TensorOp1Args (Value Builtin) ->
  m BranchingResult
compileNegRatTensor recCompile =
  compileTensorOp1 recCompile (fromRatTensorValue . VNegRatTensor) (scaleExpr (-1))

compileAddRatTensor ::
  (MonadPurifyAssertion m) =>
  TypeUnblockingFunction Result m ->
  TensorOp2Args (Value Builtin) ->
  m BranchingResult
compileAddRatTensor recCompile =
  compileTensorOp2 recCompile (fromRatTensorValue . VAddRatTensor) (addLinearExprs 1 1)

compileSubRatTensor ::
  (MonadPurifyAssertion m) =>
  TypeUnblockingFunction Result m ->
  TensorOp2Args (Value Builtin) ->
  m BranchingResult
compileSubRatTensor recCompile =
  compileTensorOp2 recCompile (fromRatTensorValue . VSubRatTensor) (addLinearExprs 1 (-1))

compileMulRatTensor ::
  (MonadPurifyAssertion m) =>
  TypeUnblockingFunction Result m ->
  TensorOp2Args (Value Builtin) ->
  m BranchingResult
compileMulRatTensor recCompile =
  compileTensorOp2 recCompile (fromRatTensorValue . VMulRatTensor) multiplyLinearExprs

compileDivRatTensor ::
  (MonadPurifyAssertion m) =>
  TypeUnblockingFunction Result m ->
  TensorOp2Args (Value Builtin) ->
  m BranchingResult
compileDivRatTensor recCompile =
  compileTensorOp2 recCompile (fromRatTensorValue . VDivRatTensor) divideLinearExprs

compileTensorOp1 ::
  (MonadPurifyAssertion m) =>
  TypeUnblockingFunction Result m ->
  (TensorOp1Args (Value Builtin) -> Value Builtin) ->
  (TensorValueLinearExpr Builtin -> TensorValueLinearExpr Builtin) ->
  TensorOp1Args (Value Builtin) ->
  m BranchingResult
compileTensorOp1 compile evalFn evalLinearExpr (TensorOp1Args ds xs) = do
  xs' <- compile xs
  forIfTreeM xs' $ \result ->
    IfLeaf <$> do
      return $
        Result
          { value = evalFn $ TensorOp1Args ds (value result),
            valueAsLinearExpr = fmap evalLinearExpr (valueAsLinearExpr result)
          }

compileTensorOp2 ::
  (MonadPurifyAssertion m) =>
  TypeUnblockingFunction Result m ->
  (TensorOp2Args (Value Builtin) -> Value Builtin) ->
  (TensorValueLinearExpr Builtin -> TensorValueLinearExpr Builtin -> Maybe (TensorValueLinearExpr Builtin)) ->
  TensorOp2Args (Value Builtin) ->
  m BranchingResult
compileTensorOp2 compile evalFn evalLinearExpr (TensorOp2Args ds xs ys) = do
  xs' <- compile xs
  ys' <- compile ys
  forIfTreeM xs' $ \rxs'' ->
    forIfTreeM ys' $ \rys'' ->
      IfLeaf <$> do
        return $
          Result
            { value = evalFn $ TensorOp2Args ds (value rxs'') (value rys''),
              valueAsLinearExpr = join $ liftA2 evalLinearExpr (valueAsLinearExpr rxs'') (valueAsLinearExpr rys'')
            }

compileIf ::
  (MonadPurifyAssertion m) =>
  TypeUnblockingFunction Result m ->
  IfArgs (Value Builtin) ->
  m BranchingResult
compileIf compile (IfArgs _t c x y) = do
  x' <- compile x
  y' <- compile y
  return $ IfTree c x' y'

--------------------------------------------------------------------------------
-- Unblocking

tryAndUnblock ::
  (MonadPurifyAssertion m) =>
  VDims Builtin ->
  Value Builtin ->
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

data BlockingReason
  = BlockingNetwork Identifier
  | BlockingDatasetOrParameter Identifier

unblockingActions ::
  (MonadPurifyAssertion m, MonadError BlockingReason m) =>
  UnblockingActions m
unblockingActions =
  UnblockingActions
    { unblockRatTensorBoundVar = purifyBoundVar,
      unblockRecordBoundVar = purifyBoundVar,
      unblockNetworkApp = \_ _ ident _ -> throwError $ BlockingNetwork ident,
      unblockDatasetOrParameter = \ident -> throwError $ BlockingDatasetOrParameter ident
    }

purifyBoundVar ::
  (MonadLogger m, MonadReadableTensorBoundContext m) =>
  Lv ->
  m (Value Builtin)
purifyBoundVar lv = do
  (_, maybeUserVars) <- lookupVariableInNestedCtx lv
  case maybeUserVars of
    Nothing -> return $ VBoundVar lv []
    Just (_tensorVar, sliceVar) -> replaceTensorVariableWithStackedChildren sliceVar

--------------------------------------------------------------------------------
-- Utility functions

isFiniteConstant :: DimensionedTensorValue Builtin -> Maybe Rational
isFiniteConstant = \case
  TensorValue _ (IRatTensor (ConstantTensor _ (Finite c1))) -> Just c1
  _ -> Nothing

addLinearExprs ::
  Coefficient ->
  Coefficient ->
  TensorValueLinearExpr Builtin ->
  TensorValueLinearExpr Builtin ->
  Maybe (TensorValueLinearExpr Builtin)
addLinearExprs c1 c2 le1 le2 = Just $ addExprsUnsafe c1 c2 le1 le2

multiplyLinearExprs ::
  TensorValueLinearExpr Builtin ->
  TensorValueLinearExpr Builtin ->
  Maybe (TensorValueLinearExpr Builtin)
multiplyLinearExprs le1 le2 = case (isConstant le1, isConstant le2) of
  (Just (isFiniteConstant -> Just c1), _) -> Just $ scaleExpr c1 le2
  (_, Just (isFiniteConstant -> Just c2)) -> Just $ scaleExpr c2 le1
  (Just (TensorValue dims c1), Just (TensorValue _ c2)) -> Just $ do
    let value = fromRatTensorValue $ VMulRatTensor $ TensorOp2Args dims c1 c2
    constantExpr $ TensorValue dims value
  _ -> Nothing

divideLinearExprs ::
  TensorValueLinearExpr Builtin ->
  TensorValueLinearExpr Builtin ->
  Maybe (TensorValueLinearExpr Builtin)
divideLinearExprs le1 le2 = case (isConstant le1, isConstant le2) of
  (_, Just (isFiniteConstant -> Just c2)) -> Just $ scaleExpr (1 / c2) le1
  (Just (TensorValue dims c1), Just (TensorValue _ c2)) -> Just $ do
    let value = fromRatTensorValue $ VDivRatTensor $ TensorOp2Args dims c1 c2
    constantExpr $ TensorValue dims value
  _ -> Nothing

logEntryAndExit ::
  (MonadPurifyAssertion m) =>
  Value Builtin ->
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
