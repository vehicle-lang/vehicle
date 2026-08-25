module Vehicle.Backend.Loss.LiftQuantifier
  ( liftQuantifiers,
    QuantifierData,
    LiftedData,
  )
where

import Control.Monad.Except (MonadError (..), runExceptT)
import Control.Monad.RWS (MonadReader, ask)
import Vehicle.Compile.Error
import Vehicle.Compile.LiftIf (unfoldIf)
import Vehicle.Compile.Normalise.Builtin (elimImplies)
import Vehicle.Compile.Normalise.Force
import Vehicle.Compile.Normalise.TypedValue
import Vehicle.Compile.Prelude
import Vehicle.Compile.Unblock (noUnblocking, unblockBoolExpr)
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Builtin.Standard hiding (And)
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Code.Interface.Args
import Vehicle.Data.Code.Interface.Operations
import Vehicle.Data.MaybeTrivial
import Vehicle.Data.Variable.Bound.Context.Name.Class
import Vehicle.Data.Variable.Free.Context (MonadFreeContext)
import Vehicle.Verify.Specification (Property, QuerySet (..), propertySize)

type QuantifierData = (Quantifier, Either (UnforcedDims Builtin, UnforcedDims Builtin) (UnforcedType Builtin), UnforcedBinder Builtin)

type HasForall = Bool

type HasExists = Bool

type LiftedData = ([QuantifierData], Thunk Builtin, HasForall, HasExists)

type MonadLiftQuantifiers m =
  ( MonadCompile m,
    MonadFreeContext Builtin m,
    MonadNameContext m,
    MonadReader DeclProvenance m
  )

liftQuantifiers ::
  (MonadLiftQuantifiers m) =>
  (Thunk Builtin, Lv) ->
  m (Property LiftedData, Lv)
liftQuantifiers (value, ctxDelta) = do
  forcedValue <- forceThunk value
  case toBoolValue forcedValue of
    VBoolLiteral _ ->
      return (NonTrivial $ Query $ QuerySet False (DisjunctAll [([], value, False, False)]), 0)
    VAnd (TensorOp2Args dims arg1 arg2) -> do
      (arg1', ctxSize1) <- liftQuantifiers (arg1, ctxDelta)
      (arg2', ctxSize2) <- liftQuantifiers (arg2, ctxDelta + ctxSize1)
      case (arg1', arg2') of
        (NonTrivial (Query (QuerySet _ (DisjunctAll [(quantifiers1, liftedValue1, hasForall1, hasExists1)]))), NonTrivial (Query (QuerySet _ (DisjunctAll [(quantifiers2, liftedValue2, hasForall2, hasExists2)])))) -> do
          let hasForall = hasForall1 || hasForall2
          let hasExists = hasExists1 || hasExists2
          if hasExists
            then do
              -- Undo the offset in the RHS arg when we don't lift
              liftedValue2' <- updateVarLevels (-(unLv $ ctxDelta + ctxSize1)) liftedValue2
              let query1 = Query (QuerySet hasForall1 (DisjunctAll [(quantifiers1, liftedValue1, hasForall1, hasExists1)]))
              let query2 = Query (QuerySet hasForall2 (DisjunctAll [(quantifiers2, liftedValue2', hasForall2, hasExists2)]))
              return (NonTrivial $ Conjunct $ ConjunctAll [query1, query2], ctxSize1 + ctxSize2)
            else do
              let newQuery = Query $ QuerySet hasForall (DisjunctAll [(quantifiers1 ++ quantifiers2, Forced $ mkExpr accessAndTensor (TensorOp2Args dims liftedValue1 liftedValue2), hasForall, hasExists)])
              return (NonTrivial newQuery, ctxSize1 + ctxSize2)
        (NonTrivial conjunctOrDisjunct1, NonTrivial conjunctOrDisjunct2) -> return (NonTrivial $ Conjunct $ ConjunctAll [conjunctOrDisjunct1, conjunctOrDisjunct2], ctxSize1 + ctxSize2)
        _ -> developerError "Conjunct cannot contain trivial args"
    VOr (TensorOp2Args dims arg1 arg2) -> do
      (arg1', ctxSize1) <- liftQuantifiers (arg1, ctxDelta)
      (arg2', ctxSize2) <- liftQuantifiers (arg2, ctxDelta + ctxSize1)
      case (arg1', arg2') of
        (NonTrivial (Query (QuerySet _ (DisjunctAll [(quantifiers1, liftedValue1, hasForall1, hasExists1)]))), NonTrivial (Query (QuerySet _ (DisjunctAll [(quantifiers2, liftedValue2, hasForall2, hasExists2)])))) -> do
          let hasForall = hasForall1 || hasForall2
          let hasExists = hasExists1 || hasExists2
          if hasForall
            then do
              -- Undo the offset in the RHS arg when we don't lift
              liftedValue2' <- updateVarLevels (-(unLv $ ctxDelta + ctxSize1)) liftedValue2
              let query1 = Query (QuerySet hasForall1 (DisjunctAll [(quantifiers1, liftedValue1, hasForall1, hasExists1)]))
              let query2 = Query (QuerySet hasForall2 (DisjunctAll [(quantifiers2, liftedValue2', hasForall2, hasExists2)]))
              return (NonTrivial $ Disjunct $ DisjunctAll [query1, query2], ctxSize1 + ctxSize2)
            else do
              let newQuery = Query $ QuerySet hasForall (DisjunctAll [(quantifiers1 ++ quantifiers2, Forced $ mkExpr accessOrTensor (TensorOp2Args dims liftedValue1 liftedValue2), hasForall, hasExists)])
              return (NonTrivial newQuery, ctxSize1 + ctxSize2)
        (NonTrivial conjunctOrDisjunct1, NonTrivial conjunctOrDisjunct2) -> return (NonTrivial $ Disjunct $ DisjunctAll [conjunctOrDisjunct1, conjunctOrDisjunct2], ctxSize1 + ctxSize2)
        _ -> developerError "Disjunct cannot contain trivial args"
    VNot (TensorOp1Args dims arg) -> do
      (arg', ctxSize) <- liftQuantifiers (arg, ctxDelta)
      case arg' of
        NonTrivial (Query (QuerySet _ (DisjunctAll [(quantifiers, newValue, hasForall, hasExists)]))) -> do
          let newQuery = Query $ QuerySet hasForall (DisjunctAll [(quantifiers, Forced $ mkExpr accessNotTensor $ TensorOp1Args dims newValue, hasForall, hasExists)])
          return (NonTrivial newQuery, ctxSize)
        NonTrivial _ -> developerError "Negation must be pushed below and/or"
        _ -> developerError "Negation cannot contain trivial args"
    VQuantifyRatTensor (quantifier, QuantifyRatTensorArgs pDims bDims binder closure) -> do
      lv <- getBinderDepth
      let normBody = extendClosureWithBound closure binder lv
      let quantifierData = (quantifier, Left (pDims, bDims), binder)
      (body', ctxSize) <- addNameToContext binder $ liftQuantifiers (normBody, ctxDelta)
      case body' of
        NonTrivial (Query querySet) -> do
          newQuerySet <- addQuantifierToQuerySet quantifierData querySet
          return (NonTrivial $ Query newQuerySet, ctxSize + 1)
        NonTrivial conjunctOrDisjunct -> do
          (lowered, newCtxSize) <- lowerQuantifier quantifierData ctxSize (NonTrivial conjunctOrDisjunct)
          return (lowered, newCtxSize)
        _ -> developerError "Quantifier body cannot be trivial"
    VQuantifyRecord (quantifier, QuantifyRecordArgs typ binder closure) -> do
      lv <- getBinderDepth
      let normBody = extendClosureWithBound closure binder lv
      let quantifierData = (quantifier, Right typ, binder)
      (body', ctxSize) <- addNameToContext binder $ liftQuantifiers (normBody, ctxDelta)
      case body' of
        NonTrivial (Query querySet) -> do
          newQuerySet <- addQuantifierToQuerySet quantifierData querySet
          return (NonTrivial $ Query newQuerySet, ctxSize + 1)
        NonTrivial conjunctOrDisjunct -> do
          (lowered, newCtxSize) <- lowerQuantifier quantifierData ctxSize (NonTrivial conjunctOrDisjunct)
          return (lowered, newCtxSize)
        _ -> developerError "Quantifier body cannot be trivial"
    VCompareIndex _ -> do
      newExpr <- updateVarLevels (unLv ctxDelta) value
      let newQuery = Query $ QuerySet False (DisjunctAll [([], newExpr, False, False)])
      return (NonTrivial newQuery, 0)
    VCompareNat _ -> do
      newExpr <- updateVarLevels (unLv ctxDelta) value
      let newQuery = Query $ QuerySet False (DisjunctAll [([], newExpr, False, False)])
      return (NonTrivial newQuery, 0)
    VCompareRatTensor _ -> do
      newExpr <- updateVarLevels (unLv ctxDelta) value
      let newQuery = Query $ QuerySet False (DisjunctAll [([], newExpr, False, False)])
      return (NonTrivial newQuery, 0)
    VBoolIf args -> do
      unfolded <- unfoldIf args
      liftQuantifiers (unfolded, ctxDelta)
    VImplies args -> do
      let unfolded = elimImplies args
      liftQuantifiers (unfolded, ctxDelta)
    VBoolVectorAt {} -> unblock
    VBoolFoldList {} -> unblock
    VReduceAndTensor {} -> unblock
    VReduceOrTensor {} -> unblock
    VBoolTensorAt {} -> unblock
  where
    unblock = do
      errorOrResult <- runExceptT $ unblockBoolExpr noUnblocking value
      case errorOrResult of
        Left _ -> do
          declProv <- ask
          throwError $ UnableToLiftQuantifiersInProperty declProv
        Right result -> liftQuantifiers (result, ctxDelta)

lowerQuantifier ::
  (MonadLiftQuantifiers m) =>
  QuantifierData ->
  Lv ->
  Property LiftedData ->
  m (Property LiftedData, Lv)
lowerQuantifier quantifierData ctxSize property = case property of
  NonTrivial expr -> do
    newExpr <- traverse (addQuantifierToQuerySet quantifierData) expr
    let newProperty = NonTrivial newExpr
    let newPropertySize = propertySize newProperty
    -- When a quantifier is lowered, it is prepended to every LiftedData's list of quantifier data
    -- so the property's context size grows by how many LiftedData leaves there are
    let newCtxSize = ctxSize + Lv newPropertySize
    return (newProperty, newCtxSize)
  _ -> developerError "Cannot lower a quantifier into an empty property"

addQuantifierToQuerySet ::
  (MonadLiftQuantifiers m) =>
  QuantifierData ->
  QuerySet LiftedData ->
  m (QuerySet LiftedData)
addQuantifierToQuerySet (quantifier, dimsOrType, binder) querySet = case querySet of
  QuerySet _ (DisjunctAll [(quantifiers, value, hasForall, hasExists)]) -> do
    let newQuantifiers = (quantifier, dimsOrType, binder) : quantifiers
    if quantifier == Forall
      then return $ QuerySet True (DisjunctAll [(newQuantifiers, value, True, hasExists)])
      else return $ QuerySet hasForall (DisjunctAll [(newQuantifiers, value, hasForall, True)])
  _ -> developerError "Missing lifted expression"

updateVarLevels ::
  (MonadLiftQuantifiers m) =>
  Int ->
  Thunk Builtin ->
  m (Thunk Builtin)
updateVarLevels offset value = do
  forcedValue <- forceThunk value
  case toBoolValue forcedValue of
    VBoolLiteral _ -> return value
    VAnd args -> do
      args' <- traverseTensorOp2Args (updateVarLevels offset) args
      return (Forced $ mkExpr accessAndTensor args')
    VOr args -> do
      args' <- traverseTensorOp2Args (updateVarLevels offset) args
      return (Forced $ mkExpr accessOrTensor args')
    VNot (TensorOp1Args dims arg) -> do
      arg' <- updateVarLevels offset arg
      return (Forced $ mkExpr accessNotTensor $ TensorOp1Args dims arg')
    VQuantifyRatTensor _ -> developerError "value should not contain a quantifier"
    VQuantifyRecord _ -> developerError "value should not contain a quantifier"
    VCompareIndex (op, IndexComparisonArgs size1 size2 arg1 arg2) -> do
      arg1' <- updateIndexBoundVar offset arg1
      arg2' <- updateIndexBoundVar offset arg2
      return (Forced $ mkExpr accessCompareIndex (op, IndexComparisonArgs size1 size2 arg1' arg2'))
    VCompareNat (op, args) -> do
      args' <- traverseOp2Args (updateNatBoundVar offset) args
      return (Forced $ mkExpr accessCompareNat (op, args'))
    VCompareRatTensor (op, TensorComparisonArgs rDims pDims xs ys) -> do
      xs' <- updateRatTensorBoundVar offset xs
      ys' <- updateRatTensorBoundVar offset ys
      let args' = TensorComparisonArgs rDims pDims xs' ys'
      return (Forced $ mkExpr accessCompareRatTensor (op, args'))
    VBoolIf args -> do
      unfolded <- unfoldIf args
      updateVarLevels offset unfolded
    VImplies args -> do
      let unfolded = elimImplies args
      updateVarLevels offset unfolded
    VBoolVectorAt {} -> unblock
    VBoolFoldList {} -> unblock
    VReduceAndTensor {} -> unblock
    VReduceOrTensor {} -> unblock
    VBoolTensorAt {} -> unblock
  where
    unblock = do
      errorOrResult <- runExceptT $ unblockBoolExpr noUnblocking value
      case errorOrResult of
        Left _ -> do
          declProv <- ask
          throwError $ UnableToLiftQuantifiersInProperty declProv
        Right result -> updateVarLevels offset result

updateIndexBoundVar ::
  (MonadLiftQuantifiers m) =>
  Int ->
  Thunk Builtin ->
  m (Thunk Builtin)
updateIndexBoundVar offset value = do
  forcedValue <- forceThunk value
  case toIndexValue forcedValue of
    VIndexLiteral {} ->
      return value
    VIndexBoundVar v spine -> do
      spine' <- traverseArgs (updateIndexBoundVar offset) spine
      let prevLv = unLv v
      return $ Forced $ VBoundVar (Lv (prevLv + offset)) spine'
    VIndexIf {} -> do
      declProv <- ask
      throwError $ UnableToLiftQuantifiersInProperty declProv
    VIndexAtVector (AtVectorArgs typ dim vector idx) -> do
      vector' <- updateIndexBoundVar offset vector
      return (Forced $ mkExpr accessAtVector (AtVectorArgs typ dim vector' idx))
    VIndexParameter {} ->
      return value
    VIndexRecordAcc typ val fieldName spine -> do
      val' <- updateIndexBoundVar offset val
      spine' <- traverseArgs (updateIndexBoundVar offset) spine
      return (Forced $ VRecordAcc typ val' fieldName spine')

updateNatBoundVar ::
  (MonadLiftQuantifiers m) =>
  Int ->
  Thunk Builtin ->
  m (Thunk Builtin)
updateNatBoundVar offset value = do
  forcedValue <- forceThunk value
  case toNatValue forcedValue of
    VNatLiteral _ -> return value
    VNatBoundVar v spine -> do
      spine' <- traverseArgs (updateNatBoundVar offset) spine
      let prevLv = unLv v
      return (Forced $ VBoundVar (Lv (prevLv + offset)) spine')
    VNatIf _ -> do
      declProv <- ask
      throwError $ UnableToLiftQuantifiersInProperty declProv
    VNatAdd args -> do
      args' <- traverseOp2Args (updateNatBoundVar offset) args
      return (Forced $ mkExpr accessAddNat args')
    VNatMul args -> do
      args' <- traverseOp2Args (updateNatBoundVar offset) args
      return (Forced $ mkExpr accessMulNat args')
    VNatParameter _ -> return value

updateRatTensorBoundVar ::
  (MonadLiftQuantifiers m) =>
  Int ->
  Thunk Builtin ->
  m (Thunk Builtin)
updateRatTensorBoundVar offset value = do
  forcedValue <- forceThunk value
  case toRatTensorValue forcedValue of
    VRatTensorLiteral _ ->
      return value
    VNegRatTensor (TensorOp1Args dims arg) -> do
      arg' <- updateRatTensorBoundVar offset arg
      return (Forced $ mkExpr accessNegRatTensor (TensorOp1Args dims arg'))
    VLogRatTensor (TensorOp1Args dims arg) -> do
      arg' <- updateRatTensorBoundVar offset arg
      return (Forced $ mkExpr accessLogRatTensor (TensorOp1Args dims arg'))
    VExpRatTensor (TensorOp1Args dims arg) -> do
      arg' <- updateRatTensorBoundVar offset arg
      return (Forced $ mkExpr accessExpRatTensor (TensorOp1Args dims arg'))
    VAddRatTensor args -> do
      args' <- traverseTensorOp2Args (updateRatTensorBoundVar offset) args
      return (Forced $ mkExpr accessAddRatTensor args')
    VSubRatTensor args -> do
      args' <- traverseTensorOp2Args (updateRatTensorBoundVar offset) args
      return (Forced $ mkExpr accessSubRatTensor args')
    VMulRatTensor args -> do
      args' <- traverseTensorOp2Args (updateRatTensorBoundVar offset) args
      return (Forced $ mkExpr accessMulRatTensor args')
    VDivRatTensor args -> do
      args' <- traverseTensorOp2Args (updateRatTensorBoundVar offset) args
      return (Forced $ mkExpr accessDivRatTensor args')
    VMinRatTensor args -> do
      args' <- traverseTensorOp2Args (updateRatTensorBoundVar offset) args
      return (Forced $ mkExpr accessMinRatTensor args')
    VMaxRatTensor args -> do
      args' <- traverseTensorOp2Args (updateRatTensorBoundVar offset) args
      return (Forced $ mkExpr accessMaxRatTensor args')
    VPowRatTensor args -> do
      args' <- traverseTensorOp2Args (updateRatTensorBoundVar offset) args
      return (Forced $ mkExpr accessPowRatTensor args')
    VReduceAddRatTensor args -> do
      args' <- traverseReductionArgs (updateRatTensorBoundVar offset) args
      return (Forced $ mkExpr accessReduceAddRat args')
    VReduceMulRatTensor args -> do
      args' <- traverseReductionArgs (updateRatTensorBoundVar offset) args
      return (Forced $ mkExpr accessReduceMulRat args')
    VReduceMinRatTensor args -> do
      args' <- traverseReductionArgs (updateRatTensorBoundVar offset) args
      return (Forced $ mkExpr accessReduceMinRat args')
    VReduceMaxRatTensor args -> do
      args' <- traverseReductionArgs (updateRatTensorBoundVar offset) args
      return (Forced $ mkExpr accessReduceMaxRat args')
    VIfRatTensor _ -> do
      declProv <- ask
      throwError $ UnableToLiftQuantifiersInProperty declProv
    VRatTensorBoundVar v -> do
      let prevLv = unLv v
      return (Forced $ VBoundVar (Lv (prevLv + offset)) [])
    VNetworkApplication ident (NetworkAppArgs arg) -> do
      arg' <- updateRatTensorBoundVar offset arg
      return (Forced $ VFreeVar ident (mkExpr accessSpine $ NetworkAppArgs arg'))
    VRatConstTensor args -> do
      args' <- traverseConstTensorValue (updateRatTensorBoundVar offset) args
      return (Forced $ mkExpr accessConstTensor args')
    VRatStackTensor args -> do
      args' <- traverseStackTensorElements (updateRatTensorBoundVar offset) args
      return (Forced $ mkExpr accessStackTensor args')
    VRatAtTensor args -> do
      args' <- traverseAtTensorArg (updateRatTensorBoundVar offset) args
      return (Forced $ mkExpr accessAtTensor args')
    VRatTensorTranspose args -> do
      args' <- traverseTransposeTensor (updateRatTensorBoundVar offset) args
      return (Forced $ mkExpr accessTransposeTensor args')
    VRatForeach (ForeachTensorArgs typ d ds fn) -> do
      fn' <- updateRatTensorBoundVar offset fn
      return (Forced $ mkExpr accessForeachTensor (ForeachTensorArgs typ d ds fn'))
    VRatTensorRecordAcc typ val fieldName spine -> do
      val' <- updateRatTensorBoundVar offset val
      spine' <- traverseArgs (updateRatTensorBoundVar offset) spine
      return (Forced $ VRecordAcc typ val' fieldName spine')
    VParameterOrDataset _ ->
      return value
    VRatAtVector (AtVectorArgs typ dim vector idx) -> do
      vector' <- updateRatTensorBoundVar offset vector
      return (Forced $ mkExpr accessAtVector (AtVectorArgs typ dim vector' idx))

{-unblockingActions :: (MonadLiftQuantifiers m) => UnblockingActions m
unblockingActions =
  UnblockingActions
    { unblockRatTensorBoundVar = \_ v -> return $ IfLeaf $ Forced $ VBoundVar v [],
      unblockRecordBoundVar = \_ v -> return $ IfLeaf $ Forced $ VBoundVar v [],
      unblockNetworkApp = \_ _ ident args -> return $ IfLeaf $ Forced $ VFreeVar ident (mkExpr accessSpine args),
      unblockDatasetOrParameter = \_ ident -> return $ IfLeaf $ Forced $ VFreeVar ident []
    }-}
