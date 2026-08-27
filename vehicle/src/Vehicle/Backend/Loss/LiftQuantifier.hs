module Vehicle.Backend.Loss.LiftQuantifier
  ( compileHardBooleanTree,
    QuantifierData,
    LiftedData,
  )
where

import Control.Monad.Except (MonadError (..), runExceptT)
import Control.Monad.RWS (MonadReader, ask)
import Vehicle.Compile.Error
import Vehicle.Compile.LiftIf (unfoldIf)
import Vehicle.Compile.LowerNot (lowerNot, negateQuantifierBody, negateRecordQuantifierBody)
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
import Vehicle.Data.Variable.Bound.Context.Name (prettyFriendlyInCtx)
import Vehicle.Data.Variable.Bound.Context.Name.Class
import Vehicle.Data.Variable.Free.Context (MonadFreeContext)
import Vehicle.Verify.Specification (Property, QuerySet (..))

type QuantifierData = (Quantifier, Either (UnforcedDims Builtin, UnforcedDims Builtin) (UnforcedType Builtin), UnforcedBinder Builtin)

type LiftedData = ([QuantifierData], Thunk Builtin, Lv)

type MonadLiftQuantifiers m =
  ( MonadCompile m,
    MonadFreeContext Builtin m,
    MonadNameContext m,
    MonadReader DeclProvenance m
  )

-- if at some point we hit an exists, then we call liftQuantifiers
-- if we hit a forall, then we convert it to not exists not and call liftQuantifiers on (exists not)
-- if we encounter a not, call lowerNot
compileHardBooleanTree ::
  (MonadLiftQuantifiers m) =>
  Thunk Builtin ->
  m (Property LiftedData)
compileHardBooleanTree value = do
  forcedValue <- forceThunk value
  case toBoolValue forcedValue of
    VBoolLiteral bool ->
      return $ Trivial bool
    VAnd (TensorOp2Args _ arg1 arg2) -> do
      arg1' <- compileHardBooleanTree arg1
      arg2' <- compileHardBooleanTree arg2
      return $ andTrivial (\x y -> andBoolExpr x y) arg1' arg2'
    VOr (TensorOp2Args _ arg1 arg2) -> do
      arg1' <- compileHardBooleanTree arg1
      arg2' <- compileHardBooleanTree arg2
      return $ orTrivial (\x y -> orBoolExpr x y) arg1' arg2'
    VNot args -> do
      errorOrResult <- runExceptT $ lowerNot noUnblocking args
      case errorOrResult of
        Left _ -> do
          declProv <- ask
          throwError $ UnableToLiftQuantifiersInProperty declProv
        Right result -> compileHardBooleanTree result
    VQuantifyRatTensor (quantifier, args) -> do
      case quantifier of
        Forall -> do
          let negatedBody = negateQuantifierBody args
          let existsExpr = Forced $ mkExpr accessQuantifyRatTensor (Exists, negatedBody)
          subtrees <- liftQuantifiers (existsExpr, 0)
          case subtrees of
            NonTrivial disjuncts -> return $ NonTrivial $ Query $ QuerySet True disjuncts
            Trivial bool -> return $ Trivial $ not bool
        Exists -> do
          subtrees <- liftQuantifiers (Forced forcedValue, 0)
          case subtrees of
            NonTrivial disjuncts -> return $ NonTrivial $ Query $ QuerySet False disjuncts
            Trivial bool -> return $ Trivial bool
    VQuantifyRecord (quantifier, args) -> do
      case quantifier of
        Forall -> do
          let negatedBody = negateRecordQuantifierBody args
          let existsExpr = Forced $ mkExpr accessQuantifyRecord (Exists, negatedBody)
          subtrees <- liftQuantifiers (existsExpr, 0)
          case subtrees of
            NonTrivial disjuncts -> return $ NonTrivial $ Query $ QuerySet True disjuncts
            Trivial bool -> return $ Trivial $ not bool
        Exists -> do
          subtrees <- liftQuantifiers (Forced forcedValue, 0)
          case subtrees of
            NonTrivial disjuncts -> return $ NonTrivial $ Query $ QuerySet False disjuncts
            Trivial bool -> return $ Trivial bool
    VCompareIndex _ -> unblock
    VCompareNat _ -> unblock
    VCompareRatTensor _ -> unblock
    VBoolIf args -> do
      unfolded <- unfoldIf args
      compileHardBooleanTree unfolded
    VImplies args -> do
      let unfolded = elimImplies args
      compileHardBooleanTree unfolded
    VBoolVectorAt {} -> unblock
    VBoolFoldList {} -> unblock
    VReduceAndTensor {} -> unblock
    VReduceOrTensor {} -> unblock
    VBoolTensorAt {} -> unblock
  where
    unblock = do
      errorOrResult <- runExceptT $ unblockBoolExpr noUnblocking value
      logDebug MaxDetail "compileHardBooleanTree"
      case errorOrResult of
        Left _ -> do
          declProv <- ask
          throwError $ UnableToLiftQuantifiersInProperty declProv
        Right result -> compileHardBooleanTree result

type SubTrees = MaybeTrivial (DisjunctAll LiftedData)

andResult ::
  forall m.
  (MonadLiftQuantifiers m) =>
  UnforcedDims Builtin ->
  Thunk Builtin ->
  Thunk Builtin ->
  Lv ->
  m SubTrees
andResult dims arg1 arg2 ctxDelta = do
  arg1' <- liftQuantifiers (arg1, ctxDelta)
  case arg1' of
    NonTrivial disjuncts -> do
      things <- traverse (flip compileRHS arg2) disjuncts
      let eliminateTrivial = eliminateTrivialDisjunctions things
      case eliminateTrivial of
        NonTrivial disjuncts' -> return $ NonTrivial $ disjunctDisjuncts disjuncts'
        Trivial True -> return arg1'
        Trivial False -> return $ Trivial False
    Trivial False -> return $ Trivial False
    Trivial True -> liftQuantifiers (arg2, ctxDelta)
  where
    compileRHS :: (MonadLiftQuantifiers m) => LiftedData -> Thunk Builtin -> m SubTrees
    compileRHS leftLiftedData@(_, _, ctxSizeLeft) arg = do
      subtreesRHS <- liftQuantifiers (arg, ctxDelta + ctxSizeLeft)
      case subtreesRHS of
        NonTrivial disjuncts -> return $ NonTrivial $ fmap (constructAnd leftLiftedData) disjuncts
        Trivial True -> return $ NonTrivial $ DisjunctAll [leftLiftedData]
        Trivial False -> return $ Trivial False

    constructAnd :: LiftedData -> LiftedData -> LiftedData
    constructAnd (quantifiersLeft, leftExpr, ctxSizeLeft) (quantifiersRight, rightExpr, ctxSizeRight) =
      ( quantifiersLeft ++ quantifiersRight,
        Forced $ mkExpr accessAndTensor (TensorOp2Args dims leftExpr rightExpr),
        ctxSizeLeft + ctxSizeRight
      )

orResult ::
  (MonadLiftQuantifiers m) =>
  Thunk Builtin ->
  Thunk Builtin ->
  Lv ->
  m SubTrees
orResult arg1 arg2 ctxDelta = do
  arg1' <- liftQuantifiers (arg1, ctxDelta)
  arg2' <- liftQuantifiers (arg2, ctxDelta)
  return $ orTrivial (<>) arg1' arg2'

-- throw an error if we encounter a forall here
liftQuantifiers ::
  (MonadLiftQuantifiers m) =>
  (Thunk Builtin, Lv) ->
  m SubTrees
liftQuantifiers (value, ctxDelta) = logEntryAndExit value $ do
  forcedValue <- forceThunk value
  case toBoolValue forcedValue of
    VBoolLiteral bool ->
      return $ Trivial bool
    VAnd (TensorOp2Args dims arg1 arg2) -> andResult dims arg1 arg2 ctxDelta
    VOr (TensorOp2Args _ arg1 arg2) -> orResult arg1 arg2 ctxDelta
    VNot args -> do
      -- call lowerNot, then call liftQuantifiers on result
      errorOrResult <- runExceptT $ lowerNot noUnblocking args
      case errorOrResult of
        Left _ -> do
          declProv <- ask
          throwError $ UnableToLiftQuantifiersInProperty declProv
        Right result -> liftQuantifiers (result, ctxDelta)
    VQuantifyRatTensor (quantifier, QuantifyRatTensorArgs pDims bDims binder closure) -> do
      case quantifier of
        Forall -> do
          declProv <- ask
          throwError $ UnableToLiftQuantifiersInProperty declProv
        Exists -> do
          lv <- getBinderDepth
          let normBody = extendClosureWithBound closure binder lv
          let quantifierData = (quantifier, Left (pDims, bDims), binder)
          subtrees <- addNameToContext binder $ liftQuantifiers (normBody, ctxDelta)
          case subtrees of
            NonTrivial disjuncts -> return $ NonTrivial $ fmap (lowerQuantifier quantifierData) disjuncts
            Trivial bool -> return $ Trivial bool
    VQuantifyRecord (quantifier, QuantifyRecordArgs typ binder closure) -> do
      case quantifier of
        Forall -> do
          declProv <- ask
          throwError $ UnableToLiftQuantifiersInProperty declProv
        Exists -> do
          lv <- getBinderDepth
          let normBody = extendClosureWithBound closure binder lv
          let quantifierData = (quantifier, Right typ, binder)
          subtrees <- addNameToContext binder $ liftQuantifiers (normBody, ctxDelta)
          case subtrees of
            NonTrivial disjuncts -> return $ NonTrivial $ fmap (lowerQuantifier quantifierData) disjuncts
            Trivial bool -> return $ Trivial bool
    VCompareIndex _ -> do
      newExpr <- updateVarLevels ctxDelta value
      return $ NonTrivial $ DisjunctAll [([], newExpr, 0)]
    VCompareNat _ -> do
      newExpr <- updateVarLevels ctxDelta value
      return $ NonTrivial $ DisjunctAll [([], newExpr, 0)]
    VCompareRatTensor _ -> do
      newExpr <- updateVarLevels ctxDelta value
      return $ NonTrivial $ DisjunctAll [([], newExpr, 0)]
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
      logDebug MaxDetail "liftQuantifiers"
      case errorOrResult of
        Left _ -> do
          -- TODO: In order for this to be sound, we need to check that there are no quantifiers in value
          newExpr <- updateVarLevels ctxDelta value
          return $ NonTrivial $ DisjunctAll [([], newExpr, 0)]
        Right result -> liftQuantifiers (result, ctxDelta)

lowerQuantifier ::
  QuantifierData ->
  LiftedData ->
  LiftedData
lowerQuantifier quantifierData (quantifiers, expr, ctxSize) = (quantifierData : quantifiers, expr, ctxSize + 1)

updateVarLevels ::
  (MonadLiftQuantifiers m) =>
  Lv ->
  Thunk Builtin ->
  m (Thunk Builtin)
updateVarLevels offset value = do
  forcedValue <- forceThunk value
  Forced <$> case forcedValue of
    VFreeVar ident spine -> VFreeVar ident <$> traverseArgs (updateVarLevels offset) spine
    VBuiltin ident spine -> VBuiltin ident <$> traverseArgs (updateVarLevels offset) spine
    VUniverse args -> return $ VUniverse args
    VBoundVar lv spine -> VBoundVar (lv + offset) <$> traverseArgs (updateVarLevels offset) spine
    VPi {} -> developerError "Cannot have VPi when updating variable levels"
    VLam {} -> developerError "Cannot have VLam when updating variable levels"
    VRecord i fs -> VRecord i <$> traverse (updateVarLevels offset) fs
    VRecordAcc typ record field spine -> do
      typ' <- updateVarLevels offset typ
      record' <- updateVarLevels offset record
      VRecordAcc typ' record' field <$> traverseArgs (updateVarLevels offset) spine

{-updateIndexBoundVar ::
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
      return (Forced $ mkExpr accessAtVector (AtVectorArgs typ dim vector' idx))-}

logEntryAndExit ::
  (MonadLiftQuantifiers m) =>
  Thunk Builtin ->
  m SubTrees ->
  m SubTrees
logEntryAndExit start action = do
  logDebugM MaxDetail $ do
    doc <- prettyFriendlyInCtx start
    return $ "lift-enter:" <+> doc
  incrCallDepth
  result <- action
  decrCallDepth
  logDebugM MaxDetail $ do
    -- doc <- prettyFriendlyInCtx result
    return "lift-exit:" -- <+> lineIndent doc
  return result
