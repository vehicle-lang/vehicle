module Vehicle.Backend.Loss.LiftQuantifier
  ( compileHardBooleanTree,
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

type LiftedData = ([QuantifierData], Thunk Builtin)

type LeafDisjunction = MaybeTrivial (DisjunctAll LiftedData)

type MonadLiftQuantifiers m =
  ( MonadCompile m,
    MonadFreeContext Builtin m,
    MonadNameContext m,
    MonadReader DeclProvenance m
  )

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
        -- If a universal quantifier is encountered, convert it to not exists not ...
        -- then call liftQuantifiers on (exists not ...)
        Forall -> do
          let negatedBody = negateQuantifierBody args
          let existsExpr = Forced $ mkExpr accessQuantifyRatTensor (Exists, negatedBody)
          leafDisjunction <- liftQuantifiers (existsExpr, 0)
          case leafDisjunction of
            NonTrivial disjuncts -> return $ NonTrivial $ Query $ QuerySet True disjuncts
            Trivial bool -> return $ Trivial $ not bool
        Exists -> do
          leafDisjunction <- liftQuantifiers (Forced forcedValue, 0)
          case leafDisjunction of
            NonTrivial disjuncts -> return $ NonTrivial $ Query $ QuerySet False disjuncts
            Trivial bool -> return $ Trivial bool
    VQuantifyRecord (quantifier, args) -> do
      case quantifier of
        Forall -> do
          let negatedBody = negateRecordQuantifierBody args
          let existsExpr = Forced $ mkExpr accessQuantifyRecord (Exists, negatedBody)
          leafDisjunction <- liftQuantifiers (existsExpr, 0)
          case leafDisjunction of
            NonTrivial disjuncts -> return $ NonTrivial $ Query $ QuerySet True disjuncts
            Trivial bool -> return $ Trivial $ not bool
        Exists -> do
          leafDisjunction <- liftQuantifiers (Forced forcedValue, 0)
          case leafDisjunction of
            NonTrivial disjuncts -> return $ NonTrivial $ Query $ QuerySet False disjuncts
            Trivial bool -> return $ Trivial bool
    VCompareIndex _ -> unblock
    VCompareNat _ -> unblock
    VCompareRatTensor _ -> unblock
    VBoolIf args -> do
      unfolded <- unfoldIf args
      compileHardBooleanTree unfolded
    VImplies args -> do
      let noImplies = elimImplies args
      compileHardBooleanTree noImplies
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
        Right result -> compileHardBooleanTree result

andResult ::
  forall m.
  (MonadLiftQuantifiers m) =>
  UnforcedDims Builtin ->
  Thunk Builtin ->
  Thunk Builtin ->
  Lv ->
  m LeafDisjunction
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
    compileRHS :: (MonadLiftQuantifiers m) => LiftedData -> Thunk Builtin -> m LeafDisjunction
    compileRHS leftLiftedData@(leftQuantifiers, _) arg = do
      leafDisjunctionRHS <- liftQuantifiers (arg, ctxDelta + Lv (length leftQuantifiers))
      case leafDisjunctionRHS of
        NonTrivial disjuncts -> return $ NonTrivial $ fmap (constructAnd leftLiftedData) disjuncts
        Trivial True -> return $ NonTrivial $ DisjunctAll [leftLiftedData]
        Trivial False -> return $ Trivial False

    constructAnd :: LiftedData -> LiftedData -> LiftedData
    constructAnd (quantifiersLeft, leftExpr) (quantifiersRight, rightExpr) =
      ( quantifiersLeft ++ quantifiersRight,
        Forced $ mkExpr accessAndTensor (TensorOp2Args dims leftExpr rightExpr)
      )

orResult ::
  (MonadLiftQuantifiers m) =>
  Thunk Builtin ->
  Thunk Builtin ->
  Lv ->
  m LeafDisjunction
orResult arg1 arg2 ctxDelta = do
  arg1' <- liftQuantifiers (arg1, ctxDelta)
  arg2' <- liftQuantifiers (arg2, ctxDelta)
  return $ orTrivial (<>) arg1' arg2'

liftQuantifiers ::
  (MonadLiftQuantifiers m) =>
  (Thunk Builtin, Lv) ->
  m LeafDisjunction
liftQuantifiers (value, ctxDelta) = logEntryAndExit value $ do
  forcedValue <- forceThunk value
  case toBoolValue forcedValue of
    VBoolLiteral bool ->
      return $ Trivial bool
    VAnd (TensorOp2Args dims arg1 arg2) -> andResult dims arg1 arg2 ctxDelta
    VOr (TensorOp2Args _ arg1 arg2) -> orResult arg1 arg2 ctxDelta
    VNot args -> do
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
          leafDisjunction <- addNameToContext binder $ liftQuantifiers (normBody, ctxDelta)
          case leafDisjunction of
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
          leafDisjunction <- addNameToContext binder $ liftQuantifiers (normBody, ctxDelta)
          case leafDisjunction of
            NonTrivial disjuncts -> return $ NonTrivial $ fmap (lowerQuantifier quantifierData) disjuncts
            Trivial bool -> return $ Trivial bool
    VCompareIndex _ -> do
      newExpr <- updateVarLevels ctxDelta value
      return $ NonTrivial $ DisjunctAll [([], newExpr)]
    VCompareNat _ -> do
      newExpr <- updateVarLevels ctxDelta value
      return $ NonTrivial $ DisjunctAll [([], newExpr)]
    VCompareRatTensor _ -> do
      newExpr <- updateVarLevels ctxDelta value
      return $ NonTrivial $ DisjunctAll [([], newExpr)]
    VBoolIf args -> do
      unfolded <- unfoldIf args
      liftQuantifiers (unfolded, ctxDelta)
    VImplies args -> do
      let noImplies = elimImplies args
      liftQuantifiers (noImplies, ctxDelta)
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
          -- TODO: In order for this to be sound, we need to check that there are no quantifiers in value
          newExpr <- updateVarLevels ctxDelta value
          return $ NonTrivial $ DisjunctAll [([], newExpr)]
        Right result -> liftQuantifiers (result, ctxDelta)

lowerQuantifier ::
  QuantifierData ->
  LiftedData ->
  LiftedData
lowerQuantifier quantifierData (quantifiers, expr) = (quantifierData : quantifiers, expr)

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

logEntryAndExit ::
  (MonadLiftQuantifiers m) =>
  Thunk Builtin ->
  m LeafDisjunction ->
  m LeafDisjunction
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
