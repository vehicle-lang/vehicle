module Vehicle.Backend.Loss.Domain
  ( compileQuantifier,
    orLossValue,
  )
where

import Control.Monad (foldM, forM)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.Reader (MonadReader)
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (foldrM)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Vehicle.Backend.Loss.Core
import Vehicle.Backend.Loss.Domain.PurifyAssertion (tryPurifyAssertion, unblockingActions)
import Vehicle.Backend.Loss.LossCompilation
import Vehicle.Backend.Solver.UserVariableElimination.ConstraintSearch (findAllBounds)
import Vehicle.Compile.Constants.ForcedValue
import Vehicle.Compile.Error
import Vehicle.Compile.LiftIf (unfoldIf)
import Vehicle.Compile.LowerNot (lowerNot, negateQuantifierBody)
import Vehicle.Compile.Normalise.Builtin (elimImplies)
import Vehicle.Compile.Normalise.Force
import Vehicle.Compile.Normalise.Quote (unnormalise)
import Vehicle.Compile.Normalise.RewriteRules (forceAndRewriteTensor)
import Vehicle.Compile.Normalise.TypedValue
import Vehicle.Compile.Prelude
import Vehicle.Compile.Unblock (unblockBoolExpr)
import Vehicle.Data.Assertion (Assertion, NormalisedRelation (..), Relation (..))
import Vehicle.Data.Bound
import Vehicle.Data.Bound.FourierMotzkinElimination (fourierMotzkinTensorBoundsElimination)
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Builtin.Loss
import Vehicle.Data.Builtin.Loss qualified as L
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.BooleanExpr (BooleanExpr (..), DisjunctAll (..), andBoolExpr, conjunctDisjunctsM, disjunctDisjuncts, disjunctsToList, elimIfTree, eliminateTrivialDisjunctions, flattenBoolExpr)
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.DifferentiableLogic (TensorDifferentiableLogicField (..))
import Vehicle.Data.MaybeTrivial
import Vehicle.Data.Tensor (pattern ZeroDimTensor)
import Vehicle.Data.Tensor.Traversal
import Vehicle.Data.Variable.Bound.Context.Generic (BoundCtx)
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Data.Variable.Bound.Context.Tensor
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Data.Variable.Free.Context (MonadFreeContext)
import Vehicle.Prelude.Warning (CompileWarning (..))

compileQuantifier ::
  (MonadLogic m) =>
  (Quantifier, QuantifyRatTensorArgs (Thunk Builtin) (Closure Builtin)) ->
  m (DisjunctAll (Thunk LossBuiltin))
compileQuantifier (q, args) = do
  maybePartitions <- compileQuantifierInternal (q, args)
  case maybePartitions of
    Trivial b -> do
      -- TODO add a warning
      value <- Forced <$> convertBoolTensorLiteral (ZeroDimTensor b)
      return $ DisjunctAll [value]
    NonTrivial partitions -> do
      let disjunctedPartitions = partitionsToDisjuncts partitions
      traverse checkFinalPartitionUnconstrained disjunctedPartitions

checkFinalPartitionUnconstrained ::
  (MonadLogic m) =>
  Partition ->
  m (Thunk LossBuiltin)
checkFinalPartitionUnconstrained = \case
  (Nothing, Nothing) -> developerError "Found unexpected trivial partition"
  (Nothing, Just value) -> return value
  (Just {}, _) -> developerError "Constraints still unexpected present after compiling top-level quantifier"

compileQuantifierInternal ::
  (MonadLogic m) =>
  (Quantifier, QuantifyRatTensorArgs (Thunk Builtin) (Closure Builtin)) ->
  m (MaybeTrivial Partitions)
compileQuantifierInternal (q, args) = case q of
  Exists -> compileExists args
  Forall -> compileForall args

compileForall ::
  (MonadLogic m) =>
  QuantifyRatTensorArgs (Thunk Builtin) (Closure Builtin) ->
  m (MaybeTrivial Partitions)
compileForall args = do
  let notArgs = negateQuantifierBody args
  maybePartitions <- compileExists notArgs
  case maybePartitions of
    Trivial b -> return $ Trivial $ not b
    NonTrivial partitions -> NonTrivial <$> notPartitions (Forced IDimNil) partitions

compileExists ::
  (MonadLogic m) =>
  QuantifyRatTensorArgs (Thunk Builtin) (Closure Builtin) ->
  m (MaybeTrivial Partitions)
compileExists (QuantifyRatTensorArgs _pDims bDims binder closure) =
  logCompilerSection2 MaxDetail "convert-exists" $ do
    -- Extract the domain for the search
    lv <- getBinderDepth
    let body = extendClosureWithBound closure binder lv
    finalCtx <- getShrunkenContext

    shapePrefix <- extractKnownShapePrefix bDims
    result <- addTensorBinderToContextLocally shapePrefix binder $ do
      maybePartitions <- compileBool body
      case maybePartitions of
        Trivial b -> do
          -- TODO throw warning
          return $ Trivial b
        NonTrivial partitions -> do
          logDebug MaxDetail $ "number-of-partitions:" <+> pretty (numberOfPartitions partitions)
          userTensorVar <- lookupNestedTensorVariable $ UserTensorVariable $ TensorVariable $ SliceVariable lv
          xs <- traverse (compileConstraints finalCtx bDims binder userTensorVar) (partitionsToDisjuncts partitions)
          disjunctMaybeTrivialPartitions xs

    return result

compileConstraints ::
  (MonadLogic m) =>
  BoundCtx () ->
  UnforcedDims Builtin ->
  UnforcedBinder Builtin ->
  NestedSliceVariable ->
  Partition ->
  m (MaybeTrivial Partitions)
compileConstraints finalCtx dims binder var (maybeConstraints, maybeRemainder) = do
  let (varName, _) = getNamedBinderInfo binder
  logCompilerSection2 MidDetail ("extracting bounds for" <+> quotePretty varName <+> "from partition") $ do
    -- Extract the constraints we can use to bound the variable
    constraints <- case maybeConstraints of
      Nothing -> noQuantifierDomainError binder wholeTensorUnbounded
      Just constraints -> do
        logDebugM MidDetail $ do
          boundsDoc <- prettyFriendlyInCtx constraints
          return $
            "all-constraints:"
              <> lineIndent boundsDoc
        return constraints

    -- Extract the remaining body of the quantifier
    remainingBody <- case maybeRemainder of
      Just remainder -> return remainder
      Nothing -> do
        (ident, _p) <- getDeclProvenance
        logWarning $ BoundsOnlyQuantifier (nameOf ident) varName
        getLogicFieldValue TruthityElement

    logDebugM MidDetail $ do
      remainderDoc <- prettyFriendlyInCtx remainingBody
      return $
        "remaining-expression:"
          <> lineIndent remainderDoc

    -- Reform the closure around the body. Note that this needs to be done
    -- in the final context (i.e. without any reference to slice variables!)
    let lossBody = unnormalise (1 + boundCtxLv finalCtx) remainingBody
    let finalEnv = boundContextToEnv finalCtx
    let remainder = Closure finalEnv lossBody

    -- Find the bounds on the quantified variable from the constraints
    partialShape <- extractKnownShapePrefix dims
    disjunctedTensorBounds <- findTensorBounds var partialShape constraints
    logDebug MaxDetail $ "number-of-constraint-partitions:" <+> pretty (length disjunctedTensorBounds)

    -- For each set of disjuncted bounds create a search expression.
    newPartitions <- forM disjunctedTensorBounds $ \(tensorBounds, remainingTree) -> do
      logCompilerSection2 MaxDetail "flattening of constraint partition" $ do
        logDebugM MaxDetail $ do
          boundsDoc <- prettyFriendlyInCtx (BoundedValue var tensorBounds)
          return $ "all-variable-bounds:" <> lineIndent boundsDoc

        errorOrDomain <- fourierMotzkinTensorBoundsElimination tensorBounds
        domain <- case errorOrDomain of
          Left err -> noQuantifierDomainError binder err
          Right domain -> return domain

        logDebugM MaxDetail $ do
          boundsDoc <- prettyFriendlyInCtx (BoundedValue var domain)
          return $ "final-domain:" <> lineIndent boundsDoc

        logDebugM MaxDetail $ do
          remDoc <- maybe (return "") (fmap lineIndent . prettyFriendlyInCtx) remainingTree
          return $ "remaining-constraints:" <> remDoc

        finalValue <- compileSearch dims binder remainder domain
        return $ singletonPartition (remainingTree, Just finalValue)
    NonTrivial <$> disjunctPartitions newPartitions

compileSearch ::
  (MonadLogic m) =>
  Thunk Builtin ->
  UnforcedBinder Builtin ->
  Closure LossBuiltin ->
  Domain (DimensionedTensorValue LossBuiltin) ->
  m (Thunk LossBuiltin)
compileSearch dims binder closure (Domain lowerBound upperBound) = do
  -- Convert the binder and the dimensions.
  lossBinder <- traverse convertQuantifierlessExprToLoss binder
  lossDims <- convertQuantifierlessExprToLoss dims

  -- Reform the predicate as if we had no tensor variables at all
  let lossPredicate = Forced $ VLam lossBinder closure

  -- Create the final expression
  -- NOTE that this is unsound as we discard the strictness information.
  let spine =
        mkExpr accessSpine $
          SearchRatTensorArgs
            { searchDims = lossDims,
              searchLowerBound = tensorValue $ lowerBoundValue lowerBound,
              searchUpperBound = tensorValue $ upperBoundValue upperBound,
              searchPredicate = lossPredicate
            }
  return $ Forced $ VBuiltin (LossBuiltinFunction $ L.SearchRatTensor) spine

findTensorBounds ::
  forall m.
  (MonadLogic m) =>
  NestedSliceVariable ->
  KnownPrefixOfTensorShape ->
  UserVariableConstraintTree ->
  m (DisjunctAll (TensorBounds (DimensionedTensorValue LossBuiltin), Maybe UserVariableConstraintTree))
findTensorBounds parentVar parentVarShape constraints =
  go (DisjunctAll [(emptyBounds, Just constraints)]) parentVar
  where
    go ::
      DisjunctAll (TensorBounds (DimensionedTensorValue LossBuiltin), Maybe UserVariableConstraintTree) ->
      NestedSliceVariable ->
      m (DisjunctAll (TensorBounds (DimensionedTensorValue LossBuiltin), Maybe UserVariableConstraintTree))
    go allBounds var = do
      result <- forM allBounds $ \(bounds, maybeTree) ->
        case maybeTree of
          Nothing ->
            return $ DisjunctAll [(bounds, maybeTree)]
          Just tree -> do
            let tensorVar = TensorVariable $ toSliceVar parentVar
            let indices = findSliceIndices parentVar var
            let varInfo = VariableInfo tensorVar parentVarShape indices
            disjunctedBoundsAndRemainders <- findAllBounds (findVarBound var varInfo) tree
            let finalBoundsAndRemainders = fmap (first (andBounds bounds)) disjunctedBoundsAndRemainders
            case childVariablesOf var of
              Nothing -> return finalBoundsAndRemainders
              Just childVariables -> foldM go finalBoundsAndRemainders childVariables
      return $ disjunctDisjuncts result

findVarBound ::
  (MonadLogic m) =>
  NestedSliceVariable ->
  VariableInfo ->
  UserVariableConstraint LossBuiltin ->
  m (Maybe (TensorBounds (DimensionedTensorValue LossBuiltin)))
findVarBound var VariableInfo {..} (NormalisedRelation rel expr)
  | not (expr `containsVariable` toSliceVar var) = return Nothing
  | otherwise = do
      (coef, expr') <- rearrangeExprToSolveFor (toSliceVar var) expr
      boundExpr <- tensorValueLinearExprToValue expr'
      bounds <- convertToTensorBounds parentShape indices rel coef boundExpr
      return $ Just bounds

noQuantifierDomainError ::
  (MonadLogic m) =>
  UnforcedBinder Builtin ->
  UnboundedIndices ->
  m a
noQuantifierDomainError binder missingIndices = do
  propertyProv <- getDeclProvenance
  throwError $ NoQuantifierDomainFound propertyProv binder missingIndices

--------------------------------------------------------------------------------
-- Constraint search
--------------------------------------------------------------------------------
-- Definitions

type MonadDomain m =
  ( MonadCompile m,
    MonadReader LossCtx m,
    MonadFreeContext Builtin m,
    MonadFreeContext LossBuiltin m,
    MonadTensorBoundContext m
  )

notConstraint :: (MonadDomain m) => UserVariableConstraint LossBuiltin -> m (BooleanExpr (UserVariableConstraint LossBuiltin))
notConstraint (NormalisedRelation rel expr) = do
  negExpr <- scaleExpr (-1) expr
  return $ case rel of
    OLe -> Query $ NormalisedRelation OLt negExpr
    OLt -> Query $ NormalisedRelation OLe negExpr
    OEq -> do
      let less = NormalisedRelation OLe expr
      let greater = NormalisedRelation OLe negExpr
      Disjunct $ DisjunctAll [Query less, Query greater]

-- | Note that the constraints live in the extended tensor context, where as the remaining value lives
-- in the original unextended context.
type Partition = (Maybe UserVariableConstraintTree, Maybe (Thunk LossBuiltin))

notPartition :: (MonadDomain m) => Thunk LossBuiltin -> Partition -> m Partition
notPartition dims (constraintTree, value) = do
  notConstraintTree <- traverse (traverse notConstraint) constraintTree
  notValue <- traverse (notLossValue dims) value
  return (fmap flattenBoolExpr notConstraintTree, notValue)

andPartition :: (MonadDomain m) => Partition -> Partition -> m Partition
andPartition (c1, v1) (c2, v2) = do
  let c = unionMaybeWith andBoolExpr c1 c2
  v <- unionMaybeWithM andLossValue v1 v2
  return (c, v)

-- | Note that the constraints live in the extended tensor context, where as the remaining value lives
-- in the original unextended context.
type Partitions = Map (Maybe UserVariableConstraintTree) (Maybe (Thunk LossBuiltin))

singletonUnconstrainedPartition :: (MonadDomain m) => Thunk Builtin -> m (MaybeTrivial Partitions)
singletonUnconstrainedPartition nonDomainConstraint = do
  logDebugM MaxDetail $ do
    doc <- prettyFriendlyInCtx nonDomainConstraint
    return $ "Found non-domain constraint:" <+> doc
  lossNonDomainConstraint <- logCompilerSection2 MaxDetail "converting constraint to loss builtins" $ do
    convertQuantifierlessExprToLoss nonDomainConstraint
  return $ NonTrivial $ Map.singleton Nothing (Just lossNonDomainConstraint)

singletonPartition :: Partition -> Partitions
singletonPartition (tree, value) = Map.singleton tree value

numberOfPartitions :: Partitions -> Int
numberOfPartitions = length

containsConstraints :: Partitions -> Bool
containsConstraints partitions = case Map.toList partitions of
  [(Nothing, _)] -> False
  _ -> True

notPartitions :: (MonadDomain m) => Thunk Builtin -> Partitions -> m Partitions
notPartitions dims partitions = do
  -- Negate each individual partition
  lossDims <- convertQuantifierlessExprToLoss dims
  let disjuncts = partitionsToDisjuncts partitions
  DisjunctAll (p :| ps) <- traverse (notPartition lossDims) disjuncts

  -- Conjunct the results together
  result <- foldrM andPartition p ps
  return $ singletonPartition result

partitionsToDisjuncts :: Partitions -> DisjunctAll Partition
partitionsToDisjuncts ps = case Map.toList ps of
  [] -> developerError "Empty partition"
  x : xs -> DisjunctAll $ x :| xs

disjunctPartitions :: (MonadDomain m) => DisjunctAll Partitions -> m Partitions
disjunctPartitions (DisjunctAll (p :| ps)) = foldrM orPartitions p ps

disjunctMaybeTrivialPartitions :: (MonadDomain m) => DisjunctAll (MaybeTrivial Partitions) -> m (MaybeTrivial Partitions)
disjunctMaybeTrivialPartitions = traverse disjunctPartitions . eliminateTrivialDisjunctions

orPartitions :: (MonadDomain m) => Partitions -> Partitions -> m Partitions
orPartitions p1 p2 = do
  unionWithM (unionMaybeWithM orLossValue) p1 p2

andPartitions :: (MonadDomain m) => Partitions -> Partitions -> m Partitions
andPartitions p1 p2 = do
  disjuncts <- conjunctDisjunctsM andPartition (partitionsToDisjuncts p1) (partitionsToDisjuncts p2)
  return $ Map.fromList $ disjunctsToList disjuncts

--------------------------------------------------------------------------------
-- Search algorithm

compileBool :: (MonadDomain m) => Thunk Builtin -> m (MaybeTrivial Partitions)
compileBool value = logEntryAndExit value $ do
  forcedValue <- forceAndRewriteTensor value
  case toBoolValue forcedValue of
    -----------------------
    -- Useful base cases --
    -----------------------
    VCompareRatTensor args -> compileComparison args
    --------------------------
    -- Un-useful base cases --
    --------------------------
    VBoolLiteral b -> return $ Trivial b
    VCompareNat {} -> unsupportedOperation "CompareNat"
    VCompareIndex {} -> unsupportedOperation "CompareIndex"
    ---------------------
    -- Recursive cases --
    ---------------------
    VImplies args -> compileBool $ elimImplies args
    VAnd args -> compileAnd args
    VOr args -> compileOr args
    VQuantifyRatTensor args -> compileQuantifierInternal args
    VQuantifyRecord _args -> compilerDeveloperError "Non top-level record quantifiers are not supported yet"
    -------------------
    -- Blocked cases --
    -------------------
    VReduceAndTensor {} -> unblock forcedValue
    VReduceOrTensor {} -> unblock forcedValue
    VBoolTensorAt {} -> unblock forcedValue
    VBoolVectorAt {} -> unblock forcedValue
    VBoolFoldList {} -> unblock forcedValue
    VBoolIf args -> compileBool =<< unfoldIf args
    VNot (TensorOp1Args dims xs) -> unblockWith (lowerNot unblockingActions $ TensorOp1Args dims xs) (Forced forcedValue)
  where
    unblock forced = unblockWith (unblockBoolExpr unblockingActions (Forced forced)) (Forced forced)

compileAnd ::
  (MonadDomain m) =>
  TensorOp2Args (Thunk Builtin) ->
  m (MaybeTrivial Partitions)
compileAnd (TensorOp2Args _ e1 e2) = do
  c1 <- compileBool e1
  c2 <- compileBool e2
  andTrivialM andPartitions c1 c2

compileOr ::
  (MonadDomain m) =>
  TensorOp2Args (Thunk Builtin) ->
  m (MaybeTrivial Partitions)
compileOr (TensorOp2Args _ e1 e2) = do
  c1 <- compileBool e1
  c2 <- compileBool e2
  orTrivialM orPartitions c1 c2

-- | A comparison may be compiled to a potential bound as long as:
-- * It does not contain any network applications (e.g. f x < 0.5)
-- * It does not compare slices from the same user tensor (e.g. x ! 0 < x ! 1)
compileComparison ::
  forall m.
  (MonadDomain m) =>
  (ComparisonOp, TensorComparisonArgs (Thunk Builtin)) ->
  m (MaybeTrivial Partitions)
compileComparison (op, args) = do
  logCompilerSection2 MaxDetail "assertion compilation" $ do
    if op == Ne
      then singletonUnconstrainedPartition $ Forced (mkExpr accessCompareRatTensor (op, args))
      else do
        blockedValueOrResult <- tryPurifyAssertion op args
        elimIfTree compileBranch compileLeaf blockedValueOrResult
  where
    compileBranch ::
      Thunk Builtin ->
      MaybeTrivial Partitions ->
      MaybeTrivial Partitions ->
      m (MaybeTrivial Partitions)
    compileBranch c x y = do
      c' <- compileBool c
      notC' <- compileBool (Forced <$> mkExpr accessNotTensor $ TensorOp1Args (Forced IDimNil) c)
      cAndx <- andTrivialM andPartitions c' x
      notCAndy <- andTrivialM andPartitions notC' y
      orTrivialM orPartitions cAndx notCAndy

    compileLeaf ::
      (Thunk Builtin, Maybe (MaybeTrivial (Assertion (TensorValueLinearExpr Builtin)))) ->
      m (MaybeTrivial Partitions)
    compileLeaf (value, maybeEquivAssertion) = case maybeEquivAssertion of
      Nothing -> singletonUnconstrainedPartition value
      Just (Trivial b) -> return $ Trivial b
      Just (NonTrivial assertion) -> do
        let (NormalisedRelation rel (Sparse coeffs constant)) = assertion
        if Map.null coeffs
          then singletonUnconstrainedPartition value
          else do
            lossConstant <- logCompilerSection2 MaxDetail "converting constant to loss builtins" $ do
              let TensorValue dims tensorValue = constant
              lossConstant <- convertQuantifierlessExprToLoss tensorValue
              lossDims <- convertQuantifierlessExprToLoss dims
              return $ TensorValue lossDims lossConstant

            let lossAssertion = NormalisedRelation rel (Sparse coeffs lossConstant)
            let partitions = Map.singleton (Just (Query lossAssertion)) Nothing
            logDebugM MaxDetail $ do
              doc <- prettyFriendlyInCtx lossAssertion
              return $ "Found domain constraint:" <+> doc
            return $ NonTrivial partitions

-- | Unblocking a boolean value is a little complicated.
unblockWith ::
  forall m.
  (MonadDomain m) =>
  ExceptT BlockingReason m (Thunk Builtin) ->
  Thunk Builtin ->
  m (MaybeTrivial Partitions)
unblockWith action defaultValue = do
  callDepth <- getCallDepth
  blockedOrUnblockedExpr <- runExceptT action
  case blockedOrUnblockedExpr of
    -- If we cannot unblock it return an unconstrained partition
    Left _blockingExpr -> do
      setCallDepth callDepth
      singletonUnconstrainedPartition defaultValue
    Right unblockedExpr -> do
      -- If we can unblock it then try to continue compilation
      maybePartitions <- compileBool unblockedExpr
      case maybePartitions of
        NonTrivial partitions | not (containsConstraints partitions) -> singletonUnconstrainedPartition defaultValue
        _ -> return maybePartitions

--------------------------------------------------------------------------------
-- Utils

extractKnownShapePrefix ::
  forall m.
  (MonadDomain m) =>
  Thunk Builtin ->
  m KnownPrefixOfTensorShape
extractKnownShapePrefix value = do
  forcedValue <- forceThunk value
  case forcedValue of
    IDimCons d ds -> do
      forcedDim <- forceThunk d
      case forcedDim of
        INatLiteral n -> (n :) <$> extractKnownShapePrefix ds
        _ -> return []
    _ -> return []

logEntryAndExit ::
  (MonadDomain m) =>
  Thunk Builtin ->
  m (MaybeTrivial Partitions) ->
  m (MaybeTrivial Partitions)
logEntryAndExit start action = do
  logDebugM MaxDetail $ do
    doc <- prettyFriendlyInCtx start
    return $ "search-enter:" <+> doc
  incrCallDepth
  result <- action
  decrCallDepth
  logDebugM MaxDetail $ do
    doc <- prettyFriendlyInCtx result
    return $ "search-exit:" <+> lineIndent doc
  return result
