module Vehicle.Backend.Loss.Domain
  ( compileQuantifier,
  )
where

import Control.Monad (foldM, forM)
import Control.Monad.Except (MonadError (..), runExceptT)
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
import Vehicle.Compile.Constants.Value
import Vehicle.Compile.Error
import Vehicle.Compile.LiftIf (unfoldIf)
import Vehicle.Compile.LowerNot (lowerNot, negateRatTensorQuantifierBody)
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Unblock (unblockBoolExpr)
import Vehicle.Data.Assertion (Assertion, NormalisedRelation (..), Relation (..))
import Vehicle.Data.Bound
import Vehicle.Data.Bound.FourierMotzkinElimination (fourierMotzkinTensorBoundsElimination)
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Builtin.Loss
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.BooleanExpr (BooleanExpr (..), DisjunctAll (..), andBoolExpr, conjunctDisjunctsM, disjunctDisjuncts, disjunctsToList, elimIfTree, eliminateTrivialDisjunctions, flattenBoolExpr)
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Value
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
  (Quantifier, QuantifyRatTensorArgs (Value Builtin) (Closure Builtin)) ->
  m (Value LossBuiltin)
compileQuantifier (q, args) = do
  maybePartitions <- compileQuantifierInternal (q, args)
  case maybePartitions of
    Trivial b ->
      -- TODO add a warning
      convertBoolTensorLiteral (ZeroDimTensor b)
    NonTrivial partitions -> do
      let disjunctedPartitions = partitionsToDisjuncts partitions
      DisjunctAll (v :| vs) <- traverse checkFinalPartitionUnconstrained disjunctedPartitions
      finalValue <- foldrM orLossValue v vs
      return finalValue

checkFinalPartitionUnconstrained ::
  (MonadLogic m) =>
  Partition ->
  m (Value LossBuiltin)
checkFinalPartitionUnconstrained = \case
  (Nothing, Nothing) -> developerError "Found unexpected trivial partition"
  (Nothing, Just value) -> return value
  (Just {}, _) -> developerError "Constraints still unexpected present after compiling top-level quantifier"

compileQuantifierInternal ::
  (MonadLogic m) =>
  (Quantifier, QuantifyRatTensorArgs (Value Builtin) (Closure Builtin)) ->
  m (MaybeTrivial Partitions)
compileQuantifierInternal (q, args) = case q of
  Exists -> compileExists args
  Forall -> compileForall args

compileForall ::
  (MonadLogic m) =>
  QuantifyRatTensorArgs (Value Builtin) (Closure Builtin) ->
  m (MaybeTrivial Partitions)
compileForall args = do
  notArgs <- negateRatTensorQuantifierBody args
  maybePartitions <- compileExists notArgs
  case maybePartitions of
    Trivial b -> return $ Trivial $ not b
    NonTrivial partitions -> NonTrivial <$> notPartitions IDimNil partitions

compileExists ::
  (MonadLogic m) =>
  QuantifyRatTensorArgs (Value Builtin) (Closure Builtin) ->
  m (MaybeTrivial Partitions)
compileExists (QuantifyRatTensorArgs dims binder closure) =
  logCompilerSection2 MaxDetail "convert-exists" $ do
    -- Extract the domain for the search
    lv <- getBinderDepth
    body <- normaliseClosure binder closure
    finalCtx <- getShrunkenContext

    result <- addTensorBinderToContext dims binder $ do
      maybePartitions <- compileBool body
      case maybePartitions of
        Trivial b -> do
          -- TODO throw warning
          return $ Trivial b
        NonTrivial partitions -> do
          logDebug MaxDetail $ "number-of-partitions:" <+> pretty (numberOfPartitions partitions)
          userTensorVar <- lookupNestedTensorVariable $ UserTensorVariable $ TensorVariable $ SliceVariable lv
          xs <- traverse (compileConstraints finalCtx dims binder userTensorVar) (partitionsToDisjuncts partitions)
          disjunctMaybeTrivialPartitions xs

    return result

compileConstraints ::
  (MonadLogic m) =>
  BoundCtx () ->
  VDims Builtin ->
  VBinder Builtin ->
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
    let lossBody = quote mempty (1 + boundCtxLv finalCtx) remainingBody
    let finalEnv = boundContextToEnv finalCtx
    let remainder = Closure finalEnv lossBody

    -- Find the bounds on the quantified variable from the constraints
    let partialShape = extractPartialShape dims
    disjunctedTensorBounds <- findTensorBounds var partialShape constraints
    logDebug MaxDetail $ "number-of-constraint-partitions:" <+> pretty (length disjunctedTensorBounds)

    -- For each set of disjuncted bounds create a search expression.
    newPartitions <- forM disjunctedTensorBounds $ \(tensorBounds, remainingTree) -> do
      logCompilerSection2 MaxDetail "flattening of constraint partition" $ do
        logDebugM MaxDetail $ do
          boundsDoc <- prettyFriendlyInCtx (BoundedValue var tensorBounds)
          return $ "all-variable-bounds:" <> lineIndent boundsDoc

        errorOrDomain <- fourierMotzkinTensorBoundsElimination partialShape tensorBounds
        domain <- case errorOrDomain of
          Left err -> noQuantifierDomainError binder err
          Right domain -> return domain

        logDebugM MaxDetail $ do
          boundsDoc <- prettyFriendlyInCtx (BoundedValue var domain)
          return $ "final-domain:" <> lineIndent boundsDoc

        logDebugM MaxDetail $ do
          remDoc <- maybe (return "") (fmap lineIndent . prettyFriendlyInCtx) remainingTree
          return $ "remaining-constraints:" <> remDoc

        finalValue <- compileSearch varName dims binder remainder domain
        return $ singletonPartition (remainingTree, Just finalValue)
    NonTrivial <$> disjunctPartitions newPartitions

compileSearch ::
  (MonadLogic m) =>
  Name ->
  VDims Builtin ->
  VBinder Builtin ->
  Closure LossBuiltin ->
  Domain (DimensionedTensorValue LossBuiltin) ->
  m (Value LossBuiltin)
compileSearch varName dims binder closure (Domain lowerBound upperBound) = do
  -- Convert the binder and the dimensions.
  lossBinder <- traverse convertType binder
  lossDims <- convertDims dims

  -- Generate the operation for doing the reduction
  -- We do not know how many samples the quantifier will generate so we must append
  -- an explicit lambda that takes them and then applies them appropriately.
  -- The sample implementation will then provide them at run time.
  genericReductionOp <- getLogicField ReduceDisjunction
  let explicitDimsBinder = mkExplicitBinder (IListType INatType) (Just (mempty, "dims"))
  let explicitDimsReductionOp = Lam mempty explicitDimsBinder (normAppList genericReductionOp [implicitIrrelevant (BoundVar mempty (Ix 0))])
  reductionOp <- evalInEmptyEnv explicitDimsReductionOp

  -- Reform the predicate as if we had no tensor variables at all
  let lossPredicate = VLam lossBinder closure

  -- Create the final expression
  -- NOTE that this is unsound as we discard the strictness information.
  let spine =
        mkExpr accessSpine $
          SearchRatTensorArgs
            { searchDims = lossDims,
              searchReductionOp = reductionOp,
              searchLowerBound = tensorValue $ lowerBoundValue lowerBound,
              searchUpperBound = tensorValue $ upperBoundValue upperBound,
              searchPredicate = lossPredicate
            }
  minimise <- getLogicDirection
  return $ VBuiltin (LossBuiltinExtraFunction $ SearchRatTensor varName minimise) spine

findTensorBounds ::
  forall m.
  (MonadLogic m) =>
  NestedSliceVariable ->
  PartiallyKnownTensorShape ->
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
      let (coef, expr') = rearrangeExprToSolveFor (toSliceVar var) expr
      let boundExpr = tensorValueLinearExprToValue expr'
      let bounds = convertToTensorBounds parentShape indices rel coef boundExpr
      return $ Just bounds

noQuantifierDomainError ::
  (MonadLogic m) =>
  VBinder Builtin ->
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

orLossValue :: (MonadDomain m) => Value LossBuiltin -> Value LossBuiltin -> m (Value LossBuiltin)
orLossValue e1 e2 = convertOr (TensorOp2Args IDimNil e1 e2)

andLossValue :: (MonadDomain m) => Value LossBuiltin -> Value LossBuiltin -> m (Value LossBuiltin)
andLossValue e1 e2 = convertAnd (TensorOp2Args IDimNil e1 e2)

notConstraint :: (MonadDomain m) => UserVariableConstraint LossBuiltin -> m (BooleanExpr (UserVariableConstraint LossBuiltin))
notConstraint (NormalisedRelation rel expr) = do
  let negExpr = scaleExpr (-1) expr
  return $ case rel of
    OLe -> Query $ NormalisedRelation OLt negExpr
    OLt -> Query $ NormalisedRelation OLe negExpr
    OEq -> do
      let less = NormalisedRelation OLe expr
      let greater = NormalisedRelation OLe negExpr
      Disjunct $ DisjunctAll [Query less, Query greater]

-- | Note that the constraints live in the extended tensor context, where as the remaining value lives
-- in the original unextended context.
type Partition = (Maybe UserVariableConstraintTree, Maybe (Value LossBuiltin))

notPartition :: (MonadDomain m) => VDims LossBuiltin -> Partition -> m Partition
notPartition dims (constraintTree, value) = do
  notConstraintTree <- traverse (traverse notConstraint) constraintTree
  notValue <- traverse (convertNot . TensorOp1Args dims) value
  return (fmap flattenBoolExpr notConstraintTree, notValue)

andPartition :: (MonadDomain m) => Partition -> Partition -> m Partition
andPartition (c1, v1) (c2, v2) = do
  let c = unionMaybeWith andBoolExpr c1 c2
  v <- unionMaybeWithM andLossValue v1 v2
  return (c, v)

-- | Note that the constraints live in the extended tensor context, where as the remaining value lives
-- in the original unextended context.
type Partitions = Map (Maybe UserVariableConstraintTree) (Maybe (Value LossBuiltin))

singletonUnconstrainedPartition :: (MonadDomain m) => Value Builtin -> m (MaybeTrivial Partitions)
singletonUnconstrainedPartition nonDomainConstraint = do
  logDebugM MaxDetail $ do
    doc <- prettyFriendlyInCtx nonDomainConstraint
    return $ "Found non-domain constraint:" <+> doc
  lossNonDomainConstraint <- logCompilerSection2 MaxDetail "converting constant to loss builtins" $ do
    convertBoolTensor nonDomainConstraint
  return $ NonTrivial $ Map.singleton Nothing (Just lossNonDomainConstraint)

singletonPartition :: Partition -> Partitions
singletonPartition (tree, value) = Map.singleton tree value

numberOfPartitions :: Partitions -> Int
numberOfPartitions = length

containsConstraints :: Partitions -> Bool
containsConstraints partitions = case Map.toList partitions of
  [(Nothing, _)] -> False
  _ -> True

notPartitions :: (MonadDomain m) => VDims Builtin -> Partitions -> m Partitions
notPartitions dims partitions = do
  -- Negate each individual partition
  lossDims <- convertDims dims
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

compileBool :: (MonadDomain m) => Value Builtin -> m (MaybeTrivial Partitions)
compileBool value = logEntryAndExit value $ case toBoolValue value of
  -----------------------
  -- Useful base cases --
  -----------------------
  VBoolCompareRatPointwise (op, args) -> compileComparison op (Left args)
  VBoolCompareRatReduced (op, args) -> compileComparison op (Right args)
  --------------------------
  -- Un-useful base cases --
  --------------------------
  VBoolLiteral b -> return $ Trivial b
  VCompareNat {} -> unsupportedOperation "CompareNat"
  VCompareIndex {} -> unsupportedOperation "CompareIndex"
  ---------------------
  -- Recursive cases --
  ---------------------
  VAnd args -> compileAnd args
  VOr args -> compileOr args
  VBoolIf args -> compileBool =<< unfoldIf args
  VNot args -> compileBool =<< lowerNot args
  VQuantifyRatTensor args -> compileQuantifierInternal args
  -- TODO: RECORD SUPPORT
  VQuantifyRecord _args -> compilerDeveloperError "Non top-level record quantifiers are not supported yet"
  -------------------
  -- Blocked cases --
  -------------------
  VReduceAndTensor {} -> unblockBoolValue value
  VReduceOrTensor {} -> unblockBoolValue value
  VBoolAt {} -> unblockBoolValue value

compileAnd ::
  (MonadDomain m) =>
  TensorOp2Args (Value Builtin) ->
  m (MaybeTrivial Partitions)
compileAnd (TensorOp2Args _ e1 e2) = do
  c1 <- compileBool e1
  c2 <- compileBool e2
  andTrivialM andPartitions c1 c2

compileOr ::
  (MonadDomain m) =>
  TensorOp2Args (Value Builtin) ->
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
  ComparisonOp ->
  Either (TensorOp2Args (Value Builtin)) (TensorReduceComparisonArgs (Value Builtin)) ->
  m (MaybeTrivial Partitions)
compileComparison op args = do
  logCompilerSection2 MaxDetail "assertion compilation" $ do
    if op == Ne
      then singletonUnconstrainedPartition $ case args of
        Left ptArgs -> mkPointwiseCompare op ptArgs
        Right rdArgs -> mkReducedCompare op rdArgs
      else do
        blockedValueOrResult <- tryPurifyAssertion op args
        elimIfTree compileBranch compileLeaf blockedValueOrResult
  where
    compileBranch ::
      Value Builtin ->
      MaybeTrivial Partitions ->
      MaybeTrivial Partitions ->
      m (MaybeTrivial Partitions)
    compileBranch c x y = do
      c' <- compileBool c
      notC' <- compileBool (fromBoolValue $ VNot $ TensorOp1Args IDimNil c)
      cAndx <- andTrivialM andPartitions c' x
      notCAndy <- andTrivialM andPartitions notC' y
      orTrivialM orPartitions cAndx notCAndy

    compileLeaf ::
      (Value Builtin, Maybe (MaybeTrivial (Assertion (TensorValueLinearExpr Builtin)))) ->
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
              lossConstant <- convertRatTensor tensorValue
              lossDims <- convertDims dims
              return $ TensorValue lossDims lossConstant

            let lossAssertion = NormalisedRelation rel (Sparse coeffs lossConstant)
            let partitions = Map.singleton (Just (Query lossAssertion)) Nothing
            logDebugM MaxDetail $ do
              doc <- prettyFriendlyInCtx lossAssertion
              return $ "Found domain constraint:" <+> doc
            return $ NonTrivial partitions

-- | Unblocking a boolean value is a little complicated.
unblockBoolValue ::
  forall m.
  (MonadDomain m) =>
  Value Builtin ->
  m (MaybeTrivial Partitions)
unblockBoolValue value = do
  callDepth <- getCallDepth
  blockedOrUnblockedExpr <- runExceptT $ unblockBoolExpr unblockingActions value
  case blockedOrUnblockedExpr of
    -- If we cannot unblock it return an unconstrained partition
    Left _blockingExpr -> do
      setCallDepth callDepth
      singletonUnconstrainedPartition value
    Right unblockedExpr -> do
      -- If we can unblock it then try to continue compilation
      maybePartitions <- compileBool unblockedExpr
      case maybePartitions of
        NonTrivial partitions | not (containsConstraints partitions) -> singletonUnconstrainedPartition value
        _ -> return maybePartitions

--------------------------------------------------------------------------------
-- Utils

logEntryAndExit ::
  (MonadDomain m) =>
  Value Builtin ->
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
