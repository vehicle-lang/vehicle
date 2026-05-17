module Vehicle.Backend.Loss.Domain
  ( compileQuantifier,
    convertBoolTensor,
  )
where

import Control.Monad (foldM, forM)
import Control.Monad.Except (MonadError (..), runExceptT)
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (foldrM)
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Vehicle.Backend.Loss.Core
import Vehicle.Backend.Loss.LossCompilation
import Vehicle.Backend.Solver.UserVariableElimination.ConstraintSearch (findAllBounds)
import Vehicle.Compile.Constants.Value
import Vehicle.Compile.Error
import Vehicle.Compile.LiftIf (unfoldIf)
import Vehicle.Compile.LowerNot (lowerNot, negateQuantifierBody)
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Unblock (MonadPurify, UnblockingActions (..), maybeUnblockBoolExpr, tryPurifyAssertion)
import Vehicle.Data.Assertion (NormalisedRelation (..), Relation (..), comparisonToAssertion)
import Vehicle.Data.Bound
import Vehicle.Data.Bound.FourierMotzkinElimination (fourierMotzkinTensorBoundsElimination)
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Builtin.Interface.Normalise (evalConstTensor, evalDivRatTensor, evalMulRatTensor)
import Vehicle.Data.Builtin.Loss
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.BooleanExpr (BooleanExpr (..), DisjunctAll (..), andBoolExpr, conjunctDisjunctsM, disjunctDisjuncts, disjunctsToList, eliminateTrivialDisjunctions, flattenBoolExpr)
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Value
import Vehicle.Data.DifferentiableLogic (TensorDifferentiableLogicField (..))
import Vehicle.Data.MaybeTrivial
import Vehicle.Data.Tensor (Tensor (ConstantTensor), pattern ZeroDimTensor)
import Vehicle.Data.Tensor.Traversal
import Vehicle.Data.Variable.Bound.Context.Generic (BoundCtx)
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Data.Variable.Bound.Context.Tensor
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Prelude.Warning (CompileWarning (..))

-- | Dispatch for bool-valued tensor subexpressions.  Unlike
-- `convertTensorProperty` in `Vehicle.Backend.Loss`, this runs on bool
-- expressions that appear *nested* under other operators (most commonly
-- temporal operators and `foreach`).  It lives here rather than in
-- `LossCompilation` so that nested `VBoolTensorQuantifyRat` nodes can be
-- dispatched to `compileQuantifier` without introducing a module cycle.
convertBoolTensor :: (MonadLogic m) => Value Builtin -> m (Value LossBuiltin)
convertBoolTensor value = logConversion value $ case toBoolTensorValue value of
  VBoolTensorLiteral bs -> convertBoolTensorLiteral bs
  VBoolConstTensor args -> convertConstTensor convertBoolTensor args
  VBoolStackTensor args -> convertStackTensor convertBoolTensor args
  VBoolTensorNot args -> convertNot =<< convertTensorOp1 convertBoolTensor args
  VBoolTensorAnd args -> convertAnd =<< convertTensorOp2 convertBoolTensor args
  VBoolTensorOr args -> convertOr =<< convertTensorOp2 convertBoolTensor args
  VBoolTensorGlobally args -> convertGlobally =<< convertTemporalOp1 convertBoolTensor args
  VBoolTensorFinally args -> convertFinally =<< convertTemporalOp1 convertBoolTensor args
  VBoolTensorUntil args -> convertUntil =<< convertTemporalOp2 convertBoolTensor args
  VBoolTensorCompareIndex args -> convertIndexComparison args
  VBoolTensorCompareNat args -> convertNatComparison args
  VBoolTensorCompareRatPointwise args -> convertRatTensorPointwiseComparison args
  VBoolTensorCompareRatReduced args -> convertRatTensorReducedComparison args
  VBoolTensorReduceAnd args -> convertReduceAnd =<< convertTensorReduction convertBoolTensor args
  VBoolTensorReduceOr args -> convertReduceOr =<< convertTensorReduction convertBoolTensor args
  VBoolTensorQuantifyRat args -> compileQuantifier args
  VBoolTensorBoolIf args -> convertIf args
  VBoolTensorAt args -> convertAtTensor convertBoolTensor args
  VBoolTensorForeach args -> convertForeachTensor convertBoolTensor args

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
      logDebugM MaxDetail $ prettyFriendlyInCtx finalValue
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
compileForall args@(QuantifyRatTensorArgs dims _ _) = do
  notArgs <- negateQuantifierBody args
  maybePartitions <- compileExists notArgs
  case maybePartitions of
    Trivial b -> return $ Trivial $ not b
    NonTrivial partitions -> NonTrivial <$> notPartitions dims partitions

-- | `globally(P and B) == globally(P) and B` (and similarly for `finally`/
-- `until`) when `B` is constant in time. Lifting `B` out of the temporal
-- envelope exposes its bounds to the Fourier-Motzkin extractor in
-- `compileExists`.
hoistTimeIndependentBounds :: Value Builtin -> Value Builtin
hoistTimeIndependentBounds = goScalar
  where
    goScalar :: Value Builtin -> Value Builtin
    goScalar e = case toBoolValue e of
      VAnd (TensorOp2Args ds e1 e2) ->
        mkExpr accessAndTensor (TensorOp2Args ds (goScalar e1) (goScalar e2))
      VOr (TensorOp2Args ds e1 e2) ->
        mkExpr accessOrTensor (TensorOp2Args ds (goScalar e1) (goScalar e2))
      VNot (TensorOp1Args ds e1) ->
        mkExpr accessNotTensor (TensorOp1Args ds (goScalar e1))
      VBoolAt (AtTensorArgs t d ds tensor i) ->
        mkExpr accessAtTensor (AtTensorArgs t d ds (goTensor tensor) i)
      _ -> e

    goTensor :: Value Builtin -> Value Builtin
    goTensor e = case toBoolTensorValue e of
      VBoolTensorGlobally args -> hoistTemporal1 accessTemporalGlobally args
      VBoolTensorFinally args -> hoistTemporal1 accessTemporalFinally args
      VBoolTensorUntil args -> hoistTemporal2 args
      VBoolTensorAnd (TensorOp2Args ds e1 e2) ->
        mkExpr accessAndTensor (TensorOp2Args ds (goTensor e1) (goTensor e2))
      VBoolTensorOr (TensorOp2Args ds e1 e2) ->
        mkExpr accessOrTensor (TensorOp2Args ds (goTensor e1) (goTensor e2))
      VBoolTensorNot (TensorOp1Args ds e1) ->
        mkExpr accessNotTensor (TensorOp1Args ds (goTensor e1))
      _ -> e

    hoistTemporal1 ::
      Accessor (Value Builtin) (TemporalOp1Args (Value Builtin)) ->
      TemporalOp1Args (Value Builtin) ->
      Value Builtin
    hoistTemporal1 acc (TemporalOp1Args dims a b body) =
      let body' = goTensor body
          (timeDep, timeInv) = partition (not . isTimeInvariant) (splitConj body')
       in case (timeDep, timeInv) of
            (_, []) -> mkExpr acc (TemporalOp1Args dims a b body')
            ([], invs) -> conjunctTensors dims invs
            (deps, invs) ->
              let newTemporal = mkExpr acc (TemporalOp1Args dims a b (conjunctTensors dims deps))
               in andTensors dims newTemporal (conjunctTensors dims invs)

    hoistTemporal2 :: TemporalOp2Args (Value Builtin) -> Value Builtin
    hoistTemporal2 (TemporalOp2Args dims a b body1 body2) =
      let body1' = goTensor body1
          body2' = goTensor body2
          (dep1, inv1) = partition (not . isTimeInvariant) (splitConj body1')
          (dep2, inv2) = partition (not . isTimeInvariant) (splitConj body2')
          allInv = inv1 ++ inv2
       in if null allInv
            then mkExpr accessTemporalUntil (TemporalOp2Args dims a b body1' body2')
            else
              let newBody1 = case dep1 of
                    [] -> body1'
                    _ -> conjunctTensors dims dep1
                  newBody2 = case dep2 of
                    [] -> body2'
                    _ -> conjunctTensors dims dep2
                  newUntil = mkExpr accessTemporalUntil (TemporalOp2Args dims a b newBody1 newBody2)
               in andTensors dims newUntil (conjunctTensors dims allInv)

    splitConj :: Value Builtin -> [Value Builtin]
    splitConj e = case toBoolTensorValue e of
      VBoolTensorAnd (TensorOp2Args _ e1 e2) -> splitConj e1 ++ splitConj e2
      _ -> [e]

    conjunctTensors :: Value Builtin -> [Value Builtin] -> Value Builtin
    conjunctTensors _ [x] = x
    conjunctTensors dims (x : xs) = foldl (andTensors dims) x xs
    conjunctTensors _ [] = developerError "conjunctTensors: empty list"

    andTensors :: Value Builtin -> Value Builtin -> Value Builtin -> Value Builtin
    andTensors dims x y = mkExpr accessAndTensor (TensorOp2Args dims x y)

isTimeInvariant :: Value Builtin -> Bool
isTimeInvariant e = case toBoolTensorValue e of
  VBoolConstTensor (ConstTensorArgs _ v _) -> isScalarTimeInvariant v
  VBoolTensorAnd (TensorOp2Args _ e1 e2) -> isTimeInvariant e1 && isTimeInvariant e2
  VBoolTensorOr (TensorOp2Args _ e1 e2) -> isTimeInvariant e1 && isTimeInvariant e2
  VBoolTensorNot (TensorOp1Args _ e1) -> isTimeInvariant e1
  VBoolTensorCompareRatPointwise (_, TensorOp2Args _ e1 e2) ->
    isRatTimeInvariant e1 && isRatTimeInvariant e2
  _ -> False

-- NBE folds `const c [T]` to `ConstantTensor [T] c` when c is a literal, so
-- both `VRatTensorLiteral` and `VRatConstTensor` arms are needed.
isRatTimeInvariant :: Value Builtin -> Bool
isRatTimeInvariant e = case toRatTensorValue e of
  VRatTensorLiteral t -> isUniformTensor t
  VRatConstTensor (ConstTensorArgs _ v _) -> isScalarTimeInvariant v
  VNegRatTensor (TensorOp1Args _ e1) -> isRatTimeInvariant e1
  VAddRatTensor (TensorOp2Args _ e1 e2) -> isRatTimeInvariant e1 && isRatTimeInvariant e2
  VSubRatTensor (TensorOp2Args _ e1 e2) -> isRatTimeInvariant e1 && isRatTimeInvariant e2
  VMulRatTensor (TensorOp2Args _ e1 e2) -> isRatTimeInvariant e1 && isRatTimeInvariant e2
  VDivRatTensor (TensorOp2Args _ e1 e2) -> isRatTimeInvariant e1 && isRatTimeInvariant e2
  _ -> False

-- `Tensor.fromVector` collapses uniform `DenseTensor`s to `ConstantTensor`,
-- so the dense arm can stay False.
isUniformTensor :: Tensor a -> Bool
isUniformTensor (ConstantTensor _ _) = True
isUniformTensor _ = False

isScalarTimeInvariant :: Value Builtin -> Bool
isScalarTimeInvariant v = case toRatTensorValue v of
  VRatTensorLiteral _ -> True
  VRatTensorBoundVar _ -> True
  VNegRatTensor (TensorOp1Args _ e1) -> isScalarTimeInvariant e1
  VAddRatTensor (TensorOp2Args _ e1 e2) -> isScalarTimeInvariant e1 && isScalarTimeInvariant e2
  VSubRatTensor (TensorOp2Args _ e1 e2) -> isScalarTimeInvariant e1 && isScalarTimeInvariant e2
  VMulRatTensor (TensorOp2Args _ e1 e2) -> isScalarTimeInvariant e1 && isScalarTimeInvariant e2
  VDivRatTensor (TensorOp2Args _ e1 e2) -> isScalarTimeInvariant e1 && isScalarTimeInvariant e2
  _ -> False

compileExists ::
  (MonadLogic m) =>
  QuantifyRatTensorArgs (Value Builtin) (Closure Builtin) ->
  m (MaybeTrivial Partitions)
compileExists (QuantifyRatTensorArgs dims binder closure) =
  logCompilerSection2 MaxDetail "convert-exists" $ do
    -- Extract the domain for the search
    lv <- getBinderDepth
    body <- hoistTimeIndependentBounds <$> normaliseClosure binder closure
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
        getLogicField TruthityElement

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
  Domain TensorValue ->
  m (Value LossBuiltin)
compileSearch varName dims binder closure (Domain lowerBound upperBound) = do
  -- Convert the binder and the dimensions.
  lossBinder <- traverse convertType binder
  lossDims <- convertDims dims

  -- Generate the operation for doing the reduction
  nameCtx <- getNameContext
  genericReductionOp <- getLogicField ReduceDisjunction
  -- The reduction runs over one loss value per sample, so the shape is
  -- just the (unknown) sample count, not the shape of the sampled input.
  -- TODO This is a complete hack. We really need the notion of an unknown dimension inside Vehicle.
  let reductionDims = IDimCons (INatLiteral (-1)) IDimNil
  reductionOp <- normaliseAppInEmptyFreeEnv nameCtx genericReductionOp [implicitIrrelevant reductionDims]

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
  return $ VBuiltin (LossBuiltinFunction $ SearchRatTensor varName minimise) spine

findTensorBounds ::
  forall m.
  (MonadLogic m) =>
  NestedSliceVariable ->
  PartiallyKnownTensorShape ->
  UserVariableConstraintTree ->
  m (DisjunctAll (TensorBounds TensorValue, Maybe UserVariableConstraintTree))
findTensorBounds parentVar parentVarShape constraints =
  go (DisjunctAll [(emptyBounds, Just constraints)]) parentVar
  where
    go ::
      DisjunctAll (TensorBounds TensorValue, Maybe UserVariableConstraintTree) ->
      NestedSliceVariable ->
      m (DisjunctAll (TensorBounds TensorValue, Maybe UserVariableConstraintTree))
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

noQuantifierDomainError ::
  (MonadDomain m) =>
  VBinder Builtin ->
  UnboundedIndices ->
  m a
noQuantifierDomainError binder missingIndices = do
  propertyProv <- getDeclProvenance
  throwError $ NoQuantifierDomainFound propertyProv binder missingIndices

findVarBound ::
  (MonadLogic m) =>
  NestedSliceVariable ->
  VariableInfo ->
  UserVariableConstraint ->
  m (Maybe (TensorBounds TensorValue))
findVarBound var VariableInfo {..} (NormalisedRelation rel expr)
  | not (expr `containsVariable` toSliceVar var) = return Nothing
  | otherwise = do
      let (coef, expr') = rearrangeExprToSolveFor (toSliceVar var) expr
      let boundExpr = tensorValueLinarExprToValue expr'
      let bounds = convertToTensorBounds parentShape indices rel coef boundExpr
      return $ Just bounds

--------------------------------------------------------------------------------
-- Constraint search
--------------------------------------------------------------------------------
-- Definitions

type MonadDomain m =
  (MonadLogic m)

orLossValue :: (MonadDomain m) => Value LossBuiltin -> Value LossBuiltin -> m (Value LossBuiltin)
orLossValue e1 e2 = convertOr (TensorOp2Args IDimNil e1 e2)

andLossValue :: (MonadDomain m) => Value LossBuiltin -> Value LossBuiltin -> m (Value LossBuiltin)
andLossValue e1 e2 = convertAnd (TensorOp2Args IDimNil e1 e2)

notConstraint :: (MonadDomain m) => UserVariableConstraint -> m (BooleanExpr UserVariableConstraint)
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
newtype Partitions = Partitions (Map (Maybe UserVariableConstraintTree) (Maybe (Value LossBuiltin)))

singletonConstrainedPartition :: UserVariableConstraint -> Partitions
singletonConstrainedPartition constraint = Partitions $ Map.singleton (Just (Query constraint)) Nothing

singletonUnconstrainedPartition :: (MonadDomain m) => Value LossBuiltin -> m Partitions
singletonUnconstrainedPartition unconstrained = do
  return $ Partitions $ Map.singleton Nothing (Just unconstrained)

singletonPartition :: Partition -> Partitions
singletonPartition (tree, value) = Partitions $ Map.singleton tree value

numberOfPartitions :: Partitions -> Int
numberOfPartitions (Partitions ps) = length ps

containsConstraints :: Partitions -> Bool
containsConstraints (Partitions partitions) = case Map.toList partitions of
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
partitionsToDisjuncts (Partitions ps) = case Map.toList ps of
  [] -> developerError "Empty partition"
  x : xs -> DisjunctAll $ x :| xs

disjunctPartitions :: (MonadDomain m) => DisjunctAll Partitions -> m Partitions
disjunctPartitions (DisjunctAll (p :| ps)) = foldrM orPartitions p ps

disjunctMaybeTrivialPartitions :: (MonadDomain m) => DisjunctAll (MaybeTrivial Partitions) -> m (MaybeTrivial Partitions)
disjunctMaybeTrivialPartitions = traverse disjunctPartitions . eliminateTrivialDisjunctions

orPartitions :: (MonadDomain m) => Partitions -> Partitions -> m Partitions
orPartitions (Partitions p1) (Partitions p2) = do
  Partitions <$> unionWithM (unionMaybeWithM orLossValue) p1 p2

andPartitions :: (MonadDomain m) => Partitions -> Partitions -> m Partitions
andPartitions p1 p2 = do
  disjuncts <- conjunctDisjunctsM andPartition (partitionsToDisjuncts p1) (partitionsToDisjuncts p2)
  return $ Partitions $ Map.fromList $ disjunctsToList disjuncts

unblockingActions :: (MonadDomain m) => UnblockingActions m
unblockingActions =
  UnblockingActions
    { unblockRatTensorBoundVar = purifyBoundVar,
      unblockNetworkApp = \_unblockFn ident args -> return $ VFreeVar ident (mkExpr accessSpine args)
    }

--------------------------------------------------------------------------------
-- Search algorithm

-- | Drop any `! i` indexing to get at the operator underneath.
stripBoolAt :: Value Builtin -> Value Builtin
stripBoolAt v = case toBoolTensorValue v of
  VBoolTensorAt (AtTensorArgs _ _ _ t _) -> stripBoolAt t
  _ -> case toBoolValue v of
    VBoolAt (AtTensorArgs _ _ _ t _) -> stripBoolAt t
    _ -> v

isUntilHead :: Value Builtin -> Bool
isUntilHead v = case toBoolTensorValue (stripBoolAt v) of
  VBoolTensorUntil {} -> True
  _ -> case toBoolValue (stripBoolAt v) of
    VUntil {} -> True
    _ -> False

-- | `not (until ...)` (ignoring `! i` indexing): the one shape
-- `lowerNot` can't simplify. Not matched through `and`/`or`.
negatedUntilLeaf :: Value Builtin -> Bool
negatedUntilLeaf v = case toBoolTensorValue (stripBoolAt v) of
  VBoolTensorNot (TensorOp1Args _ e) -> isUntilHead e
  _ -> case toBoolValue (stripBoolAt v) of
    VNot (TensorOp1Args _ e) -> isUntilHead e
    _ -> False

compileBool :: (MonadDomain m) => Value Builtin -> m (MaybeTrivial Partitions)
compileBool value = logEntryAndExit value $ case toBoolValue value of
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
  VAnd args -> compileAnd args
  VOr args -> compileOr args
  VBoolIf args -> compileBool =<< unfoldIf args
  VNot args -> do
    let tryUnblock e = do
          maybeE <- maybeUnblockBoolExpr unblockingActions e
          return $ fromMaybe e maybeE
    lowered <- lowerNot tryUnblock args
    -- Recursing on `not (until ...)` would loop forever; pass it
    -- through to the loss IR as we already do for a plain `until`.
    if negatedUntilLeaf lowered
      then do
        lossValue <- convertBoolTensor value
        NonTrivial <$> singletonUnconstrainedPartition lossValue
      else compileBool lowered
  VQuantifyRatTensor args -> compileQuantifierInternal args
  -------------------
  -- Blocked cases --
  -------------------
  VReduceAndTensor {} -> unblockBoolValue value
  VReduceOrTensor {} -> unblockBoolValue value
  VBoolAt {} -> unblockBoolValue value
  VGlobally {} -> unblockBoolValue value
  VFinally {} -> unblockBoolValue value
  VUntil {} -> unblockBoolValue value

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
  (ComparisonOp, TensorOp2Args (Value Builtin)) ->
  m (MaybeTrivial Partitions)
compileComparison (op, args)
  | op == Ne = compileNonBoundComparison (op, args)
  | otherwise = do
      blockedValueOrResult <- purifyAssertion op args
      case blockedValueOrResult of
        Left err -> case err of
          ImpureButProgress value -> compileBool value
          ContainsNetwork ident -> do
            logDebug MaxDetail $ "invalid-bound" <+> parens ("contains" <+> quotePretty ident)
            compileNonBoundComparison (op, args)
          ContainsMultipleUserVariablesFromSameSlice _parent var1 var2 -> do
            logDebugM MaxDetail $ do
              var1Doc <- squotes <$> prettyFriendlyInCtx var1
              var2Doc <- squotes <$> prettyFriendlyInCtx var2
              return $ "invalid-bound" <+> parens ("contains" <+> var1Doc <+> "and" <+> var2Doc <+> "from same tensor")
            compileNonBoundComparison (op, args)
        Right (TensorOp2Args dims e1 e2) -> do
          lossDims <- convertDims dims
          errorOrAssertion <- runExceptT $ do
            linX <- compileLinearExpr lossDims e1
            linY <- compileLinearExpr lossDims e2
            comparisonToAssertion op linX linY

          case errorOrAssertion of
            Left uncompilable -> do
              logDebugM MaxDetail $ do
                doc <- prettyFriendlyInCtx uncompilable
                return $ "invalid-bound" <+> parens ("unable to unblock" <+> doc)
              compileNonBoundComparison (op, args)
            Right (Left trivialValue) -> do
              logDebug MaxDetail $ "invalid-bound" <+> parens ("trivially" <+> pretty trivialValue)
              return $ Trivial trivialValue
            Right (Right assertion) -> do
              logDebugM MaxDetail $ do
                doc <- prettyFriendlyInCtx assertion
                return $ "valid-bound: " <+> doc
              return $ NonTrivial $ singletonConstrainedPartition assertion

compileNonBoundComparison ::
  (MonadDomain m) =>
  (ComparisonOp, TensorOp2Args (Value Builtin)) ->
  m (MaybeTrivial Partitions)
compileNonBoundComparison args = do
  value <- convertRatTensorPointwiseComparison args
  NonTrivial <$> singletonUnconstrainedPartition value

-- | Unblocking a boolean value is a little complicated.
-- If an expression cannot be immediately compiled to constraints, we have two
-- options. Firstly, it _may_ contain constraints so we need to try unblocking
-- it to see and then compiling the results. However, if the resulting expression
-- contains no constraints over the quantified variables, then we instead
-- directly convert it to a loss value as that will likely result in a more
-- efficient expression to evaluate.
unblockBoolValue ::
  (MonadDomain m) =>
  Value Builtin ->
  m (MaybeTrivial Partitions)
unblockBoolValue value = do
  maybeUnblockedExpr <- maybeUnblockBoolExpr unblockingActions value
  maybeResult <- case maybeUnblockedExpr of
    Nothing -> return Nothing
    Just unblockedExpr -> do
      maybePartitions <- compileBool unblockedExpr
      case maybePartitions of
        Trivial {} -> return $ Just maybePartitions
        NonTrivial partitions ->
          if containsConstraints partitions
            then return $ Just maybePartitions
            else return Nothing

  case maybeResult of
    Just result -> return result
    Nothing -> do
      lossValue <- convertBoolTensor value
      NonTrivial <$> singletonUnconstrainedPartition lossValue

--------------------------------------------------------------------------------
-- Comparison purification

-- | A comparison may be compiled to a potential bound as long as:
-- * It does not contain any network applications (e.g. f x < 0.5)
-- * It does not compare slices from the same user tensor (e.g. x ! 0 < x ! 1)
data PurificationError
  = ContainsNetwork Identifier
  | ContainsMultipleUserVariablesFromSameSlice UserTensorVariable UserSliceVariable UserSliceVariable
  | ImpureButProgress (Value Builtin)

-- | Monad purify
type MonadPurifyAssertion m =
  ( MonadPurify m,
    MonadError PurificationError m,
    MonadReadableTensorBoundContext m
  )

purifyAssertion ::
  (MonadDomain m) =>
  ComparisonOp ->
  TensorOp2Args (Value Builtin) ->
  m (Either PurificationError (TensorOp2Args (Value Builtin)))
purifyAssertion op args = do
  callDepth <- getCallDepth
  runExceptT $ do
    errorOrResult <- tryPurifyAssertion purifyUnblockingActions op args
    case errorOrResult of
      Left err -> do
        setCallDepth callDepth
        throwError $ ImpureButProgress err
      Right value -> return value

purifyUnblockingActions :: (MonadPurifyAssertion m) => UnblockingActions m
purifyUnblockingActions =
  UnblockingActions
    { unblockRatTensorBoundVar = purifyBoundVar,
      unblockNetworkApp = purifyNetworkApp
    }

purifyNetworkApp ::
  (MonadPurifyAssertion m) =>
  (Value Builtin -> m (Value Builtin)) ->
  Identifier ->
  NetworkAppArgs (Value Builtin) ->
  m (Value Builtin)
purifyNetworkApp _unblockFn ident _spine = throwError $ ContainsNetwork ident

purifyBoundVar :: (MonadLogger m, MonadReadableTensorBoundContext m) => Lv -> m (Value Builtin)
purifyBoundVar lv = do
  (_, maybeUserVars) <- lookupVariableInNestedCtx lv
  case maybeUserVars of
    Nothing -> return $ VBoundVar lv []
    Just (_tensorVar, sliceVar) -> replaceTensorVariableWithStackedChildren sliceVar

--------------------------------------------------------------------------------
-- Compiling linear expressions

compileLinearExpr ::
  forall m.
  (MonadLogger m, MonadReadableTensorBoundContext m, MonadError (Value Builtin) m) =>
  VDims LossBuiltin ->
  Value Builtin ->
  m (LinearExpr SliceVariable TensorValue)
compileLinearExpr dims expr = case toRatTensorValue expr of
  ----------------
  -- Base cases --
  ----------------
  VRatTensorLiteral t -> do
    let lossExpr = mkExpr accessRatTensorLiteral t
    return $ constantExpr $ TensorValue dims lossExpr
  VRatTensorBoundVar var -> do
    maybeExpr <- compileRatTensorVar dims var
    maybe unlinearisable return maybeExpr
  ---------------------
  -- Inductive cases --
  ---------------------
  VNegRatTensor (TensorOp1Args _ e) -> do
    e' <- compileLinearExpr dims e
    return $ scaleExpr (-1) e'
  VAddRatTensor (TensorOp2Args _ e1 e2) -> do
    e1' <- compileLinearExpr dims e1
    e2' <- compileLinearExpr dims e2
    return $ addExprsUnsafe 1 1 e1' e2'
  VSubRatTensor (TensorOp2Args _ e1 e2) -> do
    e1' <- compileLinearExpr dims e1
    e2' <- compileLinearExpr dims e2
    return $ addExprsUnsafe 1 (-1) e1' e2'
  ---------------------
  -- Unreduced cases --
  ---------------------
  -- A zero-dim `const v []` is `v`; NBE leaves this form when v is non-literal.
  VRatConstTensor (ConstTensorArgs _ v innerDims) -> case toDimensionsValue innerDims of
    VDimsNil -> compileLinearExpr dims v
    _ -> unlinearisable
  VRatStackTensor (StackTensorArgs _ _ _ xs) -> do
    let innerDims = case dims of
          IDimCons _ ds -> ds
          _ -> developerError "VRatStackTensor must have non-nil dims"
    children <- traverse (compileLinearExpr innerDims) xs
    case traverse isConstant children of
      Just constChildren -> return $ constantExpr $ stackTensorValues constChildren
      Nothing -> unlinearisable
  VRatAt {} -> unlinearisable
  VRatTensorFreeVar ident [] ->
    return $ constantExpr $ TensorValue dims (VFreeVar ident [])
  VRatTensorFreeVar {} -> unlinearisable
  VRatForeach {} -> unlinearisable
  VIfRatTensor {} -> unlinearisable
  -----------------------
  -- Unsupported cases --
  -----------------------
  -- Min/max could be handled by splitting into two constraints?
  VMinRatTensor {} -> unlinearisable
  VMaxRatTensor {} -> unlinearisable
  VPowRatTensor {} -> unlinearisable
  VExpRatTensor {} -> unlinearisable
  VLogRatTensor {} -> unlinearisable
  VReduceAddRatTensor {} -> unlinearisable
  VReduceMulRatTensor {} -> unlinearisable
  VReduceMinRatTensor {} -> unlinearisable
  VReduceMaxRatTensor {} -> unlinearisable
  VRatTensorRollout {} -> unlinearisable
  VRatTensorTranspose {} -> unlinearisable
  VMulRatTensor (TensorOp2Args _ e1 e2) -> do
    e1' <- compileLinearExpr dims e1
    e2' <- compileLinearExpr dims e2
    case (isConstant e1', isConstant e2') of
      (Just (TensorValue _ v1), Just (TensorValue _ v2)) -> do
        result <- evalMulRatTensor (TensorOp2Args dims v1 v2)
        return $ constantExpr $ TensorValue dims result
      _ -> unlinearisable
  VDivRatTensor (TensorOp2Args _ e1 e2) -> do
    e1' <- compileLinearExpr dims e1
    e2' <- compileLinearExpr dims e2
    case (isConstant e1', isConstant e2') of
      (Just (TensorValue _ v1), Just (TensorValue _ v2)) -> do
        result <- evalDivRatTensor (TensorOp2Args dims v1 v2)
        return $ constantExpr $ TensorValue dims result
      _ -> unlinearisable
  where
    unlinearisable :: m (LinearExpr SliceVariable TensorValue)
    unlinearisable = throwError expr

compileRatTensorVar ::
  (MonadLogger m, MonadReadableTensorBoundContext m) =>
  VDims LossBuiltin ->
  Lv ->
  m (Maybe (LinearExpr SliceVariable TensorValue))
compileRatTensorVar dims lv = do
  (_, maybeSliceVar) <- lookupVariableInNestedCtx lv
  forM maybeSliceVar $ \(_tensorVar, sliceVar) -> do
    zeroTensor <- evalConstTensor $ ConstTensorArgs IRatType (IRatLiteral 0) dims
    return $ singletonVarExpr (TensorValue dims zeroTensor) sliceVar

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
    -- tensorCtx <- toNamedBoundCtx . originalCtx <$> getNestedVariableCtx
    return "search-exit" -- :" <+> prettyFriendlyInCt
  return result
