module Vehicle.Backend.Loss.Domain
  ( compileQuantifier,
  )
where

import Control.Monad (filterM, foldM, forM)
import Control.Monad.Except (MonadError (..), runExceptT)
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (foldrM)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
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
import Vehicle.Compile.Unblock (MonadPurify, UnblockingActions (..), tryPurifyAssertion, unblockBoolExpr)
import Vehicle.Data.Assertion (NormalisedRelation (..), Relation (..), comparisonToAssertion, relationToComparisonOp)
import Vehicle.Data.Bound
import Vehicle.Data.Bound.FourierMotzkinElimination (fourierMotzkinTensorBoundsElimination)
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Builtin.Interface.Normalise (evalConstTensor, evalDivRatTensor, evalMulRatTensor)
import Vehicle.Data.Builtin.Loss
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.BooleanExpr (BooleanExpr (..), ConjunctAll (..), DisjunctAll (..), andBoolExpr, conjunctDisjunctsM, disjunctDisjuncts, disjunctsToList, eliminateTrivialDisjunctions, flattenBoolExpr)
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Value
import Vehicle.Data.DifferentiableLogic (TensorDifferentiableLogicField (..), comparisonOpToField)
import Vehicle.Data.MaybeTrivial
import Vehicle.Data.Tensor (shapeOf, pattern ZeroDimTensor)
import Vehicle.Data.Tensor.Traversal
import Vehicle.Data.Variable.Bound.Context.Generic (BoundCtx)
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Data.Variable.Bound.Context.Tensor
import Vehicle.Data.Variable.Bound.Level
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
  (Just constraints, Nothing) -> constraintsTreeToLoss constraints
  (Just constraints, Just value) -> do
    constraintsValue <- constraintsTreeToLoss constraints
    andLossValue value constraintsValue

constraintsTreeToLoss ::
  (MonadLogic m) =>
  UserVariableConstraintTree ->
  m (Value LossBuiltin)
constraintsTreeToLoss = \case
  Query c -> constraintToLoss c
  Conjunct (ConjunctAll (c :| cs)) -> do
    c' <- constraintsTreeToLoss c
    cs' <- traverse constraintsTreeToLoss cs
    foldrM andLossValue c' cs'
  Disjunct (DisjunctAll (c :| cs)) -> do
    c' <- constraintsTreeToLoss c
    cs' <- traverse constraintsTreeToLoss cs
    foldrM orLossValue c' cs'

constraintToLoss ::
  (MonadLogic m) =>
  UserVariableConstraint ->
  m (Value LossBuiltin)
constraintToLoss (NormalisedRelation rel expr) = do
  let TensorValue dims lhs = tensorValueLinarExprToValue expr
  let rhs = tensorValue $ constantDimensionedValue dims 0
  fn <- getLogicField (comparisonOpToField $ relationToComparisonOp rel)
  nameCtx <- getNameContext
  normaliseAppInEmptyFreeEnv nameCtx fn (mkExpr accessSpine $ TensorOp2Args dims lhs rhs)

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
        getLogicField TruthityElement

    logDebugM MidDetail $ do
      remainderDoc <- prettyFriendlyInCtx remainingBody
      return $
        "remaining-expression:"
          <> lineIndent remainderDoc

    -- Find the bounds on the quantified variable from the constraints
    let partialShape = extractPartialShape dims
    disjunctedTensorBounds <- findTensorBounds var partialShape constraints

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

        partitionBodyClosed <- closeCurrentTensorValue var remainingBody
        let lossBody = quote mempty (1 + boundCtxLv finalCtx) partitionBodyClosed
        let finalEnv = boundContextToEnv finalCtx
        let remainder = Closure finalEnv lossBody

        finalValue <- compileSearch var varName dims binder remainder domain
        return $ singletonPartition (remainingTree, Just finalValue)
    NonTrivial <$> disjunctPartitions newPartitions

-- | Replaces VBoundVar slice-level references in a TensorValue with x!i
-- indexed access, so the value is closed with respect to the tensor context.
closeCurrentTensorValue :: (MonadLogic m) => NestedSliceVariable -> Value LossBuiltin -> m (Value LossBuiltin)
closeCurrentTensorValue target val = case val of
  VBoundVar lv [] -> do
    if shouldClose lv
      then do
        (_, maybeVars) <- lookupVariableInNestedCtx lv
        let var = VBoundVar (toLv target) []
        case maybeVars of
          Nothing -> return var
          Just (parentVar, sliceVar) -> do
            let indices = findSliceIndices parentVar sliceVar
            return $ mkIndexInto IRatType var (shapeOf parentVar) indices
      else return $ VBoundVar lv []
  VBoundVar lv spine -> do
    spine' <- traverse (traverse (closeCurrentTensorValue target)) spine
    return $ VBoundVar lv spine'
  VBuiltin b spine -> do
    spine' <- traverse (traverse (closeCurrentTensorValue target)) spine
    return $ VBuiltin b spine'
  VFreeVar v spine -> do
    spine' <- traverse (traverse (closeCurrentTensorValue target)) spine
    return $ VFreeVar v spine'
  VLam binder (Closure env body) -> do
    binder' <- traverse (closeCurrentTensorValue target) binder
    return $ VLam binder' (Closure env body)
  other -> return other
  where
    shouldClose lv =
      let start = toLv target
          end = start + Lv (numberOfSliceVariablesIn $ shapeOf target)
       in lv >= start && lv < end

compileSearch ::
  (MonadLogic m) =>
  NestedSliceVariable ->
  Name ->
  VDims Builtin ->
  VBinder Builtin ->
  Closure LossBuiltin ->
  Domain TensorValue ->
  m (Value LossBuiltin)
compileSearch target varName dims binder closure (Domain lowerBound upperBound) = do
  -- Convert the binder and the dimensions.
  lossBinder <- traverse convertType binder
  lossDims <- convertDims dims

  -- Generate the operation for doing the reduction
  nameCtx <- getNameContext
  genericReductionOp <- getLogicField ReduceDisjunction
  -- TODO This is a complete hack. We really need the notion of an unknown dimension inside Vehicle.
  let reductionDims = IDimCons (INatLiteral (-1)) lossDims
  reductionOp <- normaliseAppInEmptyFreeEnv nameCtx genericReductionOp [implicitIrrelevant reductionDims]

  -- Reform the predicate as if we had no tensor variables at all
  let lossPredicate = VLam lossBinder closure

  -- Close the bound values by replacing slice-level VBoundVars with x!i indexing
  closedLowerBound <- closeCurrentTensorValue target $ tensorValue $ lowerBoundValue lowerBound
  closedUpperBound <- closeCurrentTensorValue target $ tensorValue $ upperBoundValue upperBound

  -- Create the final expression
  -- NOTE that this is unsound as we discard the strictness information.
  let spine =
        mkExpr accessSpine $
          SearchRatTensorArgs
            { searchDims = lossDims,
              searchReductionOp = reductionOp,
              searchLowerBound = closedLowerBound,
              searchUpperBound = closedUpperBound,
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
      -- Only extract bounds that are independent of the quantified tensor itself.
      -- If the bound still depends on any slice from the same parent tensor,
      -- keep it as a residual constraint in the predicate instead.
      let dependentVars = Set.toList $ variablesOf expr'
      sameParentDeps <- filterM isFromSameParentTensor dependentVars
      let selfDependent = length sameParentDeps > 1
      if selfDependent
        then return Nothing
        else do
          let boundExpr = tensorValueLinarExprToValue expr'
          let bounds = convertToTensorBounds parentShape indices rel coef boundExpr
          return $ Just bounds
  where
    isFromSameParentTensor :: (MonadLogic m) => SliceVariable -> m Bool
    isFromSameParentTensor sliceVar = do
      parent <- lookupParentTensorVariable sliceVar
      return $ nestedStartingVariable parent == toSliceVar parentVariable

--------------------------------------------------------------------------------
-- Constraint search
--------------------------------------------------------------------------------
-- Definitions

type MonadDomain m =
  ( MonadLogic m
  )

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
    { unblockRatTensorBoundVar = \lv -> return $ VBoundVar lv [],
      unblockNetworkApp = \ident args -> return $ VFreeVar ident (mkExpr accessSpine args)
    }

--------------------------------------------------------------------------------
-- Search algorithm

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
  VNot args -> compileBool =<< lowerNot args
  VQuantifyRatTensor args -> compileQuantifierInternal args
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
-- If an expression cannot be immediately compiled to constriants, we have two
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
  result <- unblockBoolExpr unblockingActions value
  -- If unblocking didn't change the expression, convert directly to loss
  -- to avoid infinite loops with blocked expressions that can't be unblocked
  if result == value
    then do
      lossValue <- convertBoolTensor value
      NonTrivial <$> singletonUnconstrainedPartition lossValue
    else do
      maybePartitions <- compileBool result
      case maybePartitions of
        Trivial {} -> return maybePartitions
        NonTrivial partitions ->
          if containsConstraints partitions
            then return maybePartitions
            else do
              lossValue <- convertBoolTensor result
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

purifyNetworkApp :: (MonadPurifyAssertion m) => Identifier -> NetworkAppArgs (Value Builtin) -> m (Value Builtin)
purifyNetworkApp ident _spine = throwError $ ContainsNetwork ident

purifyBoundVar :: (MonadPurifyAssertion m) => Lv -> m (Value Builtin)
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
  -- The expression is being blocked
  VRatConstTensor {} -> unlinearisable
  VRatStackTensor {} -> unlinearisable
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
  VReduceAddRatTensor {} -> unlinearisable
  VReduceMulRatTensor {} -> unlinearisable
  VReduceMinRatTensor {} -> unlinearisable
  VReduceMaxRatTensor {} -> unlinearisable
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
