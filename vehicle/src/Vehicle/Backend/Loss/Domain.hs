module Vehicle.Backend.Loss.Domain
  ( findAndAttachQuantifierBounds,
  )
where

import Control.Monad (foldM, forM)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (foldrM, toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Proxy (Proxy (..))
import Vehicle.Backend.Loss.PurifyAssertion
import Vehicle.Backend.Solver.UserVariableElimination.ConstraintSearch (findAllBounds)
import Vehicle.Compile.Constants.TensorValue
import Vehicle.Compile.Constants.TensorValue.Core
import Vehicle.Compile.Error
import Vehicle.Compile.LiftIf (unfoldIf)
import Vehicle.Compile.LowerNot (lowerNot, negateQuantifierBody)
import Vehicle.Compile.Normalise.Builtin (EvalSimple, elimImplies, evalAnd, evalNot, evalOr, forceEvaluation)
import Vehicle.Compile.Normalise.Force
import Vehicle.Compile.Normalise.Quote (unnormaliseInTensorCtx)
import Vehicle.Compile.Normalise.RewriteRules (forceAndRewriteTensor)
import Vehicle.Compile.Normalise.TypedValue
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Compile.Unblock (unblockBoolExpr)
import Vehicle.Data.Assertion (Assertion, InequalityRelation (..), NormalisedRelation (..), Relation (..))
import Vehicle.Data.Bound
import Vehicle.Data.Bound.FourierMotzkinElimination (fourierMotzkinTensorBoundsElimination)
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Builtin.Loss
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.BooleanExpr (BooleanExpr (..), ConjunctAll (..), DisjunctAll (..), andBoolExpr, conjunctDisjunctsM, disjunctDisjuncts, disjunctsToList, elimIfTree, eliminateTrivialDisjunctions, flattenBoolExpr)
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.MaybeTrivial
import Vehicle.Data.Real (ExtendedRational (..))
import Vehicle.Data.Tensor (TensorIndices)
import Vehicle.Data.Variable.Bound.Context.Generic (toNamedBoundCtx)
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Data.Variable.Bound.Context.Tensor
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Data.Variable.Free.Context (MonadFreeContext (..), addDeclToContext, runFreshFreeContextT)
import Vehicle.Prelude.Warning (CompileWarning (..))

-- | An assertion, paired with the term it was derived from. The term is kept so that an assertion
-- that no quantifier ends up using can be put back into the body it came from, rather than being
-- reconstructed from the linear expression in a less readable normal form.
type UserVariableConstraint = (Assertion TensorValueLinearExpr, Thunk Builtin)

type UserVariableConstraintTree = BooleanExpr UserVariableConstraint

findAndAttachQuantifierBounds :: (MonadCompile m) => Prog Builtin -> m (Prog Builtin)
findAndAttachQuantifierBounds (Main decls) =
  logCompilerPass LossBounds $
    runFreshFreeContextT (Proxy @Builtin) $ do
      prog <- Main <$> processDecls decls
      logDebug MidDetail $ prettyFriendly prog
      return prog

processDecls :: (MonadCompile m, MonadFreeContext Builtin m) => [Decl Builtin] -> m [Decl Builtin]
processDecls = \case
  [] -> return []
  d : ds -> do
    d' <- processDecl d
    ds' <- addDeclToContext d $ processDecls ds
    return $ d' : ds'

processDecl :: (MonadCompile m, MonadFreeContext Builtin m) => Decl Builtin -> m (Decl Builtin)
processDecl decl =
  logCompilerSection2 MaxDetail ("searching" <+> quotePretty (identifierOf decl)) $
    case decl of
      DefAbstract {} -> return decl
      DefRecord {} -> return decl
      DefFunction p n b t e -> do
        e' <- runFreshTensorBoundContextT $ runReaderT (processExpr e) (n, p)
        return $ DefFunction p n b t e'

type MonadDomain m =
  ( MonadCompile m,
    MonadReader DeclProvenance m,
    MonadFreeContext Builtin m,
    MonadTensorBoundContext m
  )

processExpr :: (MonadDomain m) => Expr Builtin -> m (Expr Builtin)
processExpr expr = case expr of
  Pi {} -> unexpectedExprError currentPass "Pi"
  Hole {} -> unexpectedExprError currentPass "Hole"
  Meta {} -> unexpectedExprError currentPass "Meta"
  Universe {} -> unexpectedExprError currentPass "Universe"
  (getExpr accessQuantifyRatTensor -> Just (q, QuantifyRatTensorArgs pDims bDims binder body)) -> do
    boundEnv <- boundContextToEnv <$> getShrunkenContext
    let vArgs =
          QuantifyRatTensorArgs
            { quantifyPointwiseDims = Unforced boundEnv pDims,
              quantifyBaseDims = Unforced boundEnv bDims,
              quantifyBinder = fmap (Unforced boundEnv) binder,
              quantifyBody = Closure boundEnv body
            }
    result <- compileQuantifier (provenanceOf expr) (q, vArgs)
    let mkOr x y = mkExpr accessOrTensor $ TensorOp2Args pDims x y
    return $ foldr1 mkOr result
  Builtin {} -> return expr
  FreeVar {} -> return expr
  BoundVar {} -> return expr
  App fun args -> App <$> processExpr fun <*> traverse (traverse processExpr) args
  Let p bound binder body -> Let p <$> processExpr bound <*> pure binder <*> processExpr body
  Lam p binder body -> Lam p binder <$> addNonTensorBinderToContext binder (processExpr body)
  Record p t fs -> Record p t <$> traverseRecordFields processExpr fs
  RecordProj p t r field -> RecordProj p t <$> processExpr r <*> pure field

compileQuantifier ::
  (MonadDomain m) =>
  Provenance ->
  (Quantifier, QuantifyRatTensorArgs (Thunk Builtin) (Closure Builtin)) ->
  m (DisjunctAll (Expr Builtin))
compileQuantifier p (q, args) = do
  maybePartitions <- compileQuantifierInternal p (q, args)
  case maybePartitions of
    Trivial b -> do
      -- TODO add a warning
      return $ DisjunctAll [IBoolLiteral b]
    NonTrivial partitions -> do
      let disjunctedPartitions = partitionsToDisjuncts partitions
      traverse checkFinalPartitionUnconstrained disjunctedPartitions

checkFinalPartitionUnconstrained ::
  (MonadDomain m) =>
  Partition ->
  m (Expr Builtin)
checkFinalPartitionUnconstrained = \case
  (Nothing, Nothing) -> developerError "Found unexpected trivial partition"
  (Nothing, Just value) -> return value
  (Just {}, _) -> developerError "Constraints still unexpected present after compiling top-level quantifier"

compileQuantifierInternal ::
  (MonadDomain m) =>
  Provenance ->
  (Quantifier, QuantifyRatTensorArgs (Thunk Builtin) (Closure Builtin)) ->
  m (MaybeTrivial Partitions)
compileQuantifierInternal p (q, args) = case q of
  Exists -> compileExists p args
  Forall -> compileForall p args

compileForall ::
  (MonadDomain m) =>
  Provenance ->
  QuantifyRatTensorArgs (Thunk Builtin) (Closure Builtin) ->
  m (MaybeTrivial Partitions)
compileForall p args = do
  let notArgs = negateQuantifierBody args
  maybePartitions <- compileExists p notArgs
  case maybePartitions of
    Trivial b -> return $ Trivial $ not b
    NonTrivial partitions -> NonTrivial <$> notPartitions IDimNil partitions

compileExists ::
  (MonadDomain m) =>
  Provenance ->
  QuantifyRatTensorArgs (Thunk Builtin) (Closure Builtin) ->
  m (MaybeTrivial Partitions)
compileExists p (QuantifyRatTensorArgs pDims bDims binder closure) =
  logCompilerSection2 MaxDetail "convert-exists" $ do
    -- Extract the domain for the search
    lv <- getBinderDepth
    knownShape@(shapePrefix, _) <- extractKnownShape bDims

    dims' <- unnormaliseInTensorCtx bDims
    binder' <- traverse unnormaliseInTensorCtx binder

    -- Compile the body recursively into partitions
    addTensorBinderToContextLocally shapePrefix binder $ do
      userTensorVar <- lookupNestedTensorVariable $ UserTensorVariable $ TensorVariable $ SliceVariable lv
      let body = extendClosureWithBound closure binder lv
      maybePartitions <- compileBool body

      -- Compile those partitios
      case maybePartitions of
        Trivial b -> do
          -- TODO throw warning
          return $ Trivial b
        NonTrivial partitions -> do
          logDebug MaxDetail $ "number-of-partitions:" <+> pretty (numberOfPartitions partitions)
          xs <- traverse (compileConstraints p dims' knownShape binder' userTensorVar) (partitionsToDisjuncts partitions)
          pDims' <- unnormaliseInTensorCtx pDims
          disjunctMaybeTrivialPartitions pDims' xs

compileConstraints ::
  (MonadDomain m) =>
  Provenance ->
  Expr Builtin ->
  (KnownPrefixOfTensorShape, Thunk Builtin) ->
  Binder Builtin ->
  NestedSliceVariable ->
  Partition ->
  m (MaybeTrivial Partitions)
compileConstraints p dims knownShape binder var (maybeConstraints, maybeRemainder) = do
  let (varName, _) = getNamedBinderInfo binder
  logCompilerSection2 MidDetail ("extracting bounds for" <+> quotePretty varName <+> "from partition") $ do
    -- Extract the constraints we can use to bound the variable
    logDebugM MidDetail $ do
      boundsDoc <- prettyFriendlyInCtx maybeConstraints
      return $
        "all-constraints:"
          <> lineIndent boundsDoc

    -- Extract the remaining body of the quantifier
    remainingBody <- case maybeRemainder of
      Just remainder -> return remainder
      Nothing -> do
        (ident, _p) <- ask
        logWarning $ BoundsOnlyQuantifier (nameOf ident) varName
        return $ IBoolLiteral True

    logDebugM MidDetail $ do
      shrunkenCtx <- toNamedBoundCtx <$> getShrunkenContext
      let remainderDoc = prettyFriendly $ WithContext remainingBody shrunkenCtx
      return $
        "remaining-expression:"
          <> lineIndent remainderDoc

    -- Find the bounds on the quantified variable from the constraints
    disjunctedTensorBounds <- findTensorBounds var knownShape maybeConstraints
    logDebug MaxDetail $ "number-of-constraint-partitions:" <+> pretty (length disjunctedTensorBounds)

    -- For each set of disjuncted bounds create a search expression.
    newPartitions <- forM disjunctedTensorBounds $ \(tensorBounds, remainingTree) -> logCompilerSection2 MaxDetail "flattening of constraint partition" $ do
      logDebugM MaxDetail $ do
        boundsDoc <- prettyFriendlyInCtx (BoundedValue var tensorBounds)
        return $ "all-variable-bounds:" <> lineIndent boundsDoc

      domain <- fourierMotzkinTensorBoundsElimination knownShape tensorBounds

      logDebugM MaxDetail $ do
        boundsDoc <- prettyFriendlyInCtx (BoundedValue var domain)
        return $ "final-domain:" <> lineIndent boundsDoc

      logDebugM MaxDetail $ do
        remDoc <- maybe (return "") (fmap lineIndent . prettyFriendlyInCtx) remainingTree
        return $ "remaining-constraints:" <> remDoc

      -- Constraints mentioning this quantifier's variables cannot be emitted outside its binder,
      -- so put them back into its body rather than propagating them outwards.
      (inScope, outOfScope) <- case remainingTree of
        Nothing -> return (Nothing, Nothing)
        Just tree -> splitTreeByScope (toLv var) tree

      finalBody <- case inScope of
        Nothing -> return remainingBody
        Just tree -> do
          leftovers <- treeToExpr tree
          logDebugM MaxDetail $ do
            doc <- prettyFriendlyInCtx tree
            return $ "constraints-returned-to-body:" <> lineIndent doc
          combineBool accessAndTensor evalAnd remainingBody leftovers

      searchExpr <- compileSearch p dims binder finalBody domain
      return $ singletonPartition (outOfScope, Just searchExpr)
    NonTrivial <$> disjunctPartitions IDimNil newPartitions

compileSearch ::
  (MonadDomain m) =>
  Provenance ->
  Expr Builtin ->
  Binder Builtin ->
  Expr Builtin ->
  Domain TensorConstantValue ->
  m (Expr Builtin)
compileSearch p dims binder closure (Domain lowerBound upperBound) = do
  -- Create the final expression
  -- NOTE that this is unsound as we discard the strictness information.
  lowerBound' <- unnormaliseInTensorCtx =<< foldConstant (lowerBoundValue lowerBound)
  upperBound' <- unnormaliseInTensorCtx =<< foldConstant (upperBoundValue upperBound)

  let spine =
        mkExpr accessSpine $
          SearchRatTensorArgs
            { searchDims = dims,
              searchLowerBound = liftDBIndices (-1) lowerBound',
              searchUpperBound = liftDBIndices (-1) upperBound',
              searchPredicate = Lam p binder closure
            }
  return $ normAppList (Builtin p (BuiltinFunction $ QuantifyRatTensor Exists)) spine

findTensorBounds ::
  forall m.
  (MonadDomain m) =>
  NestedSliceVariable ->
  (KnownPrefixOfTensorShape, Thunk Builtin) ->
  Maybe UserVariableConstraintTree ->
  m (DisjunctAll (TensorBounds TensorConstantValue, Maybe UserVariableConstraintTree))
findTensorBounds parentVar (parentVarShapePrefix, _parentVarRemainingShape) constraints =
  go (DisjunctAll [(emptyBounds, constraints)]) parentVar
  where
    go ::
      DisjunctAll (TensorBounds TensorConstantValue, Maybe UserVariableConstraintTree) ->
      NestedSliceVariable ->
      m (DisjunctAll (TensorBounds TensorConstantValue, Maybe UserVariableConstraintTree))
    go allBounds var = do
      result <- forM allBounds $ \(bounds, maybeTree) ->
        case maybeTree of
          Nothing ->
            return $ DisjunctAll [(bounds, Nothing)]
          Just tree -> do
            let tensorVar = TensorVariable $ toSliceVar parentVar
            let indices = findSliceIndices parentVar var
            let varInfo = VariableInfo tensorVar parentVarShapePrefix indices
            disjunctedBoundsAndRemainders <- findAllBounds (findVarBound var varInfo) tree
            let finalBoundsAndRemainders = fmap (first (andBounds bounds)) disjunctedBoundsAndRemainders
            case childVariablesOf var of
              Nothing -> return finalBoundsAndRemainders
              Just childVariables -> foldM go finalBoundsAndRemainders childVariables
      return $ disjunctDisjuncts result

findVarBound ::
  (MonadDomain m) =>
  NestedSliceVariable ->
  VariableInfo ->
  UserVariableConstraint ->
  m (Maybe (TensorBounds TensorConstantValue))
findVarBound var info@VariableInfo {..} (NormalisedRelation rel expr, _originalValue)
  | not (expr `containsVariable` toSliceVar var) = findBroadcastVarBound var info rel expr
  | otherwise = do
      (coef, expr') <- rearrangeExprToSolveFor (toSliceVar var) expr
      validBoundOr Nothing parentVariable expr' $ do
        boundExpr <- tensorValueLinearExprToValue expr'
        Just <$> convertToTensorBounds parentShape indices rel coef boundExpr

-- | A bound is emitted outside the binder it bounds, so it cannot mention the tensor being bounded,
-- e.g. `x ! 0 < x ! 1`, nor anything bound inside it. Both live at a level at or above the tensor's
-- own, as the slices of a tensor occupy a contiguous range of levels.
validBoundOr ::
  (MonadDomain m) =>
  a ->
  TensorVariable ->
  TensorValueLinearExpr ->
  m a ->
  m a
validBoundOr invalid parentVariable boundExpr action = do
  occurrences <- linearExprTensorOccurrences boundExpr
  if any (>= toLv parentVariable) (Map.keys occurrences)
    then return invalid
    else action

-- | A variable can also be bounded when it appears only as a broadcast inside the constant, e.g.
-- `forall i . x ! i <= y ! 0` becomes `x <= const (y ! 0) [3]`. Broadcasting is adjoint to
-- reduction, so `(forall k . a_k <= b) <=> (max_k a_k) <= b`, and the bound for the broadcast slice
-- is the other side of the assertion reduced over the dimensions it was broadcast across.
findBroadcastVarBound ::
  (MonadDomain m) =>
  NestedSliceVariable ->
  VariableInfo ->
  Relation ->
  TensorValueLinearExpr ->
  m (Maybe (TensorBounds TensorConstantValue))
findBroadcastVarBound _var VariableInfo {..} rel expr = do
  let TensorConstantValue dims coefficient maybeValue = constantValue expr
  maybeBroadcast <- traverse (isBroadcastOfSlice (toLv parentVariable) indices) maybeValue
  case (maybeBroadcast, coefficient) of
    (Just True, Finite coef) | coef /= 0 -> do
      -- Drop the broadcast term from the constant and solve the rest for it.
      let rest = expr {constantValue = TensorConstantValue dims 0 Nothing}
      validBoundOr Nothing parentVariable rest $ do
        restValue <- tensorValueLinearExprToValue rest
        scaledRest <- scaleConstant (-(1 / coef)) restValue
        scaledThunk <- foldConstant scaledRest
        -- A negative coefficient puts the broadcast on the greater side, making this a lower bound.
        let strictness = if rel == OLt then Strict else NonStrict
        lower <- mkReducedBound reduceMaxThunks dims scaledThunk
        upper <- mkReducedBound reduceMinThunks dims scaledThunk
        let sliceBounds = case rel of
              OEq -> SliceBounds [LowerBound NonStrict lower] [UpperBound NonStrict upper]
              _
                | coef < 0 -> SliceBounds [LowerBound strictness lower] mempty
                | otherwise -> SliceBounds mempty [UpperBound strictness upper]
        return $ Just $ TensorBounds $ singleNestedSliceBound sliceBounds parentShape indices
    _ -> return Nothing
  where
    mkReducedBound reduceFn dims thunk = do
      reduced <- reduceFn dims thunk
      return $ TensorConstantValue (Forced IDimNil) 1 (Just reduced)

-- | Whether a term is exactly `tensor ! indices` broadcast across some dimensions. The slice is
-- written as a projection out of the whole tensor rather than as a slice variable of its own, so it
-- has to be decoded rather than looked up.
isBroadcastOfSlice :: (MonadDomain m) => Lv -> TensorIndices -> Thunk Builtin -> m Bool
isBroadcastOfSlice tensorLv indices thunk = do
  forced <- forceAndRewriteTensor thunk
  case toRatTensorValue forced of
    VRatConstTensor (ConstTensorArgs _ payload _) -> do
      projected <- projectionOf tensorLv payload
      return $ projected == Just indices
    _ -> return False

-- | The indices a term projects out of the given tensor variable, if that is all it does.
projectionOf :: (MonadDomain m) => Lv -> Thunk Builtin -> m (Maybe TensorIndices)
projectionOf tensorLv thunk = do
  forced <- forceAndRewriteTensor thunk
  case toRatTensorValue forced of
    VRatTensorBoundVar lv [] | lv == tensorLv -> return $ Just []
    VRatAtTensor (AtTensorArgs _ _ _ tensor index) -> do
      forcedIndex <- forceThunk index
      case forcedIndex of
        IIndexLiteral i _ -> do
          rest <- projectionOf tensorLv tensor
          return $ fmap (<> [i]) rest
        _ -> return Nothing
    _ -> return Nothing

tensorValueLinearExprToValue ::
  (MonadNorm Builtin m) =>
  LinearExpr SliceVariable TensorConstantValue ->
  m TensorConstantValue
tensorValueLinearExprToValue linearExpr = do
  let dims = tensorValueDims $ constantValue linearExpr
  let mkTerm (v, coeff) = mkTensorConstantValue dims (Finite coeff) (Forced $ VBoundVar (toLv v) [])
  linearExprToExpr id mkTerm (addConstants 1 1) linearExpr

-- | A bound is emitted outside the binder it bounds, so an assertion mentioning the variables of
-- the quantifier currently being compiled cannot travel outwards with the rest of the constraints.
-- Split those off so that they can be put back into the quantifier's body, where the variables they
-- mention are still in scope.
--
-- A disjunction cannot be split, so it stays inside if any of its disjuncts has to.
splitTreeByScope ::
  (MonadDomain m) =>
  Lv ->
  UserVariableConstraintTree ->
  m (Maybe UserVariableConstraintTree, Maybe UserVariableConstraintTree)
splitTreeByScope quantifierLevel = go
  where
    go tree = case tree of
      Query (assertion, _) -> do
        occurrences <- assertionTensorOccurrences assertion
        let mentionsQuantifier = any (>= quantifierLevel) (Map.keys occurrences)
        return $
          if mentionsQuantifier
            then (Just tree, Nothing)
            else (Nothing, Just tree)
      Conjunct (ConjunctAll xs) -> do
        results <- traverse go xs
        let combine f = combineWith andBoolExpr $ mapMaybe f $ toList results
        return (combine fst, combine snd)
      Disjunct (DisjunctAll xs) -> do
        results <- traverse go xs
        return $
          if any (isJust . fst) results
            then (Just tree, Nothing)
            else (Nothing, Just tree)

    combineWith f = \case
      [] -> Nothing
      x : xs -> Just $ foldr f x xs

-- | Put the terms the assertions were derived from back together, so that they can be emitted into
-- the body of the quantifier whose variables they mention.
treeToExpr :: (MonadDomain m) => UserVariableConstraintTree -> m (Expr Builtin)
treeToExpr = \case
  Query (_, value) -> unnormaliseInTensorCtx value
  Conjunct (ConjunctAll xs) -> foldrM1 (combineBool accessAndTensor evalAnd) =<< traverse treeToExpr xs
  Disjunct (DisjunctAll xs) -> foldrM1 (combineBool accessOrTensor evalOr) =<< traverse treeToExpr xs

combineBool ::
  (MonadDomain m) =>
  Accessor (Expr Builtin) (TensorOp2Args (Expr Builtin)) ->
  EvalSimple Expr Expr TensorOp2Args Builtin m ->
  Expr Builtin ->
  Expr Builtin ->
  m (Expr Builtin)
combineBool accessor evalFn x y = forceEvaluation accessor evalFn $ TensorOp2Args IDimNil x y

--------------------------------------------------------------------------------
-- Constraint search
--------------------------------------------------------------------------------
-- Definitions

-- | Note that the constraints live in the extended tensor context, where as the remaining value lives
-- in the original unextended context.
type Partition = (Maybe UserVariableConstraintTree, Maybe (Expr Builtin))

andPartition :: (MonadDomain m) => Expr Builtin -> Partition -> Partition -> m Partition
andPartition dims (c1, v1) (c2, v2) = do
  let c = unionMaybeWith andBoolExpr c1 c2
  v <- unionMaybeWithM (\x y -> forceEvaluation accessAndTensor evalAnd $ TensorOp2Args dims x y) v1 v2
  return (c, v)

-- | Note that the constraints live in the extended tensor context, where as the remaining value lives
-- in the original unextended context.
type Partitions = Map (Maybe UserVariableConstraintTree) (Maybe (Expr Builtin))

prettyPartitionsM :: (MonadDomain m) => MaybeTrivial Partitions -> m (Doc a)
prettyPartitionsM partitions = do
  shrunkenCtx <- toNamedBoundCtx <$> getShrunkenContext
  fullCtx <- getNameContext
  let prettyTree t = prettyFriendly (WithContext t fullCtx)
  let prettyRemainder t = prettyFriendly (WithContext t shrunkenCtx)
  return $ prettyMaybeTrivial (prettyMap prettyTree prettyRemainder) partitions

singletonUnconstrainedPartition :: (MonadDomain m) => Thunk Builtin -> m Partitions
singletonUnconstrainedPartition nonDomainConstraint = do
  logDebugM MaxDetail $ do
    doc <- prettyFriendlyInCtx nonDomainConstraint
    return $ "Found non-domain constraint:" <+> doc
  convertedNonDomainConstraint <- unnormaliseInTensorCtx nonDomainConstraint
  return $ Map.singleton Nothing (Just convertedNonDomainConstraint)

singletonPartition :: Partition -> Partitions
singletonPartition (tree, value) = Map.singleton tree value

numberOfPartitions :: Partitions -> Int
numberOfPartitions = length

containsConstraints :: Partitions -> Bool
containsConstraints partitions = case Map.toList partitions of
  [(Nothing, _)] -> False
  _ -> True

notPartitions :: forall m. (MonadDomain m) => Expr Builtin -> Partitions -> m Partitions
notPartitions dims partitions = do
  -- Negate each individual partition
  negPartitions <- traverse notPartition $ partitionsToDisjuncts partitions
  -- Conjunct the results together
  result <- foldrM1 (andPartition dims) $ unDisjunctAll negPartitions
  -- Store in a single partition
  return $ singletonPartition result
  where
    notPartition :: Partition -> m Partition
    notPartition (constraintTree, value) = do
      notConstraintTree <- traverse (traverse notConstraint) constraintTree
      notValue <- traverse (forceEvaluation accessNotTensor evalNot . TensorOp1Args dims) value
      return (fmap flattenBoolExpr notConstraintTree, notValue)

    -- NOTE: the `OEq` case produces `expr <= 0 or -expr <= 0`, which is a tautology. Negating
    -- `expr == 0` should give strict inequalities. Left as-is, as fixing it is a separate concern.
    notConstraint :: UserVariableConstraint -> m (BooleanExpr UserVariableConstraint)
    notConstraint (NormalisedRelation rel expr, value) = do
      negExpr <- scaleExpr (-1) expr
      -- The term an assertion came from is a scalar comparison, so it is negated at nil dimensions
      -- rather than at the dimensions of the partition it sits in.
      negValue <- forceEvaluation accessNotTensor evalNot $ TensorOp1Args (Forced IDimNil) value
      return $ case rel of
        OLe -> Query (NormalisedRelation OLt negExpr, negValue)
        OLt -> Query (NormalisedRelation OLe negExpr, negValue)
        OEq -> do
          let less = NormalisedRelation OLe expr
          let greater = NormalisedRelation OLe negExpr
          Disjunct $ DisjunctAll [Query (less, negValue), Query (greater, negValue)]

partitionsToDisjuncts :: Partitions -> DisjunctAll Partition
partitionsToDisjuncts ps = case Map.toList ps of
  [] -> developerError "Empty partition"
  x : xs -> DisjunctAll $ x :| xs

disjunctPartitions :: (MonadDomain m) => Expr Builtin -> DisjunctAll Partitions -> m Partitions
disjunctPartitions dims (DisjunctAll (p :| ps)) = foldrM (orPartitions dims) p ps

disjunctMaybeTrivialPartitions :: (MonadDomain m) => Expr Builtin -> DisjunctAll (MaybeTrivial Partitions) -> m (MaybeTrivial Partitions)
disjunctMaybeTrivialPartitions dims = traverse (disjunctPartitions dims) . eliminateTrivialDisjunctions

orPartitions :: (MonadDomain m) => Expr Builtin -> Partitions -> Partitions -> m Partitions
orPartitions dims = unionWithM (unionMaybeWithM (\x y -> forceEvaluation accessOrTensor evalOr $ TensorOp2Args dims x y))

andPartitions :: (MonadDomain m) => Expr Builtin -> Partitions -> Partitions -> m Partitions
andPartitions dims p1 p2 = do
  disjuncts <- conjunctDisjunctsM (andPartition dims) (partitionsToDisjuncts p1) (partitionsToDisjuncts p2)
  return $ Map.fromList $ disjunctsToList disjuncts

--------------------------------------------------------------------------------
-- Search algorithm

compileBool :: (MonadDomain m) => Thunk Builtin -> m (MaybeTrivial Partitions)
compileBool value = logEntryAndExit value $ do
  forcedValue <- forceAndRewriteTensor value
  result <- case toBoolValue forcedValue of
    -----------------------
    -- Useful base cases --
    -----------------------
    VCompareRatTensor args -> compileComparison args
    --------------------------
    -- Un-useful base cases --
    --------------------------
    VBoolLiteral b -> return $ Trivial b
    VCompareNat args -> traverse singletonUnconstrainedPartition =<< purifyNatComparison args
    VCompareIndex args -> traverse singletonUnconstrainedPartition =<< purifyIndexComparison args
    ---------------------
    -- Recursive cases --
    ---------------------
    VImplies args -> compileBool $ elimImplies args
    VAnd args -> compileAnd args
    VOr args -> compileOr args
    VQuantifyRatTensor args -> compileQuantifierInternal mempty args
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

  -- In the case where we only return one partition with no constraints then
  -- we can just return the unnormalised result.
  case result of
    NonTrivial (Map.toList -> [(Nothing, _)]) -> NonTrivial <$> singletonUnconstrainedPartition value
    _ -> return result
  where
    unblock forced = unblockWith (unblockBoolExpr unblockingActions (Forced forced)) (Forced forced)

compileAnd ::
  (MonadDomain m) =>
  TensorOp2Args (Thunk Builtin) ->
  m (MaybeTrivial Partitions)
compileAnd (TensorOp2Args dims e1 e2) = do
  dims' <- unnormaliseInTensorCtx dims
  c1 <- compileBool e1
  c2 <- compileBool e2
  andTrivialM (andPartitions dims') c1 c2

compileOr ::
  (MonadDomain m) =>
  TensorOp2Args (Thunk Builtin) ->
  m (MaybeTrivial Partitions)
compileOr (TensorOp2Args dims e1 e2) = do
  dims' <- unnormaliseInTensorCtx dims
  c1 <- compileBool e1
  c2 <- compileBool e2
  orTrivialM (orPartitions dims') c1 c2

-- | A comparison may be compiled to a potential bound as long as:
-- * It does not contain any network applications (e.g. f x < 0.5)
-- * It does not compare slices from the same user tensor (e.g. x ! 0 < x ! 1)
compileComparison ::
  forall m.
  (MonadDomain m) =>
  (ComparisonOp, TensorComparisonArgs (Thunk Builtin)) ->
  m (MaybeTrivial Partitions)
compileComparison (op, args) =
  logCompilerSection2 MaxDetail "assertion compilation" $
    if op == Ne
      then traverse singletonUnconstrainedPartition =<< purifyNotEqualRatTensorComparison args
      else do
        blockedValueOrResult <- tryPurifyRatTensorComparison op args
        elimIfTree (compileBranch (tensorPointwiseDims args)) compileLeaf blockedValueOrResult
  where
    compileBranch ::
      UnforcedDims Builtin ->
      Thunk Builtin ->
      MaybeTrivial Partitions ->
      MaybeTrivial Partitions ->
      m (MaybeTrivial Partitions)
    compileBranch dims c x y = do
      dims' <- unnormaliseInTensorCtx dims
      c' <- compileBool c
      notC' <- compileBool (Forced <$> mkExpr accessNotTensor $ TensorOp1Args (Forced IDimNil) c)
      cAndx <- andTrivialM (andPartitions dims') c' x
      notCAndy <- andTrivialM (andPartitions dims') notC' y
      orTrivialM (orPartitions dims') cAndx notCAndy

    compileLeaf ::
      MaybeTrivial (Thunk Builtin, Maybe (Assertion TensorValueLinearExpr)) ->
      m (MaybeTrivial Partitions)
    compileLeaf = \case
      Trivial b -> return $ Trivial b
      NonTrivial (value, maybeEquivAssertion) ->
        NonTrivial <$> case maybeEquivAssertion of
          Nothing -> singletonUnconstrainedPartition value
          Just assertion
            | Map.null $ coefficients $ expression assertion -> singletonUnconstrainedPartition value
            | otherwise -> do
                let partitions = Map.singleton (Just (Query (assertion, value))) Nothing
                logDebugM MaxDetail $ do
                  doc <- prettyFriendlyInCtx assertion
                  return $ "Found domain constraint:" <+> doc
                return partitions

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
      NonTrivial <$> singletonUnconstrainedPartition defaultValue
    Right unblockedExpr -> do
      -- If we can unblock it then try to continue compilation
      maybePartitions <- compileBool unblockedExpr
      case maybePartitions of
        NonTrivial partitions
          | not (containsConstraints partitions) ->
              NonTrivial <$> singletonUnconstrainedPartition defaultValue
        _ -> return maybePartitions

--------------------------------------------------------------------------------
-- Utils

extractKnownShape ::
  forall m.
  (MonadDomain m) =>
  Thunk Builtin ->
  m (KnownPrefixOfTensorShape, Thunk Builtin)
extractKnownShape value = do
  forcedValue <- forceThunk value
  case forcedValue of
    IDimCons d ds -> do
      forcedDim <- forceThunk d
      case forcedDim of
        INatLiteral n -> first (n :) <$> extractKnownShape ds
        _ -> return ([], value)
    _ -> return ([], value)

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
    doc <- prettyPartitionsM result
    return $ "search-exit:" <+> lineIndent doc
  return result

currentPass :: Doc a
currentPass = "quantifier bounds extraction"
