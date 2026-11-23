module Vehicle.Backend.Solver.QueryCompilation.MetaNetworkCalculation
  ( calculateMetaNetworkApplications,
  )
where

import Control.Monad (foldM, forM, unless)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.Trans.Writer.CPS (runWriterT)
import Control.Monad.Writer.Class (MonadWriter (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Coerce (Coercible, coerce)
import Data.Containers.ListUtils (nubOrd)
import Data.DisjointSet (DisjointSet)
import Data.DisjointSet qualified as DisjointSet
import Data.Either (lefts)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Vehicle.Backend.Solver.QueryCompilation.Core (MonadQueryCompilation, getNetworkApplications, lookupCorrespondingOutputVar)
import Vehicle.Backend.Solver.UserVariableElimination.Core
import Vehicle.Compile.Constants.Rational
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Data.Assertion
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.MaybeTrivial
import Vehicle.Data.Tensor as Tensor
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Data.Variable.Bound.Context.Tensor.Class
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Verify.QueryFormat (QueryFormat (..))
import Vehicle.Verify.Specification (CompilationStep (..))

calculateMetaNetworkApplications ::
  (MonadQueryCompilation m) =>
  ConjunctAll LinearAssertion ->
  m (MaybeTrivial (NetworkApplications, ConjunctAll LinearAssertion, [CompilationStep]))
calculateMetaNetworkApplications assertions = do
  (eliminationResult, compilationSteps) <- runWriterT $ eliminateRedundantApplications assertions
  case eliminationResult of
    Trivial b -> return $ Trivial b
    NonTrivial newAssertions -> do
      networkApps <- calculateMetaNetworkApps newAssertions
      checkIfMetaNetworkSupported networkApps
      return $ NonTrivial (networkApps, newAssertions, compilationSteps)

--------------------------------------------------------------------------------
-- Redundant network applications

type EquivalenceClasses = DisjointSet (Either NetworkInputTensorVariable RatTensor)

prettyEquivalenceClasses :: (MonadQueryCompilation m) => EquivalenceClasses -> m (Doc a)
prettyEquivalenceClasses classes = do
  let classLists = DisjointSet.toLists classes
  let prettyEntry = eitherM prettyFriendlyInCtx (return . pretty)
  classDocs <- traverse (fmap prettyFlatList . traverse prettyEntry) classLists
  return $ prettyMultiLineList classDocs

type SimpleEquality = (SliceVariable, Either SliceVariable RatTensor)

eliminateRedundantApplications ::
  (MonadQueryCompilation m, MonadWriter [CompilationStep] m) =>
  ConjunctAll LinearAssertion ->
  m (MaybeTrivial (ConjunctAll LinearAssertion))
eliminateRedundantApplications assertions =
  logCompilerSection2 MaxDetail "checking for redundant network applications" $ do
    let equalities = mapMaybe isSimpleVariableEquality $ conjunctsToList assertions
    logEqualitiesFound equalities

    applicationsByNetwork <- Map.toList <$> getNetworkApplications

    -- Only compute the equivalence classes of the slice variables if there is
    -- some network with more than one application, as otherwise there is
    -- nothing to eliminate.
    let hasMultipleApplications = any (\(_, apps) -> length apps > 1) applicationsByNetwork
    sliceClasses <-
      if hasMultipleApplications
        then calculateSliceEquivalenceClasses equalities
        else return DisjointSet.empty

    equivalenceClasses <- forM applicationsByNetwork $ \(networkName, applications) ->
      logCompilerSection2 MaxDetail ("checking applications of network" <+> quotePretty networkName) $ do
        logDebug MaxDetail $ pretty (length applications) <+> "application found" <> line
        if length applications == 1
          then return mempty
          else calculateNetworkTensorInputEquivalenceClasses sliceClasses applications

    logDebugM MaxDetail $ do
      classesDoc <- traverse prettyEquivalenceClasses equivalenceClasses
      return $ "equivalenceClasses:" <> lineIndent (prettyMultiLineList classesDoc)

    -- Calculate the substitution to perform
    subst <- logCompilerSection MaxDetail "Calculating substitution:" $ do
      subst <- createSubstitutionFromEquivalenceClasses equivalenceClasses
      logDebugM MaxDetail $ prettyFriendlyInCtx subst
      return subst

    -- Perform the substitution
    let resultingAssertions =
          if Map.null subst
            then NonTrivial assertions
            else removeDuplicateConjuncts <$> eliminateTrivialConjunctions (fmap (eliminateVarsInComparison subst) assertions)

    logDebugM MaxDetail $ do
      assertionsDoc <- prettyFriendlyInCtx resultingAssertions
      return $ "Result:" <> lineIndent assertionsDoc
    return resultingAssertions

-- | Removes duplicate assertions, which may be created when substituting
-- away redundant network applications (e.g. if `g₀[input] == f₀[input]` and
-- `g₁[input] == f₀[input]` then substituting `g₁` with `g₀` will duplicate
-- the former).
removeDuplicateConjuncts :: ConjunctAll LinearAssertion -> ConjunctAll LinearAssertion
removeDuplicateConjuncts (ConjunctAll (x :| xs)) = ConjunctAll (x :| nubOrd (filter (/= x) xs))

-- | Finds equality assertions of the form `a - b == 0` (i.e. `a == b`)
isSimpleVariableEquality :: LinearAssertion -> Maybe SimpleEquality
isSimpleVariableEquality = \case
  NormalisedRelation OEq (Sparse coefficients constant) -> do
    case Map.toList coefficients of
      [(v1, a), (v2, b)] | constant `isTensorOfAll` 0 && a == -b -> Just (v1, Left v2)
      [(v1, a)] -> Just (v1, Right $ mapTensor (/ a) constant)
      _ -> Nothing
  _ -> Nothing

-- | The equivalence classes of slice variables and constants implied by the
-- simple equalities in a query.
type SliceEquivalenceClasses = DisjointSet (Either SliceVariable RatTensor)

-- | Calculates the equivalence classes of slice variables implied by the
-- provided equalities. Note:
--
--   1. The equalities may relate the inputs and outputs of any network, so
--      equalities are found transitively via the variables of other networks,
--      e.g. `g₀[input] == f₀[input]` and `g₁[input] == f₀[input]` imply
--      `g₀[input] == g₁[input]`.
--
--   2. The classes are closed downwards, i.e. if two tensors are equal then
--      so are their corresponding slices, and if a tensor is equal to a
--      constant then each of its slices is equal to the corresponding slice of
--      the constant.
calculateSliceEquivalenceClasses ::
  (MonadQueryCompilation m) =>
  [SimpleEquality] ->
  m SliceEquivalenceClasses
calculateSliceEquivalenceClasses = foldM addEquality DisjointSet.empty
  where
    addEquality ::
      (MonadQueryCompilation m) =>
      SliceEquivalenceClasses ->
      SimpleEquality ->
      m SliceEquivalenceClasses
    addEquality classes (v1, value) = do
      nestedVar1 <- lookupNestedSliceVariable v1
      case value of
        Left v2 -> do
          nestedVar2 <- lookupNestedSliceVariable v2
          return $ unionVariables nestedVar1 nestedVar2 classes
        Right constant ->
          return $ unionConstant nestedVar1 constant classes

    unionVariables :: NestedSliceVariable -> NestedSliceVariable -> SliceEquivalenceClasses -> SliceEquivalenceClasses
    unionVariables x y classes = do
      let newClasses = DisjointSet.union (Left (toSliceVar x)) (Left (toSliceVar y)) classes
      case (childVariablesOf x, childVariablesOf y) of
        (Just xs, Just ys) -> foldr (uncurry unionVariables) newClasses (zip xs ys)
        _ -> newClasses

    unionConstant :: NestedSliceVariable -> RatTensor -> SliceEquivalenceClasses -> SliceEquivalenceClasses
    unionConstant x constant classes = do
      let newClasses = DisjointSet.union (Left (toSliceVar x)) (Right constant) classes
      let constantSlices = unstack constant
      case childVariablesOf x of
        Just xs -> foldr (uncurry unionConstant) newClasses (zip xs constantSlices)
        _ -> newClasses

-- | Partitions the applications of a network into classes whose inputs are
-- known to be equal. Two inputs are equal if they are in the same equivalence
-- class, or if all of their corresponding slices are equal.
calculateNetworkTensorInputEquivalenceClasses ::
  forall m.
  (MonadQueryCompilation m) =>
  SliceEquivalenceClasses ->
  NonEmpty NetworkApplicationInfo ->
  m EquivalenceClasses
calculateNetworkTensorInputEquivalenceClasses sliceClasses applications = do
  inputVariables <- forM (NonEmpty.toList applications) $ \app -> do
    let inputVar = inputVariable app
    nestedVar <- lookupNestedTensorVariable inputVar
    return (inputVar, nestedVar)
  let classes = foldl addToClass [] inputVariables
  return $ DisjointSet.fromLists (fmap (fmap (Left . fst)) classes)
  where
    addToClass ::
      [[(NetworkInputTensorVariable, NestedSliceVariable)]] ->
      (NetworkInputTensorVariable, NestedSliceVariable) ->
      [[(NetworkInputTensorVariable, NestedSliceVariable)]]
    addToClass [] v = [[v]]
    addToClass (c : cs) v = case c of
      (_, representative) : _ | areEqual representative (snd v) -> (c <> [v]) : cs
      _ -> c : addToClass cs v

    areEqual :: NestedSliceVariable -> NestedSliceVariable -> Bool
    areEqual x y
      | toSliceVar x == toSliceVar y = True
      | DisjointSet.equivalent (Left (toSliceVar x)) (Left (toSliceVar y)) sliceClasses = True
      | otherwise = case (childVariablesOf x, childVariablesOf y) of
          (Just xs, Just ys) -> and (zipWith areEqual xs ys)
          _ -> False

createSubstitutionFromEquivalenceClasses ::
  (MonadQueryCompilation m, MonadWriter [CompilationStep] m) =>
  [EquivalenceClasses] ->
  m (LinearSubstitution SliceVariable)
createSubstitutionFromEquivalenceClasses equivalenceClasses = do
  let allClasses = concatMap DisjointSet.toSets equivalenceClasses
  let tensorLevelEqualities = concatMap go allClasses
  substitutions <- traverse reduceInputVariableEquality tensorLevelEqualities
  return $ Map.unions substitutions
  where
    go :: Set (Either NetworkInputTensorVariable RatTensor) -> [(NetworkInputTensorVariable, NetworkInputTensorVariable)]
    go xs = case sort (lefts $ Set.toList xs) of
      v : vs -> fmap (,v) vs
      [] -> developerError "Disjoint sets should not contain empty equivalence classes"

reduceInputVariableEquality ::
  (MonadQueryCompilation m, MonadWriter [CompilationStep] m) =>
  (NetworkInputTensorVariable, NetworkInputTensorVariable) ->
  m (LinearSubstitution SliceVariable)
reduceInputVariableEquality (eqInputVar, inputVar) = do
  -- Construct the input variable substitution
  inputEq <- createEq inputVar eqInputVar
  (inputSubst, inputCompilationStep) <- createSubstitutionForVariable eqInputVar inputEq

  -- Construct the output variable substitution
  outputVar <- lookupCorrespondingOutputVar inputVar
  eqOutputVar <- lookupCorrespondingOutputVar eqInputVar
  outputEq <- createEq outputVar eqOutputVar
  (outputSubst, outputCompilationStep) <- createSubstitutionForVariable eqOutputVar outputEq

  -- Note the compilation steps
  tell [outputCompilationStep, inputCompilationStep]

  return (inputSubst <> outputSubst)
  where
    createEq ::
      (MonadQueryCompilation m) =>
      (TensorVariableLike variable) =>
      variable ->
      variable ->
      m LinearEquality
    createEq v1 v2 = do
      tensorShape <- shapeOf <$> lookupNestedTensorVariable v1
      let constant = ConstantTensor tensorShape 0
      let coefficients = Map.fromList [(toSliceVar v1, -1), (toSliceVar v2, 1)]
      return $ NormalisedRelation () $ Sparse coefficients constant

--------------------------------------------------------------------------------
-- Calculate the meta-network

calculateMetaNetworkApps ::
  (MonadQueryCompilation m, Traversable f) =>
  f LinearAssertion ->
  m (Map Name (NonEmpty NetworkApplicationInfo))
calculateMetaNetworkApps assertions = do
  -- First calculate the set of network applications actually used in the query
  let usedSliceVariables = foldMap variablesOf assertions
  usedTensorVariables <- lookupParentTensorVariables usedSliceVariables
  -- Then filter the network applications
  Map.mapMaybe (filterApplications usedTensorVariables) <$> getNetworkApplications
  where
    filterApplication :: Set TensorVariable -> NetworkApplicationInfo -> Bool
    filterApplication usedVars NetworkApplicationInfo {..} =
      Set.member (toTensorVar inputVariable) usedVars || Set.member (toTensorVar outputVariable) usedVars

    filterApplications :: Set TensorVariable -> NonEmpty NetworkApplicationInfo -> Maybe (NonEmpty NetworkApplicationInfo)
    filterApplications usedVars apps = NonEmpty.nonEmpty (NonEmpty.filter (filterApplication usedVars) apps)

logEqualitiesFound ::
  (MonadLogger m, MonadQueryCompilation m, Coercible var Lv) =>
  [(var, Either var RatTensor)] ->
  m ()
logEqualitiesFound equalities = do
  logDebugM MaxDetail $ do
    nameCtx <- getCompleteNamedCtx
    return $
      if null equalities
        then "No suitable equalities found"
        else "Possible equalities:" <> lineIndent (vsep (fmap (prettyEquality nameCtx) equalities)) <> line

prettyEquality ::
  (Coercible var Lv) =>
  CompleteNamedBoundCtx ->
  (var, Either var RatTensor) ->
  Doc a
prettyEquality ctx (a, b) = do
  let prettyVar v = pretty $ lookupLvInBoundCtx (coerce v) ctx
  prettyVar a <+> "==" <+> either prettyVar pretty b

--------------------------------------------------------------------------------
-- Compatability

-- | Check if the query format supports the current meta-network configuration
checkIfMetaNetworkSupported ::
  (MonadQueryCompilation m) =>
  NetworkApplications ->
  m ()
checkIfMetaNetworkSupported metaNetworkApps = do
  (PropertyMetaData {..}, _) <- ask
  unless (supportsMultipleNetworks queryFormat) $ do
    case toListOfApplications metaNetworkApps of
      [] -> developerError "was not expecting an empty list of meta-network applications"
      [_app] -> return ()
      apps -> do
        let formatID = queryFormatID queryFormat
        let appsWithValues = fmap (second inputValue) apps
        nameCtx <- getCompleteNamedCtx
        throwError $ UnsupportedMultipleNetworkApplications formatID propertyProvenance nameCtx appsWithValues
