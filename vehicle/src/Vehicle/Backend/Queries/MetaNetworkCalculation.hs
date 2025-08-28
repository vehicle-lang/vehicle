{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Backend.Queries.MetaNetworkCalculation
  ( calculateMetaNetworkApplications,
  )
where

import Control.Monad (forM)
import Control.Monad.Trans.Writer (WriterT (..))
import Control.Monad.Writer (MonadWriter (..))
import Data.Coerce (Coercible, coerce)
import Data.DisjointSet (DisjointSet)
import Data.DisjointSet qualified as DisjointSet
import Data.Either (lefts)
import Data.List (sort, transpose)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Vehicle.Backend.Queries.UserVariableElimination.Core
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Data.Assertion
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.QuantifiedVariable
import Vehicle.Data.Tensor as Tensor
import Vehicle.Verify.Specification (CompilationStep (..))

calculateMetaNetworkApplications ::
  (MonadCompile m) =>
  GlobalCtx ->
  ConjunctAll (Assertion SliceVariable) ->
  m (MaybeTrivial (NetworkApplications, ConjunctAll (Assertion SliceVariable), [CompilationStep]))
calculateMetaNetworkApplications ctx assertions = do
  (eliminationResult, compilationSteps) <- runWriterT $ eliminateRedundantApplications ctx assertions
  case eliminationResult of
    Trivial b -> return $ Trivial b
    NonTrivial newAssertions -> do
      let metaNetworkApps = calculateMetaNetworkApps ctx newAssertions
      return $ NonTrivial (metaNetworkApps, newAssertions, compilationSteps)

--------------------------------------------------------------------------------
-- Redundant network applications

-- | A mapping from the current variable at some position in the tensor back
-- to the original variable that represents the whole tensor
--   e.g. { (x_00 -> x), (y_00 -> y), (z_00 -> z) }
type TensorVariableMapping = Map NestedSliceVariable NetworkInputTensorVariable

childTensorVariableMappings :: TensorVariableMapping -> Maybe [TensorVariableMapping]
childTensorVariableMappings mapping = do
  let us = Map.toList mapping
  let vs = mapM (\(u, v) -> fmap (,v) (childVariablesOf u)) us
  case vs of
    Nothing -> Nothing
    Just zs -> Just $ do
      let xs = transpose $ fmap (\(u, v) -> fmap (,v) u) zs
      fmap Map.fromList xs

type EquivalenceClasses = DisjointSet (Either NetworkInputTensorVariable RatTensor)

prettyEquivalenceClasses :: CompleteNamedBoundCtx -> EquivalenceClasses -> Doc a
prettyEquivalenceClasses ctx classes = do
  let u = DisjointSet.toLists classes
  let prettyEntry = either (\v -> prettyFriendly (WithContext v ctx)) pretty
  prettyMultiLineList (fmap (\x -> prettyFlatList (fmap prettyEntry x)) u)

type SimpleEquality = (SliceVariable, Either SliceVariable RatTensor)

eliminateRedundantApplications ::
  (MonadCompile m, MonadWriter [CompilationStep] m) =>
  GlobalCtx ->
  ConjunctAll (Assertion SliceVariable) ->
  m (MaybeTrivial (ConjunctAll (Assertion SliceVariable)))
eliminateRedundantApplications ctx assertions =
  logCompilerSection2 MaxDetail "checking for redundant network applications" $ do
    let nameCtx = completeNamedCtx ctx
    let applicationsByNetwork = Map.toList $ networkApplications ctx

    let equalities = mapMaybe isSimpleVariableEquality $ conjunctsToList assertions
    logEqualitiesFound nameCtx equalities

    equivalenceClasses <- forM applicationsByNetwork $ \(networkName, applications) ->
      logCompilerSection2 MaxDetail ("checking applications of network" <+> quotePretty networkName) $ do
        logDebug MaxDetail $ pretty (length applications) <+> "application found" <> line
        if length applications == 1
          then return mempty
          else calculateNetworkTensorInputEquivalenceClasses ctx networkName equalities applications

    logDebug MaxDetail $ "equivalenceClasses:" <> lineIndent (prettyMultiLineList (fmap (prettyEquivalenceClasses (completeNamedCtx ctx)) equivalenceClasses))

    -- Calculate the substitution to perform
    subst <- logCompilerSection MaxDetail "Calculating substitution:" $ do
      subst <- createSubstitutionFromEquivalenceClasses ctx equivalenceClasses
      logDebug MaxDetail $ prettyFriendly (WithContext subst nameCtx)
      return subst

    -- Perform the substitution
    let resultingAssertions =
          if Map.null subst
            then NonTrivial assertions
            else eliminateTrivialConjunctions $ fmap (eliminateVarsInComparison subst) assertions

    logDebug MaxDetail $ "Result:" <> lineIndent (prettyFriendly (WithContext resultingAssertions nameCtx))
    return resultingAssertions

-- | Finds equality assertions of the form `a - b == 0` (i.e. `a == b`)
isSimpleVariableEquality ::
  Assertion SliceVariable ->
  Maybe SimpleEquality
isSimpleVariableEquality = \case
  NormalisedRelation OEq (Sparse coefficients constant) -> do
    case Map.toList coefficients of
      [(v1, a), (v2, b)] | constant `isTensorOfAll` 0 && a == -b -> Just (v1, Left v2)
      [(v1, a)] -> Just (v1, Right $ mapTensor (/ a) constant)
      _ -> Nothing
  _ -> Nothing

calculateNetworkTensorInputEquivalenceClasses ::
  forall m.
  (MonadLogger m) =>
  GlobalCtx ->
  Name ->
  [SimpleEquality] ->
  NonEmpty NetworkApplicationInfo ->
  m EquivalenceClasses
calculateNetworkTensorInputEquivalenceClasses ctx networkName equalities applications = do
  let inputVariables = inputVariable <$> NonEmpty.toList applications
  let initialEquivalenceClasses = DisjointSet.fromLists (fmap (\v -> [Left v]) inputVariables)
  let toEntry v = (lookupTensorVariable (globalBoundVarCtx ctx) v, v)
  let initialTensorVariableMapping = Map.fromList $ fmap toEntry inputVariables
  go mempty initialEquivalenceClasses initialTensorVariableMapping
  where
    go :: TensorIndices -> EquivalenceClasses -> TensorVariableMapping -> m EquivalenceClasses
    go tensorIndices equivalenceClasses tensorVariableMapping = do
      logCompilerSection2 MaxDetail ("search for tensor element input equalities for variable" <+> squotes (pretty networkName <> pretty (showTensorIndices (reverse tensorIndices)))) $ do
        -- Calculate the equivalence classes from the equalities you can find at this level
        expandedEquivalenceClasses <- expandEquivalenceClasses equalities tensorVariableMapping equivalenceClasses
        logDebug MaxDetail $ "equivalenceClasses:" <> lineIndent (prettyEquivalenceClasses (completeNamedCtx ctx) expandedEquivalenceClasses)

        if DisjointSet.sets expandedEquivalenceClasses == 1
          then do
            logDebug MaxDetail "all applications found to be equal"
            return expandedEquivalenceClasses
          else do
            -- Recursively calculate the equivalence classes you can find at the next level down.
            let maybeChildren = childTensorVariableMappings tensorVariableMapping
            case maybeChildren of
              Nothing -> return expandedEquivalenceClasses
              Just childMappings -> intersectEquivalenceClasses <$> forM (zip childMappings [0 ..]) (\(m, i) -> go (i : tensorIndices) expandedEquivalenceClasses m)

expandEquivalenceClasses ::
  (MonadLogger m) =>
  [SimpleEquality] ->
  Map NestedSliceVariable NetworkInputTensorVariable ->
  EquivalenceClasses ->
  m EquivalenceClasses
expandEquivalenceClasses equalities variables equivalenceClasses = do
  return $ foldr processEquality equivalenceClasses equalities
  where
    tensorVariableMap :: Map SliceVariable NetworkInputTensorVariable
    tensorVariableMap = Map.mapKeys toSliceVar variables

    processEquality ::
      SimpleEquality ->
      EquivalenceClasses ->
      EquivalenceClasses
    processEquality (v1, value) classes = case (Map.lookup v1 tensorVariableMap, value) of
      (Nothing, _) -> classes
      (Just iv1, Right v) -> DisjointSet.union (Left iv1) (Right v) classes
      (Just iv1, Left v2) -> case Map.lookup v2 tensorVariableMap of
        (Just iv2) -> DisjointSet.union (Left iv1) (Left iv2) classes
        Nothing -> classes

-- | Takes a list of intersection equivalence classes and returns the
-- intersection of the equivalence classes.
intersectEquivalenceClasses :: [EquivalenceClasses] -> EquivalenceClasses
intersectEquivalenceClasses [] = developerError "Cannot have empty equivalence classes"
intersectEquivalenceClasses (c : cs) = foldr intersect c cs
  where
    intersect :: EquivalenceClasses -> EquivalenceClasses -> EquivalenceClasses
    intersect xs ys = do
      let u = cartesianProduct Set.intersection (DisjointSet.toSets xs) (DisjointSet.toSets ys)
      case DisjointSet.fromSets u of
        Nothing -> developerError "Non-disjoint sets accidentally created"
        Just result -> result

createSubstitutionFromEquivalenceClasses ::
  (MonadCompile m, MonadWriter [CompilationStep] m) =>
  GlobalCtx ->
  [EquivalenceClasses] ->
  m (LinearSubstitution SliceVariable)
createSubstitutionFromEquivalenceClasses globalCtx equivalenceClasses = do
  let allClasses = concatMap DisjointSet.toSets equivalenceClasses
  let tensorLevelEqualities = concatMap go allClasses
  vs <- traverse (reduceInputVariableEquality globalCtx) tensorLevelEqualities
  return $ Map.unions vs
  where
    go :: Set (Either NetworkInputTensorVariable RatTensor) -> [(NetworkInputTensorVariable, NetworkInputTensorVariable)]
    go xs = case sort (lefts $ Set.toList xs) of
      v : vs -> fmap (,v) vs
      [] -> developerError "Disjoint sets should not contain empty equivalence classes"

reduceInputVariableEquality ::
  (MonadCompile m, MonadWriter [CompilationStep] m) =>
  GlobalCtx ->
  (NetworkInputTensorVariable, NetworkInputTensorVariable) ->
  m (LinearSubstitution SliceVariable)
reduceInputVariableEquality ctx (eqInputVar, inputVar) = do
  -- Construct the input variable substitution
  let inputEq = createEq (inputVar, eqInputVar)
  (inputSubst, inputCompilationStep) <- createSubstitutionForVariable ctx eqInputVar inputEq

  -- Construct the output variable substitution
  let outputVar = lookupCorrespondingOutputVar ctx inputVar
  let eqOutputVar = lookupCorrespondingOutputVar ctx eqInputVar
  let outputEq = createEq (outputVar, eqOutputVar)
  (outputSubst, outputCompilationStep) <- createSubstitutionForVariable ctx eqOutputVar outputEq

  -- Note the compilation steps
  tell [outputCompilationStep, inputCompilationStep]

  return (inputSubst <> outputSubst)
  where
    createEq ::
      (TensorVariableLike variable) =>
      (variable, variable) ->
      NormalisedRelation () SliceVariable (Tensor Rational)
    createEq (v1, v2) = do
      let tensorShape = shapeOf $ lookupTensorVariable (globalBoundVarCtx ctx) v1
      let constant = ConstantTensor tensorShape 0
      let coefficients = Map.fromList [(toSliceVar v1, -1), (toSliceVar v2, 1)]
      NormalisedRelation () $ Sparse coefficients constant

--------------------------------------------------------------------------------
-- Calculate the meta-network

calculateMetaNetworkApps ::
  (Traversable f) =>
  GlobalCtx ->
  f (Assertion SliceVariable) ->
  Map Name (NonEmpty NetworkApplicationInfo)
calculateMetaNetworkApps globalCtx assertions = do
  -- First calculate the set of network applications actually used in the query
  let usedSliceVariables = foldMap variablesOf assertions
  let usedTensorVariables = findCorrespondingTensorVariables (globalBoundVarCtx globalCtx) usedSliceVariables
  -- Then filter the network applications
  Map.mapMaybe (filterApplications usedTensorVariables) (networkApplications globalCtx)
  where
    filterApplication :: Set TensorVariable -> NetworkApplicationInfo -> Bool
    filterApplication usedVars NetworkApplicationInfo {..} =
      Set.member (toTensorVar inputVariable) usedVars || Set.member (toTensorVar outputVariable) usedVars

    filterApplications :: Set TensorVariable -> NonEmpty NetworkApplicationInfo -> Maybe (NonEmpty NetworkApplicationInfo)
    filterApplications usedVars apps = NonEmpty.nonEmpty (NonEmpty.filter (filterApplication usedVars) apps)

logEqualitiesFound ::
  (MonadLogger m, Coercible var Lv) =>
  CompleteNamedBoundCtx ->
  [(var, Either var RatTensor)] ->
  m ()
logEqualitiesFound ctx equalities = logDebug MaxDetail $ do
  if null equalities
    then "No suitable equalities found"
    else "Possible equalities:" <> lineIndent (vsep (fmap (prettyEquality ctx) equalities)) <> line

prettyEquality ::
  (Coercible var Lv) =>
  CompleteNamedBoundCtx ->
  (var, Either var RatTensor) ->
  Doc a
prettyEquality ctx (a, b) = do
  let prettyVar v = pretty $ lookupLvInBoundCtx (coerce v) ctx
  prettyVar a <+> "==" <+> either prettyVar pretty b
