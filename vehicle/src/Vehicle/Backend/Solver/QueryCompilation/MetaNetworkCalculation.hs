{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Backend.Solver.QueryCompilation.MetaNetworkCalculation
  ( calculateMetaNetworkApplications,
  )
where

import Control.Monad (forM, unless)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.Writer.Strict (MonadWriter (..), WriterT (..))
import Data.Bifunctor (Bifunctor (..))
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
  (MonadQueryCompilation m, MonadMaybeTrivial m) =>
  ConjunctAll LinearAssertion ->
  m (NetworkApplications, ConjunctAll LinearAssertion, [CompilationStep])
calculateMetaNetworkApplications assertions = do
  (eliminationResult, compilationSteps) <- runWriterT $ eliminateRedundantApplications assertions
  case eliminationResult of
    Trivial b -> trivial b
    NonTrivial newAssertions -> do
      networkApps <- calculateMetaNetworkApps newAssertions
      checkIfMetaNetworkSupported networkApps
      nonTrivial (networkApps, newAssertions, compilationSteps)

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
    equivalenceClasses <- forM applicationsByNetwork $ \(networkName, applications) ->
      logCompilerSection2 MaxDetail ("checking applications of network" <+> quotePretty networkName) $ do
        logDebug MaxDetail $ pretty (length applications) <+> "application found" <> line
        if length applications == 1
          then return mempty
          else calculateNetworkTensorInputEquivalenceClasses networkName equalities applications

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
            else eliminateTrivialConjunctions $ fmap (eliminateVarsInComparison subst) assertions

    logDebugM MaxDetail $ do
      assertionsDoc <- prettyFriendlyInCtx resultingAssertions
      return $ "Result:" <> lineIndent assertionsDoc
    return resultingAssertions

-- | Finds equality assertions of the form `a - b == 0` (i.e. `a == b`)
isSimpleVariableEquality :: LinearAssertion -> Maybe SimpleEquality
isSimpleVariableEquality = \case
  NormalisedRelation OEq (Sparse coefficients constant) -> do
    case Map.toList coefficients of
      [(v1, a), (v2, b)] | constant `isTensorOfAll` 0 && a == -b -> Just (v1, Left v2)
      [(v1, a)] -> Just (v1, Right $ mapTensor (/ a) constant)
      _ -> Nothing
  _ -> Nothing

calculateNetworkTensorInputEquivalenceClasses ::
  forall m.
  (MonadQueryCompilation m) =>
  Name ->
  [SimpleEquality] ->
  NonEmpty NetworkApplicationInfo ->
  m EquivalenceClasses
calculateNetworkTensorInputEquivalenceClasses networkName equalities applications = do
  let inputVariables = inputVariable <$> NonEmpty.toList applications
  let initialEquivalenceClasses = DisjointSet.fromLists (fmap (\v -> [Left v]) inputVariables)
  initialTensorVariableMapping <- forM inputVariables $ \inputVar -> do
    parentVar <- lookupNestedTensorVariable inputVar
    return (parentVar, inputVar)
  go mempty initialEquivalenceClasses (Map.fromList initialTensorVariableMapping)
  where
    go :: TensorIndices -> EquivalenceClasses -> TensorVariableMapping -> m EquivalenceClasses
    go tensorIndices equivalenceClasses tensorVariableMapping = logCompilerSection2 MaxDetail ("search for tensor element input equalities for variable" <+> squotes (pretty networkName <> pretty (showTensorIndices (reverse tensorIndices)))) $ do
      -- Calculate the equivalence classes from the equalities you can find at this level
      expandedEquivalenceClasses <- expandEquivalenceClasses equalities tensorVariableMapping equivalenceClasses

      logDebugM MaxDetail $ do
        classesDoc <- prettyEquivalenceClasses expandedEquivalenceClasses
        return $ "equivalenceClasses:" <> lineIndent classesDoc

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
expandEquivalenceClasses equalities variables equivalenceClasses = return $ foldr processEquality equivalenceClasses equalities
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
