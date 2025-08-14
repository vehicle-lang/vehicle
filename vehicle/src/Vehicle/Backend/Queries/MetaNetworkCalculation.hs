{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Backend.Queries.MetaNetworkCalculation
  ( calculateMetaNetworkApplications,
  )
where

import Control.Monad (forM)
import Control.Monad.Trans.Writer (WriterT (..))
import Control.Monad.Writer (MonadWriter (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Coerce (Coercible, coerce)
import Data.Either (lefts)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (transpose)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
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
  ConjunctAll (Assertion TensorVariable) ->
  m (MaybeTrivial (NetworkApplicationReplacements, ConjunctAll (Assertion TensorVariable), [CompilationStep]))
calculateMetaNetworkApplications ctx assertions = do
  (eliminationResult, compilationSteps) <- runWriterT $ eliminateRedundantApplications ctx assertions
  case eliminationResult of
    Trivial b -> return $ Trivial b
    NonTrivial newAssertions -> do
      let metaNetworkApps = calculateMetaNetworkApps ctx newAssertions
      return $ NonTrivial (metaNetworkApps, newAssertions, compilationSteps)

--------------------------------------------------------------------------------
-- Redundant network applications

type NetworkInputTensorVariable = NetworkIOVariable

type NetworkInputElementVariable = NetworkIOVariable

type EquivalenceClass = NonEmpty NetworkInputTensorVariable

type Eliminations = [(NetworkInputTensorVariable, NetworkInputTensorVariable)]

type SimpleEquality = (TensorVariable, Either TensorVariable RatTensor)

type EqualityAdjancencyList = [(NetworkIOVariable, Either NetworkIOVariable RatTensor)]

eliminateRedundantApplications ::
  (MonadCompile m, MonadWriter [CompilationStep] m) =>
  GlobalCtx ->
  ConjunctAll (Assertion TensorVariable) ->
  m (MaybeTrivial (ConjunctAll (Assertion TensorVariable)))
eliminateRedundantApplications ctx assertions =
  logCompilerSection2 MaxDetail "checking for redundant network applications" $ do
    let nameCtx = completeNamedCtx ctx
    let applicationsByNetwork = Map.toList $ networkApplications ctx

    let equalities = mapMaybe isSimpleVariableEquality $ conjunctsToList assertions
    logEqualitiesFound nameCtx equalities

    eliminationsByNetwork <- forM applicationsByNetwork $ \(networkName, applications) ->
      logCompilerSection2 MaxDetail ("checking applications of network" <+> quotePretty networkName) $ do
        logDebug MaxDetail $ pretty (length applications) <+> "application found" <> line
        if length applications == 1
          then return mempty
          else do
            let tensorInputVariables = inputVariable <$> NonEmpty.toList applications
            let lookupInputElements var = coerce $ Tensor.toList $ lookupZeroDimVariables ctx var
            let inputElementVariables = transpose $ fmap lookupInputElements tensorInputVariables

            tensorEliminations <- calculateNetworkTensorInputEliminations nameCtx tensorInputVariables equalities
            tensorElementEliminations <- calculateTensorElementEliminations nameCtx tensorInputVariables tensorEliminations inputElementVariables equalities

            return $ tensorEliminations <> tensorElementEliminations

    -- Calculate the substitution to perform
    subst <- logCompilerSection MaxDetail "Calculating substitution:" $ do
      let allEliminations = concat eliminationsByNetwork
      substs <- traverse (reduceInputVariableEquality ctx) allEliminations
      return $ Map.unions substs

    -- Perform the substitution
    let resultingAssertions =
          if Map.null subst
            then NonTrivial assertions
            else eliminateTrivialConjunctions $ fmap (eliminateVarsInComparison subst) assertions

    logDebug MaxDetail $ "Result:" <> lineIndent (prettyFriendly (WithContext resultingAssertions nameCtx))
    return resultingAssertions

-- | Finds equality assertions of the form `a - b == 0` (i.e. `a == b`)
isSimpleVariableEquality ::
  Assertion TensorVariable ->
  Maybe SimpleEquality
isSimpleVariableEquality = \case
  NormalisedRelation OEq (Sparse coefficients constant) -> do
    case Map.toList coefficients of
      [(v1, a), (v2, b)] | constant `isTensorOfAll` 0 && a == -b -> Just (v1, Left v2)
      [(v1, a)] -> Just (v1, Right $ mapTensor (/ a) constant)
      _ -> Nothing
  _ -> Nothing

-- | Eliminate redundancies found via the tensor-level variable equalities
calculateNetworkTensorInputEliminations ::
  (MonadLogger m) =>
  CompleteNamedBoundCtx ->
  [NetworkInputTensorVariable] ->
  [SimpleEquality] ->
  m Eliminations
calculateNetworkTensorInputEliminations nameCtx tensorInputVariables equalities = do
  logCompilerSection2 MaxDetail "search for tensor input equalities" $ do
    let tensorInputVariablesSet = Set.fromList tensorInputVariables
    let lookupTensorInputVariable v = if coerce v `Set.member` tensorInputVariablesSet then Just (coerce v) else Nothing
    let adjacencyList = computeEqualityAdjancencyList lookupTensorInputVariable equalities
    eqClasses <- calculateEquivalenceClasses nameCtx adjacencyList
    return $ equivalenceClassesToEliminations eqClasses

calculateTensorElementEliminations ::
  (MonadLogger m) =>
  CompleteNamedBoundCtx ->
  [NetworkInputTensorVariable] ->
  Eliminations ->
  [[NetworkInputElementVariable]] ->
  [SimpleEquality] ->
  m Eliminations
calculateTensorElementEliminations nameCtx tensorVars eliminations elementVars equalities = do
  logCompilerSection2 MaxDetail "search for tensor element input equalities" $ do
    let remainingInputVariables = listIntersection tensorVars (fmap fst eliminations)
    let allEdges = cartesianProduct (,) remainingInputVariables remainingInputVariables
    sharedAdjacencyList <- calculateSharedEdges (zip elementVars [0 ..]) allEdges
    sharedEquivalenceClasses <- calculateEquivalenceClasses nameCtx (fmap (second Left) sharedAdjacencyList)
    return $ equivalenceClassesToEliminations sharedEquivalenceClasses
  where
    calculateSharedEdges ::
      (MonadLogger m) =>
      [([NetworkIOVariable], Int)] ->
      [(NetworkInputTensorVariable, NetworkInputTensorVariable)] ->
      m [(NetworkInputTensorVariable, NetworkInputTensorVariable)]
    calculateSharedEdges vars sharedEdges
      | null sharedEdges = return []
      | otherwise = case vars of
          [] -> return sharedEdges
          (elementVarsAtIndex, index) : remainingElementVars -> do
            let elementTensorMap = zip elementVarsAtIndex tensorVars
            newSharedEdges <- logCompilerSection MaxDetail ("Calculating equalities at index" <+> pretty index) $ do
              localEqClasses <- calculateTensorElementEquivalenceClasses nameCtx eliminations elementTensorMap equalities
              return $ filter (isEquivalent localEqClasses) sharedEdges
            calculateSharedEdges remainingElementVars newSharedEdges

-- | Eliminate redundancies found via the element-level variable equalities
calculateTensorElementEquivalenceClasses ::
  (MonadLogger m) =>
  CompleteNamedBoundCtx ->
  Eliminations ->
  [(NetworkIOVariable, NetworkIOVariable)] ->
  [SimpleEquality] ->
  m [EquivalenceClass]
calculateTensorElementEquivalenceClasses ctx tensorEliminations childVariables equalities = do
  let childVarMap = Map.fromList childVariables
  let eliminationMap = Map.fromList tensorEliminations
  let lookupInputVariable v = case Map.lookup (coerce v) childVarMap of
        Nothing -> Nothing
        Just tensorVar -> Just $ fromMaybe tensorVar (Map.lookup tensorVar eliminationMap)
  let adjacencyList = computeEqualityAdjancencyList lookupInputVariable equalities
  calculateEquivalenceClasses ctx adjacencyList

computeEqualityAdjancencyList ::
  (TensorVariable -> Maybe NetworkIOVariable) ->
  [SimpleEquality] ->
  EqualityAdjancencyList
computeEqualityAdjancencyList lookupInputVar equalities = do
  let equalityToListEntry (v1, value) = case (lookupInputVar v1, value) of
        (Nothing, _) -> Nothing
        (Just iv1, Right v) -> Just (iv1, Right v)
        (Just iv1, Left v2) -> case lookupInputVar v2 of
          (Just iv2) -> Just (iv1, Left iv2)
          Nothing -> Nothing
  mapMaybe equalityToListEntry equalities

calculateEquivalenceClasses ::
  (MonadLogger m) =>
  CompleteNamedBoundCtx ->
  EqualityAdjancencyList ->
  m [EquivalenceClass]
calculateEquivalenceClasses nameCtx equalities = do
  let arcs = concatMap (\(u, v) -> [(Left u, [v]), (v, [Left u])]) equalities
  let adjacencyList = Map.toList $ Map.fromListWith (<>) arcs
  let inputEqualityAdjacencyList = (\(v, vs) -> (v, v, vs)) <$> adjacencyList
  let equalityPartitions = stronglyConnComp inputEqualityAdjacencyList

  -- For each strong connected component calculate the equivalence classes
  let equivalanceClasses = mapMaybe sccToEquivalenceClass equalityPartitions

  logDebug MaxDetail $ do
    let prettyVar var = pretty $ lookupLvInBoundCtx (toLv var) nameCtx
    let prettyClass c = prettyFlatList (prettyVar <$> NonEmpty.toList c)
    if null equivalanceClasses
      then "No suitable equalities found"
      else "Equal network input variables found:" <> lineIndent (vsep (fmap prettyClass equivalanceClasses))

  return equivalanceClasses

isEquivalent ::
  [EquivalenceClass] ->
  (NetworkInputTensorVariable, NetworkInputTensorVariable) ->
  Bool
isEquivalent classes = do
  let createGroup (groupID, vars) = Map.fromList ((,groupID) <$> NonEmpty.toList vars)
  let varByGroupNumber = Map.unions $ fmap createGroup (zip [0 :: Int ..] classes)
  let lookupGroupNumber v = Map.lookup v varByGroupNumber
  \(v1, v2) -> case (lookupGroupNumber v1, lookupGroupNumber v2) of
    (Just g1, Just g2) -> g1 == g2
    _ -> False

reduceInputVariableEquality ::
  (MonadCompile m, MonadWriter [CompilationStep] m) =>
  GlobalCtx ->
  (NetworkInputTensorVariable, NetworkInputTensorVariable) ->
  m (LinearSubstitution TensorVariable)
reduceInputVariableEquality ctx (eqInputVar, inputVar) = do
  let createEq (v1, v2) = do
        let tensorShape = lookupTensorVariableShape ctx v1
        let constant = ConstantTensor tensorShape 0
        let coefficients = Map.fromList [(coerce v1, -1), (coerce v2, 1)]
        NormalisedRelation () $ Sparse coefficients constant

  -- Construct the input variable substitution
  let inputEq = createEq (inputVar, eqInputVar)
  (inputSubst, inputCompilationStep) <- createSubstitutionForVariable ctx (coerce eqInputVar) inputEq

  -- Construct the output variable substitution
  let outputVar = lookupCorrespondingOutputVar ctx inputVar
  let eqOutputVar = lookupCorrespondingOutputVar ctx eqInputVar
  let outputEq = createEq (outputVar, eqOutputVar)
  (outputSubst, outputCompilationStep) <- createSubstitutionForVariable ctx (coerce eqOutputVar) outputEq

  tell [outputCompilationStep, inputCompilationStep]

  -- Return the result
  return (inputSubst <> outputSubst)

sccToEquivalenceClass :: SCC (Either NetworkIOVariable RatTensor) -> Maybe EquivalenceClass
sccToEquivalenceClass = \case
  AcyclicSCC {} -> Nothing
  CyclicSCC eqClass -> case lefts eqClass of
    v1 : v2 : vs -> Just (v1 :| v2 : vs)
    _ -> Nothing

equivalenceClassesToEliminations :: [EquivalenceClass] -> Eliminations
equivalenceClassesToEliminations eqClasses = do
  let classToElim eqClass = do
        let inputVar = minimum eqClass
        fmap (,inputVar) (NonEmpty.filter (/= inputVar) eqClass)
  concatMap classToElim eqClasses

--------------------------------------------------------------------------------
-- Calculate the meta-network

calculateMetaNetworkApps ::
  (Traversable f) =>
  GlobalCtx ->
  f (Assertion TensorVariable) ->
  Map Name (NonEmpty NetworkApplicationInfo)
calculateMetaNetworkApps globalCtx@GlobalCtx {..} assertions = do
  -- First calculate the set of network applications actually used in the query
  let referencedVars = foldMap variablesOf assertions
  Map.mapMaybe (isMetaNetworkUsed referencedVars) networkApplications
  where
    isMetaNetworkUsed ::
      Set TensorVariable ->
      NonEmpty NetworkApplicationInfo ->
      Maybe (NonEmpty NetworkApplicationInfo)
    isMetaNetworkUsed referencedVars apps =
      case NonEmpty.filter (isApplicationUsed referencedVars) apps of
        [] -> Nothing
        a : as -> Just (a :| as)

    isApplicationUsed ::
      Set TensorVariable ->
      NetworkApplicationInfo ->
      Bool
    isApplicationUsed referencedVars NetworkApplicationInfo {..} = do
      let lookupVar = Tensor.toList . lookupNetworkElementVariables globalCtx
      let appVars = Set.fromList (coerce inputVariable : coerce outputVariable : (lookupVar inputVariable <> lookupVar outputVariable))
      not $ Set.disjoint referencedVars appVars

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
