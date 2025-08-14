module Vehicle.Backend.Queries.UserVariableElimination.EliminateExists
  ( eliminateQuantifiedVariable,
  )
where

import Control.Monad (foldM)
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (MonadState (..), gets)
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.These (These (..))
import Vehicle.Backend.Queries.ConstraintSearch
import Vehicle.Backend.Queries.UserVariableElimination.Core
import Vehicle.Compile.Context.Name (getNameContext)
import Vehicle.Compile.FourierMotzkinElimination (fourierMotzkinElimination)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Data.Assertion
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.Code.LinearExpr (HasVariables (containsVariable), VariableLike (..))
import Vehicle.Data.QuantifiedVariable
import Vehicle.Data.Tensor as Tensor (RatTensor, toList)
import Vehicle.Prelude.Warning (CompileWarning (..))
import Vehicle.Verify.Specification (CompilationStep (..))

--------------------------------------------------------------------------------
-- Main function

type MonadSolveExists m = MonadQueryStructure m

-- | Eliminates the provided user variable from the assertion tree. This may
-- require partially converting the expression to disjunctive normal form so it
-- returns a set of disjuncted updated assertion trees and variable solutions.
eliminateQuantifiedVariable ::
  (MonadSolveExists m) =>
  MaybeTrivial (Partitions TensorVariable) ->
  UserVariable ->
  m (MaybeTrivial (Partitions TensorVariable))
eliminateQuantifiedVariable maybePartitions userVar = do
  userVarName <- prettyFriendlyInCtx userVar
  logCompilerSection2 MaxDetail ("elimination of" <+> squotes userVarName <+> "in" <+> pretty (partitionsSize maybePartitions) <+> "partitions") $ do
    case maybePartitions of
      Trivial b -> return $ Trivial b
      NonTrivial partitions -> do
        let var = toTensorVar userVar
        let disjunctedPartitions = partitionsToDisjuncts partitions
        newPartitions <- traverse (eliminateVariableViaEquality var var) disjunctedPartitions
        return $ disjunctMaybeTrivialPartitions newPartitions

--------------------------------------------------------------------------------
-- Attempt 1
--
-- Try to eliminate the tensor variable by finding an equality over it.

eliminateVariableViaEquality ::
  (MonadSolveExists m) =>
  TensorVariable ->
  TensorVariable ->
  Partition TensorVariable ->
  m (MaybeTrivial (Partitions TensorVariable))
eliminateVariableViaEquality topLevelVar var (steps, tree) = do
  userVarName <- prettyFriendlyInCtx var
  logCompilerSection2 MaxDetail ("attempt to eliminate" <+> squotes userVarName <+> "via equalities") $ do
    ctx <- getNameContext
    logDebugM MaxDetail $
      return $
        prettyFriendly (WithContext tree ctx) <> line

    equalitySearchResults <- findEqualityConstraint var tree
    case equalitySearchResults of
      This constrainedTrees -> logSearchResults constrainedTrees False $ do
        solvePartitions (solveVariableViaEquality steps var) constrainedTrees
      That remainingTree -> logSearchResults ([] :: [Int]) True $ do
        eliminateVariableViaChildEqualities topLevelVar var (steps, remainingTree)
      These constrainedTrees remainingTree -> logSearchResults constrainedTrees True $ do
        xs <- solvePartitions (solveVariableViaEquality steps var) constrainedTrees
        ys <- eliminateVariableViaChildEqualities topLevelVar var (steps, remainingTree)
        return $ orTrivial orPartitions xs ys

solveVariableViaEquality ::
  (MonadSolveExists m) =>
  [CompilationStep] ->
  TensorVariable ->
  (Equality TensorVariable RatTensor, Maybe (AssertionTree TensorVariable)) ->
  m (MaybeTrivial (Partition TensorVariable))
solveVariableViaEquality compilationTrace userVar (equality, remainingTree) = do
  globalCtx <- get
  (solutionMap, compilationStep) <- createSubstitutionForVariable globalCtx userVar equality
  let updatedTree = solutionMap `substituteThrough` remainingTree
  let newCompilationTrace = compilationStep : compilationTrace
  -- Update tree
  logEqualitySolved (coerce userVar) compilationStep updatedTree
  return $ fmap (newCompilationTrace,) updatedTree

substituteThrough ::
  LinearSubstitution TensorVariable ->
  Maybe (AssertionTree TensorVariable) ->
  MaybeTrivial (AssertionTree TensorVariable)
substituteThrough f =
  maybe (Trivial True) (eliminateTrivialAtoms . fmap (eliminateVarsInComparison f))

--------------------------------------------------------------------------------
-- Attempt 2
--
-- Otherwise try to eliminate the tensor variable by finding equalities over
-- its element variables.

eliminateVariableViaChildEqualities ::
  forall m.
  (MonadSolveExists m) =>
  TensorVariable ->
  TensorVariable ->
  Partition TensorVariable ->
  m (MaybeTrivial (Partitions TensorVariable))
eliminateVariableViaChildEqualities topLevelVar var partition@(steps, tree) =
  logCompilerSection2 MaxDetail "solving for individual elements" $ do
    -- We need to reduce all unsolved inequalities here to ensure that
    -- we don't have any of the top-level variables left in the tree.
    maybeReducedTree <-
      if var == topLevelVar
        then reduceInequalitiesInvolving topLevelVar tree
        else return (NonTrivial tree)

    case maybeReducedTree of
      Trivial b -> return $ Trivial b
      NonTrivial reducedTree -> do
        maybeChildVariables <- gets (`lookupChildVariables` var)
        case maybeChildVariables of
          Nothing -> eliminateVariableWithInequalities var partition
          Just childVariables -> do
            let step = ReconstructTensorVariable var childVariables
            let newPartition = (step : steps, reducedTree)
            foldM eliminateChildVar (NonTrivial $ singletonPartition newPartition) $ Tensor.toList childVariables
            where
              eliminateChildVar ::
                MaybeTrivial (Partitions TensorVariable) ->
                TensorVariable ->
                m (MaybeTrivial (Partitions TensorVariable))
              eliminateChildVar partitions childVar = do
                let us = fmap partitionsToDisjuncts partitions
                xs <- traverse (traverse (eliminateVariableViaEquality topLevelVar childVar)) us
                let ys = flattenTrivial $ fmap eliminateTrivialDisjunctions xs
                return $ fmap disjunctPartitions ys

reduceInequalitiesInvolving ::
  forall m.
  (MonadSolveExists m) =>
  TensorVariable ->
  AssertionTree TensorVariable ->
  m (MaybeTrivial (AssertionTree TensorVariable))
reduceInequalitiesInvolving variable tree = do
  varName <- prettyFriendlyInCtx variable
  logCompilerSection2 MaxDetail ("Reducing remaining instances of" <+> squotes varName) $ do
    ctx <- get
    us <- traverse (reduceAssertion ctx) tree
    let result = flattenBoolExpr <$> eliminateTrivialAtoms us
    logDebugM MaxDetail $ prettyFriendlyInCtx result
    return result
  where
    reduceAssertion :: GlobalCtx -> Assertion TensorVariable -> m (MaybeTrivial (AssertionTree TensorVariable))
    reduceAssertion ctx ass@(NormalisedRelation rel linearExpr)
      | rel == OEq || not (linearExpr `containsVariable` variable) = return $ NonTrivial $ Query ass
      | otherwise = do
          let conjunctedAssertions = fmap Query <$> reduceComparison (lookupChildVariablesCertain ctx) ass
          return $ maybe (Trivial True) (NonTrivial . Conjunct) conjunctedAssertions

--------------------------------------------------------------------------------
-- Attempt 3
--
-- If there are no equalities over an element variable then we must resort to
-- Fourier-Motzkin elimination which is very expensive.

eliminateVariableWithInequalities ::
  (MonadSolveExists m) =>
  TensorVariable ->
  Partition TensorVariable ->
  m (MaybeTrivial (Partitions TensorVariable))
eliminateVariableWithInequalities var (steps, tree) = do
  userVarName <- prettyFriendlyInCtx var
  logCompilerSection2 MaxDetail ("attempt to eliminate" <+> squotes userVarName <+> "via inequalities") $ do
    searchResults <- findInequalityConstraints var tree
    solvePartitions (solveVariableViaInequalities (coerce var) steps) searchResults

solveVariableViaInequalities ::
  (MonadSolveExists m) =>
  UserVariable ->
  [CompilationStep] ->
  ([Inequality TensorVariable RatTensor], Maybe (AssertionTree TensorVariable)) ->
  m (MaybeTrivial (Partition TensorVariable))
solveVariableViaInequalities var steps (inequalities, remainingTree) = do
  (bounds, newInequalities) <- fourierMotzkinElimination (coerce var) inequalities
  let addIneq ineq = andTrivial andBoolExpr (NonTrivial $ Query $ inequalityToNormRelation ineq)
  let updatedTree = foldr addIneq (maybe (Trivial True) NonTrivial remainingTree) newInequalities
  let step = SolveInequalities var bounds
  let newCompilationTrace = step : steps
  logInequalitiesSolved var step remainingTree
  return $ fmap (newCompilationTrace,) updatedTree

--------------------------------------------------------------------------------
-- Logging

solvePartitions ::
  (MonadSolveExists m) =>
  (a -> m (MaybeTrivial (Partition TensorVariable))) ->
  DisjunctAll a ->
  m (MaybeTrivial (Partitions TensorVariable))
solvePartitions solve constrainedTrees = do
  us <- traverse solve constrainedTrees
  return (Partitions <$> eliminateTrivialDisjunctions us)

logEqualitySolved ::
  (MonadSolveExists m) =>
  UserVariable ->
  CompilationStep ->
  MaybeTrivial (AssertionTree TensorVariable) ->
  m ()
logEqualitySolved _var compilationStep updatedTree =
  logDebugM MaxDetail $ do
    ctx <- getNameContext
    return $
      "Using substitution"
        <> line
        <> indent 2 (prettyFriendly (WithContext compilationStep ctx))
        <> line
        <> "to obtain:"
        <> line
        <> indent 2 (prettyFriendly (WithContext updatedTree ctx))

logInequalitiesSolved ::
  (MonadSolveExists m) =>
  UserVariable ->
  CompilationStep ->
  Maybe (AssertionTree TensorVariable) ->
  m ()
logInequalitiesSolved var step remainingTree = do
  PropertyMetaData {..} <- ask
  ctx <- getNameContext
  let varName = fromMaybe "<unknown-var>" $ lookupLvInBoundCtx (toLv var) ctx

  let treeDoc = maybe "true" (\x -> prettyFriendly (WithContext x ctx)) remainingTree

  logWarning $ UnderSpecifiedProblemSpaceVar propertyAddress varName
  logDebugM MaxDetail $ do
    return $
      "Solving"
        <> line
        <> indent 2 (prettyFriendly (WithContext step ctx))
        <> line
        <> "in context:"
        <> line
        <> indent 2 treeDoc

logSearchResults :: (MonadSolveExists m, Foldable f) => f a -> Bool -> m b -> m b
logSearchResults partitions remainingTree result =
  logCompilerSection2 MaxDetail "processing constraint search results" $ do
    logDebug MaxDetail $
      "Found" <+> pretty (length partitions) <+> "constrained sub-partitions"
        <> (if remainingTree then " and an unconstrained sub-partition" else "")
    result
