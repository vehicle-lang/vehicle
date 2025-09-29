module Vehicle.Backend.Queries.UserVariableElimination.EliminateExists
  ( eliminateQuantifiedVariable,
  )
where

import Control.Monad (foldM)
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (MonadState (..))
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.These (These (..))
import Vehicle.Backend.Queries.ConstraintSearch
import Vehicle.Backend.Queries.UserVariableElimination.Core
import Vehicle.Compile.Context.Name (getNameContext, prettyFriendlyInCtx)
import Vehicle.Compile.FourierMotzkinElimination (fourierMotzkinElimination)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Data.Assertion
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.Code.LinearExpr (HasVariables (containsVariable))
import Vehicle.Data.QuantifiedVariable
import Vehicle.Prelude.Warning (CompileWarning (..))
import Vehicle.Verify.Specification (CompilationStep (..), ReconstructionDepth (OneDimension))

--------------------------------------------------------------------------------
-- Main function

type MonadSolveExists m = MonadQueryStructure m

-- | Eliminates the provided user variable from the assertion tree. This may
-- require partially converting the expression to disjunctive normal form so it
-- returns a set of disjuncted updated assertion trees and variable solutions.
eliminateQuantifiedVariable ::
  (MonadSolveExists m) =>
  MaybeTrivial Partitions ->
  UserTensorVariable ->
  m (MaybeTrivial Partitions)
eliminateQuantifiedVariable maybePartitions userVar = do
  userVarName <- prettyFriendlyInCtx userVar
  logCompilerSection2 MaxDetail ("elimination of" <+> squotes userVarName <+> "in" <+> pretty (partitionsSize maybePartitions) <+> "partitions") $ case maybePartitions of
    Trivial b -> return $ Trivial b
    NonTrivial partitions -> do
      ctx <- get
      let nestedVar = lookupTensorVariable (globalBoundVarCtx ctx) userVar
      let disjunctedPartitions = partitionsToDisjuncts partitions
      newPartitions <- traverse (eliminateVariableViaEquality nestedVar) disjunctedPartitions
      return $ disjunctMaybeTrivialPartitions newPartitions

--------------------------------------------------------------------------------
-- Attempt 1
--
-- Try to eliminate the tensor variable by finding an equality over it.

eliminateVariableViaEquality ::
  (MonadSolveExists m) =>
  NestedSliceVariable ->
  Partition ->
  m (MaybeTrivial Partitions)
eliminateVariableViaEquality var (steps, tree) = do
  let tensorVar = toSliceVar var
  userVarName <- prettyFriendlyInCtx tensorVar
  logCompilerSection2 MaxDetail ("attempt to eliminate" <+> squotes userVarName <+> "via equalities") $ do
    ctx <- getNameContext
    logDebugM MaxDetail $
      return $
        prettyFriendly (WithContext tree ctx) <> line

    equalitySearchResults <- findEqualityConstraint tensorVar tree
    case equalitySearchResults of
      This constrainedTrees -> logSearchResults constrainedTrees False $ solvePartitions (solveVariableViaEquality steps tensorVar) constrainedTrees
      That remainingTree -> logSearchResults ([] :: [Int]) True $ eliminateVariableViaChildEqualities var (steps, remainingTree)
      These constrainedTrees remainingTree -> logSearchResults constrainedTrees True $ do
        xs <- solvePartitions (solveVariableViaEquality steps tensorVar) constrainedTrees
        ys <- eliminateVariableViaChildEqualities var (steps, remainingTree)
        return $ orTrivial orPartitions xs ys

solveVariableViaEquality ::
  (MonadSolveExists m) =>
  [CompilationStep] ->
  SliceVariable ->
  (LinearEquality, Maybe LinearAssertionTree) ->
  m (MaybeTrivial Partition)
solveVariableViaEquality compilationTrace userVar (equality, remainingTree) = do
  globalCtx <- get
  (solutionMap, compilationStep) <- createSubstitutionForVariable globalCtx userVar equality
  let updatedTree = solutionMap `substituteThrough` remainingTree
  let newCompilationTrace = compilationStep : compilationTrace
  -- Update tree
  logEqualitySolved (coerce userVar) compilationStep updatedTree
  return $ fmap (newCompilationTrace,) updatedTree

substituteThrough ::
  LinearSubstitution SliceVariable ->
  Maybe LinearAssertionTree ->
  MaybeTrivial LinearAssertionTree
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
  NestedSliceVariable ->
  Partition ->
  m (MaybeTrivial Partitions)
eliminateVariableViaChildEqualities var partition@(steps, tree) =
  logCompilerSection2 MaxDetail "solving for tensor rows" $ case childVariablesOf var of
    Nothing -> eliminateVariableWithInequalities (toSliceVar var) partition
    Just childVariables -> do
      -- We need to reduce all unsolved inequalities here to ensure that
      -- we don't have any of the top-level variables left in the tree.
      maybeReducedTree <- reduceInequalitiesInvolving (length childVariables) (toSliceVar var) tree
      case maybeReducedTree of
        Trivial b -> return $ Trivial b
        NonTrivial reducedTree -> do
          let step = ReconstructTensorVariable var OneDimension
          let newPartition = (step : steps, reducedTree)
          foldM eliminateChildVar (NonTrivial $ singletonPartition newPartition) childVariables
          where
            eliminateChildVar ::
              MaybeTrivial Partitions ->
              NestedSliceVariable ->
              m (MaybeTrivial Partitions)
            eliminateChildVar partitions childVar = do
              let us = fmap partitionsToDisjuncts partitions
              xs <- traverse (traverse (eliminateVariableViaEquality childVar)) us
              let ys = flattenTrivial $ fmap eliminateTrivialDisjunctions xs
              return $ fmap disjunctPartitions ys

reduceInequalitiesInvolving ::
  forall m.
  (MonadSolveExists m) =>
  Int ->
  SliceVariable ->
  LinearAssertionTree ->
  m (MaybeTrivial LinearAssertionTree)
reduceInequalitiesInvolving dim variable tree = do
  varName <- prettyFriendlyInCtx variable
  logCompilerSection2 MaxDetail ("reducing remaining instances of" <+> squotes varName) $ do
    ctx <- get
    us <- traverse (reduceAssertion ctx) tree
    let result = flattenBoolExpr <$> eliminateTrivialAtoms us
    return result
  where
    reduceAssertion :: GlobalCtx -> LinearAssertion -> m (MaybeTrivial LinearAssertionTree)
    reduceAssertion ctx ass@(NormalisedRelation rel linearExpr)
      | rel == OEq || not (linearExpr `containsVariable` variable) = return $ NonTrivial $ Query ass
      | otherwise = do
          let conjunctedAssertions = fmap Query <$> reduceComparison dim (lookupChildVariablesCertain ctx) ass
          return $ maybe (Trivial True) (NonTrivial . Conjunct) conjunctedAssertions

--------------------------------------------------------------------------------
-- Attempt 3
--
-- If there are no equalities over an element variable then we must resort to
-- Fourier-Motzkin elimination which is very expensive.

eliminateVariableWithInequalities ::
  (MonadSolveExists m) =>
  SliceVariable ->
  Partition ->
  m (MaybeTrivial Partitions)
eliminateVariableWithInequalities var (steps, tree) = do
  userVarName <- prettyFriendlyInCtx var
  logCompilerSection2 MaxDetail ("attempt to eliminate" <+> squotes userVarName <+> "via inequalities") $ do
    searchResults <- findInequalityConstraints var tree
    solvePartitions (solveVariableViaInequalities (coerce var) steps) searchResults

solveVariableViaInequalities ::
  (MonadSolveExists m) =>
  UserVariable ->
  [CompilationStep] ->
  ([LinearInequality], Maybe LinearAssertionTree) ->
  m (MaybeTrivial Partition)
solveVariableViaInequalities var steps (inequalities, remainingTree) = do
  (bounds, newInequalities) <- fourierMotzkinElimination (coerce var) inequalities
  let addIneq ineq = andTrivial andBoolExpr (NonTrivial $ Query $ inequalityToNormRelation ineq)
  let updatedTree = foldr addIneq (maybe (Trivial True) NonTrivial remainingTree) newInequalities
  let step = SolveInequalities (toSliceVar var) bounds
  let newCompilationTrace = step : steps
  logInequalitiesSolved var step remainingTree
  return $ fmap (newCompilationTrace,) updatedTree

--------------------------------------------------------------------------------
-- Logging

solvePartitions ::
  (MonadSolveExists m) =>
  (a -> m (MaybeTrivial Partition)) ->
  DisjunctAll a ->
  m (MaybeTrivial Partitions)
solvePartitions solve constrainedTrees = do
  us <- traverse solve constrainedTrees
  return $ disjunctMaybeTrivialPartitions (fmap (fmap singletonPartition) us)

logEqualitySolved ::
  (MonadSolveExists m) =>
  UserVariable ->
  CompilationStep ->
  MaybeTrivial LinearAssertionTree ->
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
  Maybe LinearAssertionTree ->
  m ()
logInequalitiesSolved var step remainingTree = do
  PropertyMetaData {..} <- ask
  ctx <- getNameContext
  let varName = fromMaybe "<unknown-var>" $ lookupLvInBoundCtx (toLv var) ctx

  let treeDoc = maybe "true" (\x -> prettyFriendly (WithContext x ctx)) remainingTree

  logWarning $ UnderSpecifiedProblemSpaceVar propertyAddress varName
  logDebugM MaxDetail $
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
