module Vehicle.Backend.Queries.UserVariableElimination.EliminateExists
  ( eliminateExists,
  )
where

import Control.Monad.Reader (MonadReader (..))
import Data.Foldable (foldlM)
import Data.Map qualified as Map
import Vehicle.Backend.Queries.ConstraintSearch
import Vehicle.Backend.Queries.UserVariableElimination.Core
import Vehicle.Backend.Queries.UserVariableElimination.FourierMotzkinElimination
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Data.BooleanExpr
import Vehicle.Data.LinearExpr (rearrangeExprToSolveFor, referencesVariable)
import Vehicle.Prelude.Warning (CompileWarning (..))
import Vehicle.Verify.Variable

--------------------------------------------------------------------------------
-- Main function

-- | Eliminates the provided user variable from the assertion tree. This may
-- require partially converting the expression to disjunctive normal form so it
-- returns a set of disjuncted updated assertion trees and variable solutions.
eliminateExists ::
  (MonadQueryStructure m) =>
  MaybeTrivial Partitions ->
  OriginalUserVariable ->
  m (MaybeTrivial Partitions)
eliminateExists partitions userVariable = do
  logDebug MidDetail ""
  logCompilerPass MaxDetail ("solving for user variable" <+> quotePretty userVariable) $
    solveExists fromTensorAssertion solveTensorVariable partitions userVariable

type MonadSolveExists m = MonadQueryStructure m

type ConstraintSolver m variable equality =
  variable -> UserVariableReconstruction -> ConstrainedAssertionTree equality -> m (MaybeTrivial Partitions)

solveExists ::
  (MonadSolveExists m, Pretty variable, Pretty equality) =>
  ConstraintSearchCriteria variable equality ->
  ConstraintSolver m variable equality ->
  MaybeTrivial Partitions ->
  variable ->
  m (MaybeTrivial Partitions)
solveExists searchCriteria solveVarConstraints partitions userVar = do
  let solve (sol, tree) = do
        logDebug MaxDetail ("Solving for" <+> pretty userVar <+> "in" <+> quotePretty tree <> line)
        constraints <- findVariableConstraints searchCriteria userVar tree
        traverse (solveVarConstraints userVar sol) constraints
  results <- traverse (traverse solve . partitionsToDisjuncts) partitions
  let flattenedResults =
        flattenTrivial $ fmap (fmap (foldr1 orPartitions) . eliminateTrivialDisjunctions . disjunctDisjuncts) results
  return flattenedResults

--------------------------------------------------------------------------------
-- Tensor equalities

fromTensorAssertion :: OriginalUserVariable -> Assertion -> ConstrainedAssertionTree TensorEquality
fromTensorAssertion var = \case
  TensorEq eq | tensorEqExpr eq `referencesVariable` UserTensorVar var -> Equality (eq, Trivial True)
  assertion -> NoConstraints (Query assertion)

solveTensorVariable ::
  (MonadSolveExists m) =>
  OriginalUserVariable ->
  UserVariableReconstruction ->
  ConstrainedAssertionTree TensorEquality ->
  m (MaybeTrivial Partitions)
solveTensorVariable userTensorVar solutions = \case
  Equality (TensorEquality tensorEq, remainingTree) -> do
    let (_, rearrangedEq) = rearrangeExprToSolveFor (UserTensorVar userTensorVar) tensorEq
    let solution = SolveTensorEquality userTensorVar rearrangedEq
    logDebug MaxDetail $
      "Solving"
        <> line
        <> indent 2 (pretty solution)
        <> line
        <> "in context:"
        <> line
        <> indent 2 (pretty remainingTree)

    -- Generate accompanying rational solutions
    rationalRearrangedEqs <- reduceTensorExpr rearrangedEq
    userRationalVars <- getReducedUserVariablesFor userTensorVar
    let solutionMap = Map.fromList $ zip (fmap UserRationalVar userRationalVars) rationalRearrangedEqs
    -- Update tree
    let updatedTree = fmap (fmap (substituteTensorEq (userTensorVar, tensorEq) solutionMap)) remainingTree
    return $ mkSinglePartition (solution : solutions, filterTrivialAtoms updatedTree)
  NoConstraints tree -> do
    logDebug MaxDetail "No constraints on original variable found"
    userRationalVars <- getReducedUserVariablesFor userTensorVar
    let updatedSolutions = ReconstructTensor (UserTensorVar userTensorVar) (fmap UserRationalVar userRationalVars) : solutions
    let initial = mkSinglePartition (updatedSolutions, NonTrivial tree)
    foldlM (solveExists fromRationalAssertion solveRationalVariable) initial userRationalVars
  Inequalities {} ->
    compilerDeveloperError $
      "When trying to solve rational variable"
        <+> quotePretty userTensorVar
        <+> "found unexpected tensor inequalities."

--------------------------------------------------------------------------------
-- UserRationalVariables and equalities/constraints

fromRationalAssertion :: UserRationalVariable -> Assertion -> ConstrainedAssertionTree RationalEquality
fromRationalAssertion var = \case
  RationalEq eq | rationalEqExpr eq `referencesVariable` UserRationalVar var -> Equality (eq, Trivial True)
  RationalIneq ineq | rationalIneqExpr ineq `referencesVariable` UserRationalVar var -> Inequalities (ConjunctAll [ineq], Trivial True)
  assertion -> NoConstraints (Query assertion)

solveRationalVariable ::
  (MonadSolveExists m) =>
  UserRationalVariable ->
  UserVariableReconstruction ->
  ConstrainedAssertionTree RationalEquality ->
  m (MaybeTrivial Partitions)
solveRationalVariable var solutions constraint =
  mkSinglePartition <$> case constraint of
    Equality (RationalEquality eq, remainingTree) -> do
      let (_, rearrangedEq) = rearrangeExprToSolveFor (UserRationalVar var) eq
      let solution = SolveRationalEquality var rearrangedEq
      logDebug MaxDetail $
        "Solving"
          <> line
          <> indent 2 (pretty solution)
          <> line
          <> "in context:"
          <> line
          <> indent 2 (pretty remainingTree)
      let updatedTree = fmap (fmap (substituteRationalEq var rearrangedEq)) remainingTree
      return (solution : solutions, filterTrivialAtoms updatedTree)
    Inequalities (ineqs, remainingTree) -> solveRationalInequalities var solutions (conjunctsToList ineqs) remainingTree
    NoConstraints tree -> solveRationalInequalities var solutions [] (NonTrivial tree)

solveRationalInequalities ::
  (MonadSolveExists m) =>
  UserRationalVariable ->
  UserVariableReconstruction ->
  [RationalInequality] ->
  MaybeTrivial AssertionTree ->
  m (UserVariableReconstruction, MaybeTrivial AssertionTree)
solveRationalInequalities var solutions ineqs remainingTree = do
  PropertyMetaData {..} <- ask
  (solution, newInequalities) <- fourierMotzkinElimination var ineqs
  let step = SolveRationalInequalities var solution
  logDebug MaxDetail $
    "Solving"
      <> line
      <> indent 2 (pretty step)
      <> line
      <> "in context:"
      <> line
      <> indent 2 (pretty remainingTree)
  logWarning $ UnderSpecifiedProblemSpaceVar propertyAddress var
  let updatedTree = andTrivial andBoolExpr remainingTree (conjunct $ fmap RationalIneq newInequalities)
  let updatedUserVariableReconstruction = step : solutions
  return (updatedUserVariableReconstruction, updatedTree)
