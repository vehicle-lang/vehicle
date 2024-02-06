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
import Vehicle.Data.LinearExpr (referencesVariable)
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
  logCompilerPass MaxDetail ("solving for user variable" <+> quotePretty userVariable) $
    solveExists fromTensorAssertion solveTensorVariable partitions userVariable

type MonadSolveExists m = MonadQueryStructure m

type ConstraintSolver m variable equality =
  variable -> UserVarSolutions -> ConstrainedAssertionTree equality -> m (MaybeTrivial Partitions)

solveExists ::
  (MonadSolveExists m, Pretty variable, Pretty equality) =>
  ConstraintSearchCriteria variable equality ->
  ConstraintSolver m variable equality ->
  MaybeTrivial Partitions ->
  variable ->
  m (MaybeTrivial Partitions)
solveExists searchCriteria solveVarConstraints partitions userVar = do
  let solve (sol, tree) = do
        logDebug MaxDetail ("Solving for" <+> pretty userVar <+> "in" <+> quotePretty tree)
        constraints <- findVariableConstraints searchCriteria userVar tree
        traverse (solveVarConstraints userVar sol) constraints
  results <- traverse (traverse solve . partitionsToDisjuncts) partitions
  let flattenedResults =
        flattenTrivial $ fmap (fmap (foldr1 orPartitions) . eliminateTrivialDisjunctions . disjunctDisjuncts) results
  logDebug MaxDetail ("Final queries" <+> quotePretty flattenedResults)
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
  UserVarSolutions ->
  ConstrainedAssertionTree TensorEquality ->
  m (MaybeTrivial Partitions)
solveTensorVariable userTensorVar solutions = \case
  Equality (tensorEq, remainingTree) -> do
    logDebug MaxDetail $
      "Solving tensor equality:"
        <> line
        <> indent 2 (pretty tensorEq)
        <> line
        <> "in context:"
        <> line
        <> indent 2 (pretty remainingTree)

    -- Generate accompanying rational solutions
    rationalEqualties <- reduceTensorEquality tensorEq
    userRationalVars <- getReducedUserVariablesFor userTensorVar
    let solutionMap = Map.fromList $ zip (fmap UserRationalVar userRationalVars) rationalEqualties
    -- Update tree
    let updatedTree = fmap (fmap (substituteTensorEq (userTensorVar, tensorEq) solutionMap)) remainingTree
    let updatedUserVarSolutions = SolveTensorEquality userTensorVar tensorEq : solutions
    return $ mkSinglePartition (updatedUserVarSolutions, filterTrivialAtoms updatedTree)
  NoConstraints tree -> do
    logDebug MaxDetail "No constraints on original variable found"
    userRationalVars <- getReducedUserVariablesFor userTensorVar
    let initial = mkSinglePartition (solutions, NonTrivial tree)
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
  UserVarSolutions ->
  ConstrainedAssertionTree RationalEquality ->
  m (MaybeTrivial Partitions)
solveRationalVariable var solutions constraint =
  mkSinglePartition <$> case constraint of
    Equality (eq, remainingTree) -> do
      let updatedTree = fmap (fmap (substituteRationalEq var eq)) remainingTree
      let updatedUserVarSolutions = addRationalEqualitySolution var eq solutions
      return (updatedUserVarSolutions, filterTrivialAtoms updatedTree)
    Inequalities (ineqs, remainingTree) -> solveRationalInequalities var solutions (conjunctsToList ineqs) remainingTree
    NoConstraints tree -> solveRationalInequalities var solutions [] (NonTrivial tree)

solveRationalInequalities ::
  (MonadSolveExists m) =>
  UserRationalVariable ->
  UserVarSolutions ->
  [RationalInequality] ->
  MaybeTrivial AssertionTree ->
  m (UserVarSolutions, MaybeTrivial AssertionTree)
solveRationalInequalities var solutions ineqs remainingTree = do
  PropertyMetaData {..} <- ask
  logWarning $ UnderSpecifiedProblemSpaceVar propertyAddress var
  (solution, newInequalities) <- fourierMotzkinElimination var ineqs
  let updatedTree = andTrivial andBoolExpr remainingTree (conjunct $ fmap RationalIneq newInequalities)
  let updatedUserVarSolutions = SolveRationalInequalities var solution : solutions
  return (updatedUserVarSolutions, updatedTree)
