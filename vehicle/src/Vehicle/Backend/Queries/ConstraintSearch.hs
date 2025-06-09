module Vehicle.Backend.Queries.ConstraintSearch
  ( findConstraints,
    ConstraintSearchCriteria,
    ConstrainedAssertionTree,
    VariableConstraints (..),
  )
where

import Control.Monad (foldM)
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Vehicle.Backend.Queries.UserVariableElimination.Core
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Data.Assertion
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.QuantifiedVariable
import Vehicle.Data.Tensor (RatTensor)

--------------------------------------------------------------------------------
-- Data

-- | The strongest set of available constraints for a given variable. Equalities
-- are stronger than inequalities.
data VariableConstraints
  = SingleEquality !(Equality TensorVariable RatTensor)
  | Inequalities ![Inequality TensorVariable RatTensor]
  deriving (Eq, Ord)

instance Pretty VariableConstraints where
  pretty = \case
    SingleEquality eq -> "SingleEquality[" <+> prettyVerbose eq <> "]"
    Inequalities [] -> "NoConstraints"
    Inequalities ineqs -> "Inequalities[" <+> prettyVerbose ineqs <> "]"

type ConstrainedAssertionTree = (VariableConstraints, MaybeTrivial (AssertionTree TensorVariable))

-- | A scheme for pulling out constraints from assertions. Used to control
-- which assertions are considered valid constraints.
type ConstraintSearchCriteria =
  Assertion TensorVariable -> ConstrainedAssertionTree

--------------------------------------------------------------------------------
-- Algorithm

-- Takes in a tree of assertions and partitions it only as much as strictly
-- necessary to find the strongest set of constraints over the variable.
findConstraints ::
  forall m.
  (MonadCompile m) =>
  ConstraintSearchCriteria ->
  AssertionTree TensorVariable ->
  m (DisjunctAll ConstrainedAssertionTree)
findConstraints criteria = go
  where
    go :: AssertionTree TensorVariable -> m (DisjunctAll ConstrainedAssertionTree)
    go = \case
      Query assertion -> return $ DisjunctAll [criteria assertion]
      Disjunct xs -> findConstraintsDisjunctAll criteria xs
      Conjunct xs -> findConstraintsConjunctAll criteria xs

findConstraintsDisjunctAll ::
  forall m.
  (MonadCompile m) =>
  ConstraintSearchCriteria ->
  DisjunctAll (AssertionTree TensorVariable) ->
  m (DisjunctAll ConstrainedAssertionTree)
findConstraintsDisjunctAll criteria tree = do
  disjuncts <- disjunctDisjuncts <$> traverse (findConstraints criteria) tree
  -- Collapse disjunctions that have the same constraint, e.g.
  --    (x and a) or (x and b) or (x and c) or (y and d)...
  --      ->
  --    (x and (a or b or c)) or (y and d)
  let treeByConstraints = Map.fromListWith (orTrivial orBoolExpr) $ disjunctsToList disjuncts
  return $ DisjunctAll $ NonEmpty.fromList $ Map.toList treeByConstraints

findConstraintsConjunctAll ::
  forall m.
  (MonadCompile m) =>
  ConstraintSearchCriteria ->
  ConjunctAll (AssertionTree TensorVariable) ->
  m (DisjunctAll ConstrainedAssertionTree)
findConstraintsConjunctAll criteria (ConjunctAll (t :| ts)) = do
  -- Finds the first constraint that has an equality and other combines all inequalities
  r1 <- findConstraints criteria t
  rs' <- foldM andDisjuncts (t, r1) ts
  return $ snd rs'
  where
    andDisjuncts ::
      (AssertionTree TensorVariable, DisjunctAll ConstrainedAssertionTree) ->
      AssertionTree TensorVariable ->
      m (AssertionTree TensorVariable, DisjunctAll ConstrainedAssertionTree)
    andDisjuncts (x, r1) y = do
      let (shortCircuitedLHS, remainingLHS) = partitionEithers $ fmap (shortCircuitConstraints y) (disjunctsToList r1)
      result <-
        if null remainingLHS
          then return shortCircuitedLHS
          else do
            r2 <- findConstraints criteria y
            let (shortCircuitedRHS, remainingRHS) = partitionEithers $ fmap (shortCircuitConstraints x) (disjunctsToList r2)
            if null remainingRHS
              then return shortCircuitedRHS
              else do
                let remainingDisjuncts = cartesianProduct mergeConstraints remainingLHS remainingRHS
                return $ shortCircuitedLHS <> shortCircuitedRHS <> remainingDisjuncts

      case result of
        r : rs -> return (andBoolExpr x y, DisjunctAll (r :| rs))
        [] -> compilerDeveloperError "The conjunctions of non-empty disjunctions should be non-empty."

    shortCircuitConstraints ::
      AssertionTree TensorVariable ->
      ConstrainedAssertionTree ->
      Either ConstrainedAssertionTree ConstrainedAssertionTree
    shortCircuitConstraints disjunctedTree constraint = case constraint of
      (SingleEquality eq, remaining) -> Left (SingleEquality eq, andTrivial andBoolExpr remaining (NonTrivial disjunctedTree))
      _ -> Right constraint

    mergeConstraints ::
      ConstrainedAssertionTree ->
      ConstrainedAssertionTree ->
      ConstrainedAssertionTree
    mergeConstraints c1 c2 = case (c1, c2) of
      ((Inequalities ineqs1, t1), (Inequalities ineqs2, t2)) -> (Inequalities (ineqs1 <> ineqs2), andTrivial andBoolExpr t1 t2)
      _ -> developerError "Impossible - should be no equality constraints after short-circuiting"
