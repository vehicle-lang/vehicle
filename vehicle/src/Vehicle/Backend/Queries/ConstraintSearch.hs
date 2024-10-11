module Vehicle.Backend.Queries.ConstraintSearch
  ( findVariableConstraints,
    ConstrainedAssertionTree (..),
    ConstraintSearchCriteria,
  )
where

import Control.Monad (foldM)
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Vehicle.Backend.Queries.UserVariableElimination.Core
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Data.Assertion
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.QuantifiedVariable (Variable)
import Vehicle.Data.Tensor (RationalTensor)

--------------------------------------------------------------------------------
-- Data

-- | The strongest set of available constraints for a given variable. Equalities
-- are stronger than inequalities. Is parameterised by the type of equality
-- field so we can either look for rational equalities, tensor equalities or
-- no equalties at all.
data ConstrainedAssertionTree
  = SingleEquality !(Equality RationalTensor) !(MaybeTrivial AssertionTree)
  | Inequalities !(ConjunctAll (Inequality RationalTensor)) !(MaybeTrivial AssertionTree)
  | NoConstraints !AssertionTree

instance Pretty ConstrainedAssertionTree where
  pretty = \case
    SingleEquality eq r -> "SingleEquality[" <+> prettyVerbose eq <> "," <+> prettyVerbose r <+> "]"
    Inequalities ineqs r -> "Inequalities[" <+> prettyVerbose ineqs <> "," <+> prettyVerbose r <+> "]"
    NoConstraints r -> "NoConstraints[" <+> prettyVerbose r <+> "]"

-- | A scheme for pulling out constraints from assertions. Used to control
-- which assertions are considered valid constraints.
type ConstraintSearchCriteria =
  Variable -> Assertion -> ConstrainedAssertionTree

--------------------------------------------------------------------------------
-- Algorithm

-- Takes in a tree of assertions and partitions it only as much as strictly
-- necessary to find the strongest set of constraints over the variable.
findVariableConstraints ::
  forall m.
  (MonadCompile m) =>
  ConstraintSearchCriteria ->
  Variable ->
  AssertionTree ->
  m (DisjunctAll ConstrainedAssertionTree)
findVariableConstraints fromAssertion var = go
  where
    go :: AssertionTree -> m (DisjunctAll ConstrainedAssertionTree)
    go = \case
      Query assertion -> return $ DisjunctAll [fromAssertion var assertion]
      Disjunct xs -> disjunctDisjuncts <$> traverse go xs
      Conjunct (ConjunctAll (NonEmpty.reverse -> x :| xs)) -> do
        r1 <- go x
        rs' <- foldM andDisjuncts (x, r1) xs
        return $ snd rs'

    andDisjuncts ::
      (AssertionTree, DisjunctAll ConstrainedAssertionTree) ->
      AssertionTree ->
      m (AssertionTree, DisjunctAll ConstrainedAssertionTree)
    andDisjuncts (x, r1) y = do
      let (shortCircuitedLHS, remainingLHS) = partitionEithers $ fmap (shortCircuitConstraints y) (disjunctsToList r1)
      result <-
        if null remainingLHS
          then return shortCircuitedLHS
          else do
            r2 <- go y
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
      AssertionTree ->
      ConstrainedAssertionTree ->
      Either ConstrainedAssertionTree ConstrainedAssertionTree
    shortCircuitConstraints disjunctedTree constraint = case constraint of
      SingleEquality eq remaining -> Left $ SingleEquality eq (andTrivial andBoolExpr remaining (NonTrivial disjunctedTree))
      Inequalities ineq remaining -> Right (Inequalities ineq remaining)
      NoConstraints ineq -> Right (NoConstraints ineq)

    mergeConstraints ::
      ConstrainedAssertionTree ->
      ConstrainedAssertionTree ->
      ConstrainedAssertionTree
    mergeConstraints c1 c2 = case (c1, c2) of
      (NoConstraints t1, NoConstraints t2) -> NoConstraints (andBoolExpr t1 t2)
      (NoConstraints t1, Inequalities ineqs2 t2) -> Inequalities ineqs2 (andTrivial andBoolExpr (NonTrivial t1) t2)
      (Inequalities ineqs1 t1, NoConstraints t2) -> Inequalities ineqs1 (andTrivial andBoolExpr t1 (NonTrivial t2))
      (Inequalities ineqs1 t1, Inequalities ineqs2 t2) -> Inequalities (ineqs1 <> ineqs2) (andTrivial andBoolExpr t1 t2)
      _ -> developerError "Impossible - should be no equality constraints after short-circuiting"
