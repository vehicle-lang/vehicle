module Vehicle.Backend.Queries.ConstraintSearch
  ( findEqualityConstraint,
    findInequalityConstraints,
  )
where

import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes)
import Data.These (These (..))
import Data.These.Combinators (catHere, catThere)
import Vehicle.Backend.Queries.UserVariableElimination.Core
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude (mergeNonEmptyKeyValues, unionMaybeWith)
import Vehicle.Data.Assertion
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.Code.LinearExpr (HasVariables (..))
import Vehicle.Data.QuantifiedVariable
import Vehicle.Data.Tensor (RatTensor)

--------------------------------------------------------------------------------
-- Public interface

-- | Tries to find an equality constraint in the tree of assertions for
-- the variable while trying to generate the minimum of disjuncts possible.
findEqualityConstraint ::
  (MonadCompile m) =>
  SliceVariable ->
  LinearAssertionTree ->
  m SingleSearchResults
findEqualityConstraint = findSingleConstraint

findInequalityConstraints ::
  (MonadCompile m) =>
  SliceVariable ->
  LinearAssertionTree ->
  m (DisjunctAll ([Inequality SliceVariable RatTensor], Maybe LinearAssertionTree))
findInequalityConstraints = findAllConstraints getInequality

--------------------------------------------------------------------------------
-- Single constraints

-- | Implicitly conjuncted
type ConstrainedTree = (Equality SliceVariable RatTensor, Maybe LinearAssertionTree)

-- | Implicitly disjuncted
type SingleSearchResults = These (DisjunctAll ConstrainedTree) LinearAssertionTree

findSingleConstraint ::
  forall m.
  (MonadCompile m) =>
  SliceVariable ->
  LinearAssertionTree ->
  m SingleSearchResults
findSingleConstraint var = go
  where
    go :: LinearAssertionTree -> m SingleSearchResults
    go = \case
      Disjunct xs -> disjunctSingleResults xs =<< traverse go xs
      Conjunct xs -> conjunctSingleConstraints go xs
      Query assertion -> case getEquality assertion of
        Nothing -> return $ That $ Query assertion
        Just constraint
          | assertion `containsVariable` var -> return $ This (DisjunctAll [(constraint, Nothing)])
          | otherwise -> return $ That $ Query assertion

disjunctSingleResults ::
  forall m.
  (MonadCompile m) =>
  DisjunctAll LinearAssertionTree ->
  DisjunctAll SingleSearchResults ->
  m SingleSearchResults
disjunctSingleResults xs (DisjunctAll results) = do
  let allConstrainedTrees = catHere $ NonEmpty.toList results
  let allUnconstrainedTrees = catThere $ NonEmpty.toList results
  return $ case (allConstrainedTrees, allUnconstrainedTrees) of
    ([], _) -> That $ Disjunct xs
    (c : cs, []) -> This (mergeConstrainedTrees (DisjunctAll $ c :| cs))
    (c : cs, u : us) -> These (mergeConstrainedTrees $ DisjunctAll $ c :| cs) (mergeUnconstrainedTrees $ DisjunctAll $ u :| us)
  where
    mergeConstrainedTrees ::
      DisjunctAll (DisjunctAll ConstrainedTree) ->
      DisjunctAll ConstrainedTree
    mergeConstrainedTrees nestedDisjuncts = do
      let disjuncts = disjunctDisjuncts nestedDisjuncts
      -- \| Optimisation: Collapse disjunctions that have the same constraint, e.g.
      --    (x and a) ||or|| (x and b) ||or|| (x and c) ||or|| (y and d)...
      --      ->
      --    (x and (a or b or c)) ||or|| (y and d)
      -- let treeByConstraints = Map.fromListWith (orTrivial orBoolExpr) $ disjunctsToList disjuncts
      let collapse u = fmap (Disjunct . DisjunctAll) $ NonEmpty.nonEmpty $ catMaybes $ NonEmpty.toList u
      DisjunctAll $ mergeNonEmptyKeyValues collapse $ unDisjunctAll disjuncts

    mergeUnconstrainedTrees :: DisjunctAll LinearAssertionTree -> LinearAssertionTree
    mergeUnconstrainedTrees = Disjunct

conjunctSingleConstraints ::
  forall m.
  (MonadCompile m) =>
  (LinearAssertionTree -> m SingleSearchResults) ->
  ConjunctAll LinearAssertionTree ->
  m SingleSearchResults
conjunctSingleConstraints search conjuncts = searchConjuncts $ unConjunctAll conjuncts
  where
    searchConjuncts :: NonEmpty LinearAssertionTree -> m SingleSearchResults
    searchConjuncts (x :| xs) = do
      results <- search x
      case xs of
        [] -> return results
        y : ys -> case results of
          -- If there are no constraints in the current conjunct then search the current conjuncts
          -- and conjunct the current conjunct to the result.
          That {} -> andResults [x] <$> searchConjuncts (y :| ys)
          -- If there are some partial constraints in `the current conjunct
          -- then search the remaining conjuncts
          These constrained unconstrained -> do
            recResults <- searchConjuncts (y :| ys)
            case recResults of
              That {} -> return $ andResults (y :| ys) results
              This {} -> return $ andResults [x] recResults
              These recConstrained recUnconstrained -> do
                -- (A v B) and (C v D) = (A and C) or (A and D) or (B and C) or (B and D)
                let newUnconstrained = andBoolExpr unconstrained recUnconstrained
                let newConstrained1 = andConstraints [collapseTrees recConstrained] constrained
                let newConstrained2 = andConstraints [unconstrained] recConstrained
                let newConstrained3 = andConstraints [recUnconstrained] constrained
                let newConstrained = disjunctDisjuncts (DisjunctAll [newConstrained1, newConstrained2, newConstrained3])
                return $ These newConstrained newUnconstrained
          This totalConstraints
            | length totalConstraints == 1 ->
                -- Then we've found a single equality constraint that doesn't require us
                -- to perform any disjunctions and we can't do better than this so halt
                -- the search and return
                return $ andResults (y :| ys) results
            | otherwise -> do
                -- Otherwise there may be still be an equality elsewhere that requires
                -- less disjunctions to extract so recursively search the remainder of
                -- the conjunctions.
                recResults <- searchConjuncts (y :| ys)
                case recResults of
                  This bestTotalConstraints
                    | length totalConstraints >= length bestTotalConstraints -> return $ andResults [x] recResults
                  _ -> return $ andResults (y :| ys) results

    collapseTrees :: DisjunctAll ConstrainedTree -> LinearAssertionTree
    collapseTrees t2 = do
      let eqToAssertion = Query . equalityToAssertion
      Disjunct $ fmap (\(a, b) -> maybe (eqToAssertion a) (andBoolExpr (eqToAssertion a)) b) t2

    andConstraints :: NonEmpty LinearAssertionTree -> DisjunctAll ConstrainedTree -> DisjunctAll ConstrainedTree
    andConstraints xs = do
      let t = Conjunct $ ConjunctAll xs
      fmap (second (Just . maybe t (andBoolExpr t)))

    andResults :: NonEmpty LinearAssertionTree -> SingleSearchResults -> SingleSearchResults
    andResults xs = bimap (andConstraints xs) (andBoolExpr (Conjunct $ ConjunctAll xs))

-- (u == x or y >=1 ) and (u == x + 1 or y <= 1)

--------------------------------------------------------------------------------
-- Core algorithm

-- Implicitly conjuncted
type AllConstrainedTree constraint = ([constraint], Maybe LinearAssertionTree)

type AllSearchResults constraint = DisjunctAll (AllConstrainedTree constraint)

noResults :: LinearAssertionTree -> AllSearchResults constraint
noResults tree = DisjunctAll [(mempty, Just tree)]

oneResult :: constraint -> AllSearchResults constraint
oneResult constraint = DisjunctAll [([constraint], Nothing)]

findAllConstraints ::
  forall m constraint.
  (MonadCompile m, Ord constraint) =>
  (Assertion SliceVariable -> Maybe constraint) ->
  SliceVariable ->
  LinearAssertionTree ->
  m (AllSearchResults constraint)
findAllConstraints assertionToConstraint var = go
  where
    go :: LinearAssertionTree -> m (AllSearchResults constraint)
    go = \case
      Disjunct xs -> findAllConstraintsDisjunct =<< traverse go xs
      Conjunct xs -> findAllConstraintsConjunct =<< traverse go xs
      Query assertion -> case assertionToConstraint assertion of
        Nothing -> return $ noResults (Query assertion)
        Just constraint
          | assertion `containsVariable` var -> return $ oneResult constraint
          | otherwise -> return $ noResults (Query assertion)

findAllConstraintsDisjunct ::
  forall m constraint.
  (MonadCompile m, Ord constraint) =>
  DisjunctAll (AllSearchResults constraint) ->
  m (AllSearchResults constraint)
findAllConstraintsDisjunct disjuncts = return $ optimiseDisjuncts $ disjunctDisjuncts disjuncts
  where
    optimiseDisjuncts :: AllSearchResults constraint -> AllSearchResults constraint
    optimiseDisjuncts allDisjuncts = DisjunctAll $ mergeNonEmptyKeyValues (fmap (Conjunct . ConjunctAll) . sequence) (unDisjunctAll allDisjuncts)

findAllConstraintsConjunct ::
  forall m constraint.
  (MonadCompile m) =>
  ConjunctAll (AllSearchResults constraint) ->
  m (AllSearchResults constraint)
findAllConstraintsConjunct conjuncts = return $ combineConjuncts conjuncts
  where
    combineConjuncts :: ConjunctAll (AllSearchResults constraint) -> AllSearchResults constraint
    combineConjuncts = foldr1 $ conjunctDisjuncts (\(a, b) (c, d) -> (a <> c, unionMaybeWith andBoolExpr b d))
