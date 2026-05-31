module Vehicle.Backend.Solver.UserVariableElimination.ConstraintSearch
  ( findEqualityConstraint,
    findInequalityConstraints,
    findAllBounds,
  )
where

import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes)
import Data.These (These (..))
import Data.These.Combinators (catHere, catThere)
import Vehicle.Compile.Constants.Rational
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude (MonadLogger, mergeNonEmptyKeyValues, unionMaybeWith)
import Vehicle.Data.Assertion
import Vehicle.Data.Bound
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.Code.LinearExpr (ConstantLike, HasVariables (..), LinearExpr)
import Vehicle.Data.Variable.Bound.Context.Name (MonadReadableNameContext)
import Vehicle.Data.Variable.Bound.Level

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
  (MonadCompile m, MonadReadableNameContext m) =>
  SliceVariable ->
  LinearAssertionTree ->
  m (DisjunctAll (SliceBounds LinearExpression, Maybe LinearAssertionTree))
findInequalityConstraints var = findAllBounds (tryConvertAssertionToSliceBounds var)

--------------------------------------------------------------------------------
-- Single constraints

-- | Implicitly conjuncted
type ConstrainedTree = (LinearEquality, Maybe LinearAssertionTree)

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
      Atom assertion -> case getEquality assertion of
        Nothing -> return $ That $ Atom assertion
        Just constraint
          | assertion `containsVariable` var -> return $ This (DisjunctAll [(constraint, Nothing)])
          | otherwise -> return $ That $ Atom assertion

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
    ([], _) -> That $ disjunctExprs xs
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
      let collapse u = fmap (disjunctExprs . DisjunctAll) $ NonEmpty.nonEmpty $ catMaybes $ NonEmpty.toList u
      DisjunctAll $ mergeNonEmptyKeyValues collapse $ unDisjunctAll disjuncts

    mergeUnconstrainedTrees :: DisjunctAll LinearAssertionTree -> LinearAssertionTree
    mergeUnconstrainedTrees = disjunctExprs

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
      let eqToAssertion = Atom . equalityToAssertion
      disjunctExprs $ fmap (\(a, b) -> maybe (eqToAssertion a) (andBoolExpr (eqToAssertion a)) b) t2

    andConstraints :: NonEmpty LinearAssertionTree -> DisjunctAll ConstrainedTree -> DisjunctAll ConstrainedTree
    andConstraints xs = do
      let t = conjunctExprs $ ConjunctAll xs
      fmap (second (Just . maybe t (andBoolExpr t)))

    andResults :: NonEmpty LinearAssertionTree -> SingleSearchResults -> SingleSearchResults
    andResults xs = bimap (andConstraints xs) (andBoolExpr (conjunctExprs $ ConjunctAll xs))

--------------------------------------------------------------------------------
-- Multiple constraints

type ConstraintTree constant = BooleanExpr (Assertion (LinearExpr SliceVariable constant))

-- Implicitly conjuncted
type BoundedTree bounds constant expr = (bounds expr, Maybe (ConstraintTree constant))

type BoundedTrees bounds constant expr = DisjunctAll (BoundedTree bounds constant expr)

noResults :: (IsBounds bounds expr) => ConstraintTree constant -> BoundedTrees bounds constant expr
noResults tree = DisjunctAll [(emptyBounds, Just tree)]

oneResult :: bounds expr -> BoundedTrees bounds constant expr
oneResult bounds = DisjunctAll [(bounds, Nothing)]

findAllBounds ::
  forall m bounds constant expr.
  (MonadCompile m, IsBounds bounds expr, Ord (bounds expr)) =>
  (Assertion (LinearExpr SliceVariable constant) -> m (Maybe (bounds expr))) ->
  BooleanExpr (Assertion (LinearExpr SliceVariable constant)) ->
  m (BoundedTrees bounds constant expr)
findAllBounds assertionToConstraint = go
  where
    go :: ConstraintTree constant -> m (BoundedTrees bounds constant expr)
    go = \case
      Disjunct xs -> findAllBoundsDisjunct =<< traverse go xs
      Conjunct xs -> findAllBoundsConjunct =<< traverse go xs
      Atom assertion -> do
        maybeConstraint <- assertionToConstraint assertion
        case maybeConstraint of
          Nothing -> return $ noResults (Atom assertion)
          Just constraint -> return $ oneResult constraint

findAllBoundsDisjunct ::
  forall m bounds constant expr.
  (MonadCompile m, Ord (bounds expr)) =>
  DisjunctAll (BoundedTrees bounds constant expr) ->
  m (BoundedTrees bounds constant expr)
findAllBoundsDisjunct disjuncts = return $ optimiseDisjuncts $ disjunctDisjuncts disjuncts
  where
    optimiseDisjuncts :: BoundedTrees bounds constant expr -> BoundedTrees bounds constant expr
    optimiseDisjuncts allDisjuncts = do
      let mergeDisjuncts = fmap (conjunctExprs . ConjunctAll) . sequence
      DisjunctAll $ mergeNonEmptyKeyValues mergeDisjuncts (unDisjunctAll allDisjuncts)

findAllBoundsConjunct ::
  forall m bounds constant expr.
  (MonadCompile m, IsBounds bounds expr) =>
  ConjunctAll (BoundedTrees bounds constant expr) ->
  m (BoundedTrees bounds constant expr)
findAllBoundsConjunct conjuncts = return $ combineConjuncts conjuncts
  where
    combineConjuncts :: ConjunctAll (BoundedTrees bounds constant expr) -> BoundedTrees bounds constant expr
    combineConjuncts = foldr1 $ conjunctDisjuncts (\(a, b) (c, d) -> (andBounds a c, unionMaybeWith andBoolExpr b d))

tryConvertAssertionToSliceBounds ::
  (MonadLogger m, MonadReadableNameContext m, ConstantLike constant) =>
  SliceVariable ->
  Assertion (LinearExpr SliceVariable constant) ->
  m (Maybe (SliceBounds (LinearExpr SliceVariable constant)))
tryConvertAssertionToSliceBounds var assertion = do
  return (maybeBoundsToSliceBounds <$> tryConvertAssertionToBound var assertion)
