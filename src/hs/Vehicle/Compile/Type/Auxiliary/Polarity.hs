
-- | File for solving the boolean polarity annotations
module Vehicle.Compile.Type.Auxiliary.Polarity
  ( solvePolarityConstraint
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (init)

import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Error
import Vehicle.Language.Print

--------------------------------------------------------------------------------
-- Main method

solvePolarityConstraint :: MonadConstraintSolving m
                        => ConstraintContext
                        -> PolarityConstraint
                        -> m ConstraintProgress
solvePolarityConstraint ctx e@(App _ (Builtin _ (PolarityTypeClass tc)) args) = do
  let c = PC ctx e

  argExprs <- case getExplicitArgs args of
    Just x  -> return x
    Nothing -> compilerDeveloperError $
      "Found non-explicit arguments in polarity constraint:" <+> squotes (prettyVerbose e)

  blockOnMetas argExprs $ case (tc, argExprs) of
    (HasNot,          [Pol pol1, res])           -> solveHasNot        c pol1 res
    (HasAndOr,        [Pol pol1, Pol pol2, res]) -> solveHasAndOr      c pol1 pol2 res
    (HasImpl,         [Pol pol1, Pol pol2, res]) -> solveHasImpl       c pol1 pol2 res
    (HasQuantifier q, [domain, Pol pol1, res])   -> solveHasQuantifier c q domain pol1 res
    _ -> compilerDeveloperError $
      "Malformed polarity constraint: " <+> squotes (prettyVerbose e)

solvePolarityConstraint _ctx e = compilerDeveloperError $
  "Unknown polarity type-class" <+> squotes (prettyVerbose e)

--------------------------------------------------------------------------------
-- Solutions

solveHasNot :: MonadConstraintSolving m
            => Constraint
            -> Polarity
            -> CheckedExpr
            -> m ConstraintProgress
solveHasNot c Unquantified    res = unify c res Unquantified
solveHasNot c (Quantified q)  res = unify c res (Quantified (neg q))
solveHasNot c MixedParallel   res = unify c res MixedParallel
solveHasNot c MixedSequential res = unify c res MixedSequential

solveHasAndOr :: MonadConstraintSolving m
              => Constraint
              -> Polarity
              -> Polarity
              -> CheckedExpr
              -> m ConstraintProgress
solveHasAndOr c Unquantified    pol2            res  = unify c res pol2
solveHasAndOr c pol1            Unquantified    res = unify c res pol1
solveHasAndOr c (Quantified q1) (Quantified q2) res
  | q1 == q2  = unify c res (Quantified q1)
  | otherwise = unify c res MixedParallel
solveHasAndOr c Quantified{}    MixedParallel   res = unify c res MixedParallel
solveHasAndOr c MixedParallel   Quantified{}    res = unify c res MixedParallel
solveHasAndOr c MixedParallel   MixedParallel   res = unify c res MixedParallel
solveHasAndOr c MixedSequential _               res = unify c res MixedSequential
solveHasAndOr c _               MixedSequential res = unify c res MixedSequential

solveHasImpl :: MonadConstraintSolving m
              => Constraint
              -> Polarity
              -> Polarity
              -> CheckedExpr
              -> m ConstraintProgress
solveHasImpl c pol1 = solveHasAndOr c (neg pol1)

solveHasQuantifier :: MonadConstraintSolving m
                   => Constraint
                   -> Quantifier
                   -> CheckedExpr
                   -> Polarity
                   -> CheckedExpr
                   -> m ConstraintProgress
solveHasQuantifier c q domain pol res
  | isFinite domain = unify c res pol
  | otherwise       = case pol of
    Unquantified    -> unify c res (Quantified q)
    MixedParallel   -> unify c res MixedSequential
    MixedSequential -> unify c res MixedSequential
    Quantified q'
      | q == q'   -> unify c res (Quantified q)
      | otherwise -> unify c res MixedSequential

--------------------------------------------------------------------------------
-- Utils

pattern Pol :: Polarity -> Expr binder var ann
pattern Pol p <- Builtin _ (Polarity p)

unify :: MonadConstraintSolving m
      => Constraint
      -> CheckedExpr
      -> Polarity
      -> m ConstraintProgress
unify c e1 pol = return $ Progress
  { newConstraints = [UC ctx $ Unify (e1, Builtin (provenanceOf ctx) (Polarity pol))]
  , solvedMetas    = mempty
  } where ctx = (constraintContext c) { blockedBy = mempty }

blockOnMetas :: MonadConstraintSolving m
             => NonEmpty CheckedExpr
             -> m ConstraintProgress
             -> m ConstraintProgress
blockOnMetas argExprs solve = do
  let metas = filter isMeta (NonEmpty.init argExprs)
  if null metas
    then solve
    else do
      logDebug MaxDetail $ "stuck-on metas" <+> prettyVerbose metas
      return Stuck
