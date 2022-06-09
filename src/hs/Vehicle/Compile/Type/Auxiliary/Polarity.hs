{-# LANGUAGE ViewPatterns #-}

-- | File for solving the boolean polarity annotations
module Vehicle.Compile.Type.Auxiliary.Polarity
  ( solvePolarityConstraint
  ) where

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

  case (tc, argExprs) of
    (HasNot,          [arg, res])         -> solveHasNot   c arg res
    (HasAndOr,        [arg1, arg2, res])  -> solveHasAndOr c arg1 arg2 res
    (HasImpl,         [arg1, arg2, res])  -> solveHasImpl  c arg1 arg2 res
    (HasQuantifier q, [domain, arg, res]) -> solveHasQuantifier c q domain arg res
    _                                     -> malformedConstraint c

solvePolarityConstraint _ctx e = compilerDeveloperError $
  "Unknown polarity type-class" <+> squotes (prettyVerbose e)

--------------------------------------------------------------------------------
-- Solutions

negatePolarity :: (PolarityProvenance -> PolarityProvenance)
               -> Polarity
               -> Polarity
negatePolarity negProv = \case
  Unquantified              -> Unquantified
  Quantified      q pp      -> Quantified (neg q) (negProv pp)
  MixedParallel     pp1 pp2 -> MixedParallel   (negProv pp2) (negProv pp1)
  -- We don't negate a mixed sequential polarity as its the top of the polarity
  -- lattice and we want to give as meaningful and localised error messages
  -- as possible.
  pol@MixedSequential{}     -> pol

solveHasNot :: MonadConstraintSolving m
            => Constraint
            -> CheckedExpr
            -> CheckedExpr
            -> m ConstraintProgress
solveHasNot _ (exprHead -> Meta _ v) _   = blockOnMetas [v]
solveHasNot c (Pol _ pol)            res = do
  let negPol = negatePolarity (NegationProvenance (provenanceOf c)) pol
  return $ unifyPolarity c res negPol
solveHasNot c _ _ = malformedConstraint c

solveHasImpl :: MonadConstraintSolving m
             => Constraint
             -> CheckedExpr
             -> CheckedExpr
             -> CheckedExpr
             -> m ConstraintProgress
solveHasImpl _ (exprHead -> Meta _ v)   _    _   = blockOnMetas [v]
solveHasImpl c (Pol p pol1)             arg2 res = do
  let arg1 = Pol p $ negatePolarity (LHSImpliesProvenance (provenanceOf c)) pol1
  solveHasAndOr c arg1 arg2 res
solveHasImpl c _ _ _ = malformedConstraint c

solveHasAndOr :: MonadConstraintSolving m
              => Constraint
              -> CheckedExpr
              -> CheckedExpr
              -> CheckedExpr
              -> m ConstraintProgress
solveHasAndOr _ (exprHead -> Meta _ m1) _ _   = blockOnMetas [m1]
solveHasAndOr _ _ (exprHead -> Meta _ m2) _   = blockOnMetas [m2]
solveHasAndOr c (Pol _ pol1) (Pol _ pol2) res =
  return $ unifyPolarity c res $ case (pol1, pol2) of
    (Unquantified,      _)            -> pol2
    (_,                 Unquantified) -> pol1
    (Quantified q1 pp1, Quantified q2 pp2)
      | q1 == q2     -> pol1
      | q1 == Forall -> MixedParallel pp1 pp2
      | otherwise    -> MixedParallel pp2 pp1
    (Quantified{},      MixedParallel{})   -> pol2
    (MixedParallel{},   Quantified{})      -> pol1
    (MixedParallel{},   MixedParallel{})   -> pol1
    (MixedSequential{}, _)                 -> pol1
    (_,                 MixedSequential{}) -> pol2
solveHasAndOr c _ _ _ = malformedConstraint c

solveHasQuantifier :: MonadConstraintSolving m
                   => Constraint
                   -> Quantifier
                   -> CheckedExpr
                   -> CheckedExpr
                   -> CheckedExpr
                   -> m ConstraintProgress
solveHasQuantifier _ _ (exprHead -> Meta _ m) _ _ = blockOnMetas [m]
solveHasQuantifier c q domain     arg res =
  case checkIfFinite domain of
    Finite Nothing               -> return $ unify c arg res
    Finite (Just domainPolarity) -> return $ unify c arg res <> unifyPolarity c domainPolarity Unquantified
    Infinite -> do
      let p = provenanceOf c
      case exprHead arg of
        Meta _ m    -> blockOnMetas [m]
        (Pol _ pol) -> return $ unifyPolarity c res $ case pol of
          Unquantified          -> Quantified q (QuantifierProvenance p)
          Quantified q' pp      -> if q == q' then pol else MixedSequential q p pp
          MixedParallel pp1 pp2 -> MixedSequential q p (if q == Forall then pp2 else pp1)
          MixedSequential{}     -> pol
        _ -> malformedConstraint c

--------------------------------------------------------------------------------
-- Utils

pattern Pol :: Provenance -> Polarity -> Expr binder var
pattern Pol p pol = Builtin p (Polarity pol)

data Finiteness
  = Infinite
  | Finite (Maybe CheckedExpr)

checkIfFinite :: CheckedExpr -> Finiteness
checkIfFinite = \case
  AnnotatedBoolType _ t  -> Finite (Just t)
  IndexType{}            -> Finite Nothing
  (TensorType _ tElem _) -> checkIfFinite tElem
  _                      -> Infinite

unify :: Constraint
      -> CheckedExpr
      -> CheckedExpr
      -> ConstraintProgress
unify c e1 e2 = Progress
  { newConstraints = [UC ctx $ Unify (e1, e2)]
  , solvedMetas    = mempty
  } where ctx = (constraintContext c) { blockedBy = mempty }

unifyPolarity :: Constraint
      -> CheckedExpr
      -> Polarity
      -> ConstraintProgress
unifyPolarity c e1 pol = unify c e1 (Pol (provenanceOf c) pol)

blockOnMetas :: MonadConstraintSolving m
             => [Meta]
             -> m ConstraintProgress
blockOnMetas metas = do
  logDebug MaxDetail $ "stuck-on metas" <+> pretty metas
  return Stuck

malformedConstraint :: MonadConstraintSolving m => Constraint -> m a
malformedConstraint c = compilerDeveloperError $
  "Malformed polarity constraint:" <+> prettyVerbose c