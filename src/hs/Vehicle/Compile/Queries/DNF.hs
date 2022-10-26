

module Vehicle.Compile.Queries.DNF
  ( convertToDNF
  , splitConjunctions
  , splitDisjunctions
  ) where

import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Language.Print

--------------------------------------------------------------------------------
-- Or operations

-- | Converts an expression to disjunctive normal form.
-- Currently assumes all implications and negations have
-- been previously normalised out.
convertToDNF :: MonadCompile m => CheckedExpr -> m CheckedExpr
convertToDNF expr =
  logCompilerPass MinDetail "conversion to disjunctive normal form" $ do
    result <- dnf expr
    logCompilerPassOutput (prettyFriendly result)
    return result

dnf :: MonadCompile m => CheckedExpr -> m CheckedExpr
dnf expr = do
  showEntry expr
  result <- case expr of
    Literal{}   -> return expr
    Builtin{}   -> return expr
    Var{}       -> return expr

    LVec{}      -> normalisationError currentPass "LVec"
    Ann{}       -> normalisationError currentPass "Ann"
    Let{}       -> normalisationError currentPass "Let"
    Universe{}  -> unexpectedTypeInExprError          currentPass "Universe"
    Pi{}        -> unexpectedTypeInExprError          currentPass "Pi"
    Hole{}      -> visibilityError    currentPass "Hole"
    Meta{}      -> resolutionError    currentPass "Meta"
    Lam{}       -> caseError          currentPass "Lam" ["QuantifierExpr"]

    -- Some sanity checks
    NotExpr{}     -> normalisationError currentPass "Not"
    ImpliesExpr{} -> normalisationError currentPass "Implies"

    ExistsRatExpr p binder body -> do
      body' <- dnf body
      return $ liftOr (ExistsRatExpr p binder) body'

    AndExpr p [ExplicitArg p1 e1, ExplicitArg p2 e2] -> do
      e1' <- dnf e1
      e2' <- dnf e2
      return $
        liftOr (\e1'' ->
          liftOr (\e2'' ->
            AndExpr p [ExplicitArg p1 e1'', ExplicitArg p2 e2'']) e2') e1'

    OrExpr p [e1, e2] -> do
      e1' <- traverse dnf e1
      e2' <- traverse dnf e2
      return $ OrExpr p [e1', e2']

    App{} -> return expr

  showExit result
  return result

liftOr :: (CheckedExpr -> CheckedExpr) -> CheckedExpr -> CheckedExpr
liftOr f (OrExpr ann [e1, e2]) = OrExpr ann (fmap (liftOr f) <$> [e1, e2])
liftOr f e                     = f e

splitConjunctions :: Expr binder var -> [Expr binder var]
splitConjunctions (AndExpr _ann [e1, e2]) =
  splitConjunctions (argExpr e1) <> splitConjunctions (argExpr e2)
splitConjunctions e = [e]

splitDisjunctions :: Expr binder var -> [Expr binder var]
splitDisjunctions (OrExpr _ann [e1, e2]) =
  splitDisjunctions (argExpr e1) <> splitDisjunctions (argExpr e2)
splitDisjunctions e = [e]

currentPass :: Doc a
currentPass = "conversion to DNF"

showEntry :: MonadLogger m => CheckedExpr -> m ()
showEntry e = do
  logDebug MaxDetail $ "dnf-entry" <+> prettySimple e
  incrCallDepth

showExit :: MonadLogger m => CheckedExpr -> m ()
showExit e = do
  decrCallDepth
  logDebug MaxDetail $ "dnf-exit " <+> prettySimple e
