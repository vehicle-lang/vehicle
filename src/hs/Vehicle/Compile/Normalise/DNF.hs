

module Vehicle.Compile.Normalise.DNF
  ( convertToDNF
  , splitConjunctions
  , splitDisjunctions
  ) where

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Language.Print

-- | Converts an expression to disjunctive normal form.
-- Currently assumes all implications and negations have
-- been previously normalised out.
convertToDNF :: MonadCompile m => CheckedExpr -> m CheckedExpr
convertToDNF expr = logCompilerPass "conversion to disjunctive normal form" $ do
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

    LSeq{}      -> normalisationError currentPass "LSeq"
    Ann{}       -> normalisationError currentPass "Ann"
    Let{}       -> normalisationError currentPass "Let"
    Type{}      -> typeError          currentPass "Type"
    Pi{}        -> typeError          currentPass "Pi"
    PrimDict{}  -> visibilityError    currentPass "PrimDict"
    Hole{}      -> visibilityError    currentPass "Hole"
    Meta{}      -> resolutionError    currentPass "Meta"
    Lam{}       -> caseError          currentPass "Lam" ["QuantifierExpr"]

    -- Some sanity checks
    NotExpr{}  -> normalisationError currentPass "Not"
    ImplExpr{} -> normalisationError currentPass "Impl"

    QuantifierExpr ann t binder body -> do
      body' <- dnf body
      return $ liftOr (QuantifierExpr ann t binder) body'

    AndExpr ann t [ExplicitArg ann1 e1, ExplicitArg ann2 e2] -> do
      e1' <- dnf e1
      e2' <- dnf e2
      return $
        liftOr (\e1'' ->
          liftOr (\e2'' ->
            AndExpr ann t [ExplicitArg ann1 e1'', ExplicitArg ann2 e2'']) e2') e1'

    OrExpr ann t [e1, e2] -> do
      e1' <- traverseArgExpr dnf e1
      e2' <- traverseArgExpr dnf e2
      return $ OrExpr ann t [e1', e2']

    App{} -> return expr

  showExit result
  return result

liftOr :: (CheckedExpr -> CheckedExpr) -> CheckedExpr -> CheckedExpr
liftOr f (OrExpr ann t [e1, e2]) = OrExpr ann t (mapArgExpr (liftOr f) <$> [e1, e2])
liftOr f e                       = f e

splitConjunctions :: Expr binder var ann -> [Expr binder var ann]
splitConjunctions (AndExpr _ann _t [e1, e2]) =
  splitConjunctions (argExpr e1) <> splitConjunctions (argExpr e2)
splitConjunctions e = [e]

splitDisjunctions :: Expr binder var ann -> [Expr binder var ann]
splitDisjunctions (OrExpr _ann _t [e1, e2]) =
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
