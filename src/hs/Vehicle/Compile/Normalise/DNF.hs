

module Vehicle.Compile.Normalise.DNF where

import Vehicle.Prelude
import Vehicle.Language.AST
import Vehicle.Compile.Error

-- | Converts an expression to disjunctive normal form.
-- Currently assumes all implications and negations have
-- been previously normalised out.
dnf :: MonadLogger m => CheckedExpr -> m CheckedExpr
dnf expr = case expr of
  Literal{}   -> return expr
  Builtin{}   -> return expr
  PrimDict{}  -> return expr
  Lam{}       -> return expr
  Var{}       -> return expr
  Hole{}      -> return expr

  LSeq{}      -> normalisationError "LSeq"
  Ann{}       -> normalisationError "Ann"
  Let{}       -> normalisationError "Let"
  Meta{}      -> resolutionError "Meta"
  Type{}      -> typeError "Type"
  Pi{}        -> typeError "Pi"

  -- Some sanity checks
  NotExpr{}  -> normalisationError "Not"
  ImplExpr{} -> normalisationError "Impl"

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

liftOr :: (CheckedExpr -> CheckedExpr) -> CheckedExpr -> CheckedExpr
liftOr f (OrExpr ann t [e1, e2]) = OrExpr ann t (mapArgExpr (liftOr f) <$> [e1, e2])
liftOr f e                       = f e