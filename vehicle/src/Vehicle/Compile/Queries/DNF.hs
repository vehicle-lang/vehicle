

module Vehicle.Compile.Queries.DNF
  ( convertToDNF
  , lowerNot
  ) where

import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print

-- | Converts an expression to disjunctive normal form.
-- Currently assumes all implications and negations have
-- been previously normalised out.
convertToDNF :: MonadCompile m => CheckedExpr -> m CheckedExpr
convertToDNF expr =
  logCompilerPass MinDetail "conversion to disjunctive normal form" $ do
    result <- dnf expr
    logCompilerPassOutput (prettyFriendly result)
    return result

--------------------------------------------------------------------------------
-- DNF

dnf :: MonadCompile m => CheckedExpr -> m CheckedExpr
dnf expr = do
  showEntry expr
  result <- case expr of
    LVec{}      -> normalisationError currentPass "LVec"
    Ann{}       -> normalisationError currentPass "Ann"
    Let{}       -> normalisationError currentPass "Let"
    Universe{}  -> unexpectedTypeInExprError          currentPass "Universe"
    Pi{}        -> unexpectedTypeInExprError          currentPass "Pi"
    Hole{}      -> visibilityError    currentPass "Hole"
    Meta{}      -> resolutionError    currentPass "Meta"
    Lam{}       -> caseError          currentPass "Lam" ["QuantifierExpr"]

    -- Inductive cases
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

    -- Elimination cases
    NotExpr _ [e] ->
      dnf $ lowerNot (argExpr e)

    ImpliesExpr p [arg1, arg2] ->
      dnf $ OrExpr p [ExplicitArg p (NotExpr p [arg1]), arg2]

    -- Anything else is a base case.
    App{}     -> return expr
    Literal{} -> return expr
    Builtin{} -> return expr
    Var{}     -> return expr


  showExit result
  return result

liftOr :: (CheckedExpr -> CheckedExpr) -> CheckedExpr -> CheckedExpr
liftOr f (OrExpr ann [e1, e2]) = OrExpr ann (fmap (liftOr f) <$> [e1, e2])
liftOr f e                     = f e

lowerNot :: CheckedExpr -> CheckedExpr
lowerNot arg = case arg of
  -- Base cases
  BoolLiteral    p b               -> BoolLiteral p (not b)
  OrderExpr      p dom ord args    -> OrderExpr p dom (neg ord) args
  Builtin        p (Equals dom eq) -> Builtin p (Equals dom (neg eq))
  EqualityExpr   p dom eq args     -> EqualityExpr p dom (neg eq) args
  NotExpr       _ [e]              -> argExpr e

  -- Inductive cases
  ForallRatExpr p binder body  -> ExistsRatExpr p binder $ lowerNot body
  ExistsRatExpr p binder body  -> ForallRatExpr p binder $ lowerNot body
  ImpliesExpr   p [e1, e2]     -> AndExpr p [e1, lowerNotArg e2]
  OrExpr        p args         -> AndExpr p (lowerNotArg <$> args)
  AndExpr       p args         -> OrExpr p (lowerNotArg <$> args)
  IfExpr p tRes [c, e1, e2]    -> IfExpr p tRes [c, lowerNotArg e1, lowerNotArg e2]

  -- Errors
  e -> developerError ("Unable to lower 'not' through" <+> prettyVerbose e)

lowerNotArg :: CheckedArg -> CheckedArg
lowerNotArg = fmap lowerNot

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
