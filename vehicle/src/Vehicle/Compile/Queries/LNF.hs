module Vehicle.Compile.Queries.LNF
  ( convertToLNF,
  )
where

import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print

-- | Converts an arithmetic expression to linear normal form.
convertToLNF :: MonadCompile m => CheckedExpr -> m CheckedExpr
convertToLNF expr =
  logCompilerPass MinDetail "conversion to linear normal form" $ do
    result <- lnf expr
    logCompilerPassOutput (prettyVerbose result)
    return result

--------------------------------------------------------------------------------
-- PNF

lnf :: MonadCompile m => CheckedExpr -> m CheckedExpr
lnf expr = case expr of
  LVec {} -> normalisationError currentPass "LVec"
  Ann {} -> normalisationError currentPass "Ann"
  Let {} -> normalisationError currentPass "Let"
  Universe {} -> unexpectedTypeInExprError currentPass "Universe"
  Pi {} -> unexpectedTypeInExprError currentPass "Pi"
  Hole {} -> visibilityError currentPass "Hole"
  Meta {} -> resolutionError currentPass "Meta"
  Lam {} -> caseError currentPass "Lam" ["QuantifierExpr"]
  -- Elimination cases
  NegExpr _ dom [arg] ->
    lnf $ lowerNeg dom (argExpr arg)
  SubExpr p dom [arg1, arg2] -> do
    let negDom = subToNegDomain dom
    let addDom = subToAddDomain dom
    lnf $ AddExpr p addDom [arg1, fmap (lowerNeg negDom) arg2]

  -- Inductive cases
  AddExpr p dom args ->
    AddExpr p dom <$> traverse (traverse lnf) args
  MulExpr p dom args@[arg1, arg2] -> case (argExpr arg1, argExpr arg2) of
    (_, AddExpr _ addDom [v1, v2]) -> do
      let e1 = ExplicitArg p $ MulExpr p dom [arg1, v1]
      let e2 = ExplicitArg p $ MulExpr p dom [arg1, v2]
      lnf $ AddExpr p addDom [e1, e2]
    (AddExpr _ addDom [v1, v2], _) -> do
      let e1 = ExplicitArg p $ MulExpr p dom [v1, arg1]
      let e2 = ExplicitArg p $ MulExpr p dom [v2, arg2]
      lnf $ AddExpr p addDom [e1, e2]
    _ -> do
      MulExpr p dom <$> traverse (traverse lnf) args

  -- Anything else is a base case.
  App {} -> return expr
  Literal {} -> return expr
  Builtin {} -> return expr
  Var {} -> return expr

lowerNeg :: NegDomain -> CheckedExpr -> CheckedExpr
lowerNeg dom = \case
  -- Base cases
  NegExpr _ _ [e] -> argExpr e
  IntLiteral p x -> IntLiteral p (-x)
  RatLiteral p x -> RatLiteral p (-x)
  v@(Var p _) -> do
    let mulDom = negToMulDomain dom
    let minus1 = ExplicitArg p $ case dom of
          NegInt -> IntLiteral p (-1)
          NegRat -> RatLiteral p (-1)
    MulExpr p mulDom [minus1, ExplicitArg p v]

  -- Inductive cases
  AddExpr p addDom [e1, e2] -> AddExpr p addDom [fmap (lowerNeg dom) e1, fmap (lowerNeg dom) e2]
  MulExpr p mulDom [e1, e2] -> MulExpr p mulDom [fmap (lowerNeg dom) e1, e2]
  -- Errors
  e -> developerError ("Unable to lower 'neg' through" <+> pretty (show e))

currentPass :: Doc a
currentPass = "conversion to LNF"
