module Vehicle.Compile.Queries.LNF
  ( convertToLNF,
  )
where

import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Expr.Normalised

-- | Converts an arithmetic expression to linear normal form.
convertToLNF :: MonadCompile m => NormExpr -> m NormExpr
convertToLNF expr =
  logCompilerPass MinDetail "conversion to linear normal form" $ do
    result <- lnf expr
    logCompilerPassOutput (prettyVerbose result)
    return result

--------------------------------------------------------------------------------
-- PNF

lnf :: MonadCompile m => NormExpr -> m NormExpr
lnf expr = case expr of
  VLVec {} -> normalisationError currentPass "LVec"
  VUniverse {} -> unexpectedTypeInExprError currentPass "Universe"
  VPi {} -> unexpectedTypeInExprError currentPass "Pi"
  VMeta {} -> resolutionError currentPass "Meta"
  VLam {} -> caseError currentPass "Lam" ["QuantifierExpr"]
  VFreeVar {} -> normalisationError currentPass "FreeVar"
  -- Elimination cases
  VBuiltin _ (Neg dom) [arg] ->
    lnf $ lowerNeg dom (argExpr arg)
  VBuiltin p (Sub dom) [arg1, arg2] -> do
    let negDom = subToNegDomain dom
    let addDom = subToAddDomain dom
    lnf $ VBuiltin p (Add addDom) [arg1, fmap (lowerNeg negDom) arg2]

  -- Inductive cases
  VBuiltin p (Add dom) args ->
    VBuiltin p (Add dom) <$> traverse (traverse lnf) args
  VBuiltin p (Mul dom) args@[arg1, arg2] -> case (argExpr arg1, argExpr arg2) of
    (_, VBuiltin _ (Add addDom) [v1, v2]) -> do
      let e1 = ExplicitArg p $ VBuiltin p (Mul dom) [arg1, v1]
      let e2 = ExplicitArg p $ VBuiltin p (Mul dom) [arg1, v2]
      lnf $ VBuiltin p (Add addDom) [e1, e2]
    (VBuiltin _ (Add addDom) [v1, v2], _) -> do
      let e1 = ExplicitArg p $ VBuiltin p (Mul dom) [v1, arg1]
      let e2 = ExplicitArg p $ VBuiltin p (Mul dom) [v2, arg2]
      lnf $ VBuiltin p (Add addDom) [e1, e2]
    _ -> do
      VBuiltin p (Mul dom) <$> traverse (traverse lnf) args

  -- Anything else is a base case.
  VLiteral {} -> return expr
  VBuiltin {} -> return expr
  VBoundVar {} -> return expr

lowerNeg :: NegDomain -> NormExpr -> NormExpr
lowerNeg dom = \case
  -- Base cases
  VBuiltin _ (Neg _) [e] -> argExpr e
  VLiteral p (LInt x) -> VLiteral p $ LInt (-x)
  VLiteral p (LRat x) -> VLiteral p $ LRat (-x)
  v@(VBoundVar p _ []) -> do
    let mulDom = negToMulDomain dom
    let minus1 = ExplicitArg p $ case dom of
          NegInt -> VLiteral p $ LInt (-1)
          NegRat -> VLiteral p $ LRat (-1)
    VBuiltin p (Mul mulDom) [minus1, ExplicitArg p v]

  -- Inductive cases
  VBuiltin p (Add addDom) [e1, e2] -> VBuiltin p (Add addDom) [fmap (lowerNeg dom) e1, fmap (lowerNeg dom) e2]
  VBuiltin p (Mul mulDom) [e1, e2] -> VBuiltin p (Mul mulDom) [fmap (lowerNeg dom) e1, e2]
  -- Errors
  e -> developerError ("Unable to lower 'neg' through" <+> pretty (show e))

currentPass :: Doc a
currentPass = "conversion to LNF"
