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
  VBuiltinFunction (Neg dom) [arg] ->
    lnf $ lowerNeg dom (argExpr arg)
  VBuiltinFunction (Sub dom) [arg1, arg2] -> do
    let negDom = subToNegDomain dom
    let addDom = subToAddDomain dom
    lnf $ VBuiltinFunction (Add addDom) [arg1, fmap (lowerNeg negDom) arg2]

  -- Inductive cases
  VBuiltinFunction (Add dom) args ->
    VBuiltinFunction (Add dom) <$> traverse (traverse lnf) args
  VBuiltinFunction (Mul dom) args@[arg1, arg2] -> case (argExpr arg1, argExpr arg2) of
    (_, VBuiltinFunction (Add addDom) [v1, v2]) -> do
      let e1 = ExplicitArg p $ VBuiltinFunction (Mul dom) [arg1, v1]
      let e2 = ExplicitArg p $ VBuiltinFunction (Mul dom) [arg1, v2]
      lnf $ VBuiltinFunction (Add addDom) [e1, e2]
    (VBuiltinFunction (Add addDom) [v1, v2], _) -> do
      let e1 = ExplicitArg p $ VBuiltinFunction (Mul dom) [v1, arg1]
      let e2 = ExplicitArg p $ VBuiltinFunction (Mul dom) [v2, arg2]
      lnf $ VBuiltinFunction (Add addDom) [e1, e2]
    _ -> do
      VBuiltinFunction (Mul dom) <$> traverse (traverse lnf) args

  -- Anything else is a base case.
  VLiteral {} -> return expr
  VBuiltin {} -> return expr
  VBoundVar {} -> return expr
  where
    p = mempty

lowerNeg :: NegDomain -> NormExpr -> NormExpr
lowerNeg dom = \case
  -- Base cases
  VBuiltinFunction (Neg _) [e] -> argExpr e
  VLiteral (LInt x) -> VLiteral $ LInt (-x)
  VLiteral (LRat x) -> VLiteral $ LRat (-x)
  v@(VBoundVar _ []) -> do
    let mulDom = negToMulDomain dom
    let minus1 = ExplicitArg mempty $ case dom of
          NegInt -> VLiteral $ LInt (-1)
          NegRat -> VLiteral $ LRat (-1)
    VBuiltinFunction (Mul mulDom) [minus1, ExplicitArg mempty v]

  -- Inductive cases
  VBuiltinFunction (Add addDom) [e1, e2] -> VBuiltinFunction (Add addDom) [fmap (lowerNeg dom) e1, fmap (lowerNeg dom) e2]
  VBuiltinFunction (Mul mulDom) [e1, e2] -> VBuiltinFunction (Mul mulDom) [fmap (lowerNeg dom) e1, e2]
  -- Errors
  e -> developerError ("Unable to lower 'neg' through" <+> pretty (show e))

currentPass :: Doc a
currentPass = "conversion to LNF"
