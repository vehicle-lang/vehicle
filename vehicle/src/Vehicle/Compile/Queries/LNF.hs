module Vehicle.Compile.Queries.LNF
  ( convertToLNF,
  )
where

import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Expr.Normalised

-- | Converts an arithmetic expression to linear normal form.
convertToLNF :: MonadCompile m => StandardNormExpr -> m StandardNormExpr
convertToLNF expr =
  logCompilerPass MinDetail "conversion to linear normal form" $ do
    result <- lnf expr
    logCompilerPassOutput (prettyVerbose result)
    return result

--------------------------------------------------------------------------------
-- PNF

lnf :: MonadCompile m => StandardNormExpr -> m StandardNormExpr
lnf expr = case expr of
  VUniverse {} -> unexpectedTypeInExprError currentPass "Universe"
  VPi {} -> unexpectedTypeInExprError currentPass "Pi"
  VMeta {} -> resolutionError currentPass "Meta"
  VLam {} -> caseError currentPass "Lam" ["QuantifierExpr"]
  VFreeVar {} -> normalisationError currentPass "FreeVar"
  -- Elimination cases
  VBuiltinFunction (Neg dom) [arg] ->
    lnf $ lowerNeg dom arg
  VBuiltinFunction (Sub dom) [arg1, arg2] -> do
    let negDom = subToNegDomain dom
    let addDom = subToAddDomain dom
    lnf $ VBuiltinFunction (Add addDom) [arg1, lowerNeg negDom arg2]

  -- Inductive cases
  VBuiltinFunction (Add dom) args ->
    VBuiltinFunction (Add dom) <$> traverse lnf args
  VBuiltinFunction (Mul dom) args@[arg1, arg2] -> case (arg1, arg2) of
    (_, VBuiltinFunction (Add addDom) [v1, v2]) -> do
      let e1 = VBuiltinFunction (Mul dom) [arg1, v1]
      let e2 = VBuiltinFunction (Mul dom) [arg1, v2]
      lnf $ VBuiltinFunction (Add addDom) [e1, e2]
    (VBuiltinFunction (Add addDom) [v1, v2], _) -> do
      let e1 = VBuiltinFunction (Mul dom) [v1, arg1]
      let e2 = VBuiltinFunction (Mul dom) [v2, arg2]
      lnf $ VBuiltinFunction (Add addDom) [e1, e2]
    _ -> do
      VBuiltinFunction (Mul dom) <$> traverse lnf args

  -- Anything else is a base case.
  VBuiltin {} -> return expr
  VBoundVar {} -> return expr

lowerNeg :: NegDomain -> StandardNormExpr -> StandardNormExpr
lowerNeg dom = \case
  -- Base cases
  VBuiltinFunction (Neg _) [e] -> e
  VIntLiteral x -> VIntLiteral (-x)
  VRatLiteral x -> VRatLiteral (-x)
  v@(VBoundVar _ []) -> do
    let mulDom = negToMulDomain dom
    let minus1 = case dom of
          NegInt -> VIntLiteral (-1)
          NegRat -> VRatLiteral (-1)
    VBuiltinFunction (Mul mulDom) [minus1, v]

  -- Inductive cases
  VBuiltinFunction (Add addDom) [e1, e2] -> VBuiltinFunction (Add addDom) [lowerNeg dom e1, lowerNeg dom e2]
  VBuiltinFunction (Mul mulDom) [e1, e2] -> VBuiltinFunction (Mul mulDom) [lowerNeg dom e1, e2]
  -- Errors
  e -> developerError ("Unable to lower 'neg' through" <+> pretty (show e))

currentPass :: Doc a
currentPass = "conversion to LNF"
