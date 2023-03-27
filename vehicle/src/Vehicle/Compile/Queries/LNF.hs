module Vehicle.Compile.Queries.LNF
  ( convertToLNF,
  )
where

import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (evalMul)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Expr.Normalised

-- | Converts an arithmetic expression to linear normal form.
convertToLNF :: (MonadCompile m) => StandardNormExpr -> m StandardNormExpr
convertToLNF = lnf

{-
logCompilerPass MinDetail "conversion to linear normal form" $ do
  result <- lnf expr
  logCompilerPassOutput (prettyVerbose result)
  return result
-}
--------------------------------------------------------------------------------
-- PNF

lnf :: (MonadCompile m) => StandardNormExpr -> m StandardNormExpr
lnf expr = case expr of
  VUniverse {} -> unexpectedTypeInExprError currentPass "Universe"
  VPi {} -> unexpectedTypeInExprError currentPass "Pi"
  VMeta {} -> resolutionError currentPass "Meta"
  VLam {} -> caseError currentPass "Lam" ["QuantifierExpr"]
  VFreeVar {} -> normalisationError currentPass "FreeVar"
  VBuiltinFunction fun args -> do
    args' <- traverse lnf args
    case (fun, args') of
      (Neg dom, [e1]) -> return $ lowerNeg dom e1
      (Add dom, [e1, e2]) -> return $ VBuiltinFunction (Add dom) [e1, e2]
      (Sub dom, [e1, e2]) -> return $ normSub dom e1 e2
      (Mul dom, [e1, e2]) -> return $ normMul dom e1 e2
      (Div dom, [e1, e2]) -> return $ normDiv dom e1 e2
      _ -> return $ VBuiltinFunction fun args'
  VBuiltin {} -> return expr
  VBoundVar {} -> return expr

normMul :: MulDomain -> StandardNormExpr -> StandardNormExpr -> StandardNormExpr
normMul dom e1 e2 = case (e1, e2) of
  (_, VBuiltinFunction (Add addDom) [v1, v2]) -> do
    let r1 = normMul dom e1 v1
    let r2 = normMul dom e1 v2
    VBuiltinFunction (Add addDom) [r1, r2]
  (VBuiltinFunction (Add addDom) [v1, v2], _) -> do
    let r1 = normMul dom v1 e2
    let r2 = normMul dom v2 e2
    VBuiltinFunction (Add addDom) [r1, r2]
  _ -> case evalMul dom [e1, e2] of
    Nothing -> VBuiltinFunction (Mul dom) [e1, e2]
    Just r -> r

normSub :: SubDomain -> StandardNormExpr -> StandardNormExpr -> StandardNormExpr
normSub dom e1 e2 = do
  let negDom = subToNegDomain dom
  let addDom = subToAddDomain dom
  VBuiltinFunction (Add addDom) [e1, lowerNeg negDom e2]

normDiv :: DivDomain -> StandardNormExpr -> StandardNormExpr -> StandardNormExpr
normDiv dom e1 e2 = case (e1, e2) of
  (_, VRatLiteral l) -> do
    let mulDom = divToMulDomain dom
    normMul mulDom e1 (VRatLiteral (1 / l))
  _ -> do
    VBuiltinFunction (Div dom) [e1, e2]

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
