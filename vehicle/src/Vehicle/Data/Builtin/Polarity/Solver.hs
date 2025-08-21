module Vehicle.Data.Builtin.Polarity.Solver
  ( solvePolarityConstraint,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Maybe (mapMaybe)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (normaliseClosure)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.System
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Builtin.Polarity
import Vehicle.Data.Code.Value

solvePolarityConstraint ::
  (MonadPolaritySolver m) =>
  WithContext (InstanceConstraint PolarityBuiltin) ->
  m ()
solvePolarityConstraint constraintWithCtx = do
  normConstraintWithCtx@(WithContext normConstraint@(Resolve origin _ _ goal) ctx) <- substMetaVariables constraintWithCtx
  logDebugM MaxDetail $ do
    let forcedExpr = goalExpr $ instanceGoal $ objectIn normConstraintWithCtx
    let boundCtx = namedBoundCtxOf $ contextOf normConstraintWithCtx
    return $ "forced goal:" <+> prettyFriendly (WithContext forcedExpr boundCtx)

  (tc, spine) <- getTypeClass goal
  let maybeProgress = solve tc (ctx, origin) (mapMaybe getExplicitArg spine)
  let nConstraint = WithContext normConstraint ctx
  case maybeProgress of
    Nothing -> malformedConstraintError nConstraint
    Just progress -> do
      let solution = VBuiltin (PolarityConstructor UnitLiteral) []
      handleAuxiliaryConstraintProgress solution nConstraint =<< progress

--------------------------------------------------------------------------------
-- Constraint solving

pattern VPolarityExpr :: Polarity -> Value PolarityBuiltin
pattern VPolarityExpr l <- VBuiltin (Polarity l) []
  where
    VPolarityExpr l = VBuiltin (Polarity l) []

type MonadPolaritySolver m = MonadTypeChecker PolarityBuiltin m

type PolaritySolver =
  forall m.
  (MonadPolaritySolver m) =>
  InstanceConstraintInfo PolarityBuiltin ->
  [VType PolarityBuiltin] ->
  Maybe (m (AuxiliaryConstraintProgress PolarityBuiltin))

solve :: PolarityRelation -> PolaritySolver
solve = \case
  NegPolarity -> solveNegPolarity
  QuantifierPolarity p q -> solveQuantifierPolarity p q
  AddPolarity p q -> solveAddPolarityOp p q
  ImpliesPolarity -> solveImplPolarity
  MaxPolarity -> solveMaxPolarityOp
  FunctionPolarity position -> solveFunctionPolarity position
  IfPolarity -> solveIfCondPolarity

solveNegPolarity :: PolaritySolver
solveNegPolarity info@(ctx, _) [arg1, res] = case arg1 of
  (getNMeta -> Just m) -> blockOn [m]
  VPolarityExpr pol -> Just $ do
    let resPol = VPolarityExpr $ negatePolarity (provenanceOf ctx) pol
    resEq <- createInstanceUnification info res resPol
    return $ Progress [resEq] []
  _ -> Nothing
solveNegPolarity _ _ = Nothing

solveQuantifierPolarity :: Provenance -> Quantifier -> PolaritySolver
solveQuantifierPolarity p q info@(ctx, _) [lam, res] = case lam of
  (getNMeta -> Just m) -> blockOn [m]
  (VPi binder resPol) -> Just $ do
    binderEq <- createInstanceUnification info (typeOf binder) (VPolarityExpr Unquantified)
    let tc = PolarityRelation $ AddPolarity p q
    resultPolarity <- normaliseClosure (toNamedBoundCtx $ boundContext ctx) binder resPol
    (_, addConstraint) <- createDerivedInstanceConstraint info Irrelevant (VBuiltin tc (explicit <$> [resultPolarity, res]))
    return $ Progress [binderEq] [addConstraint]
  _ -> Nothing
solveQuantifierPolarity _ _ _c _ = Nothing

solveAddPolarityOp :: Provenance -> Quantifier -> PolaritySolver
solveAddPolarityOp p q info [arg, res] = do
  case arg of
    (getNMeta -> Just m) -> blockOn [m]
    VPolarityExpr inputPol -> Just $ do
      let resPol = VPolarityExpr $ addPolarityOp p q inputPol
      domEq <- createInstanceUnification info res resPol
      return $ Progress [domEq] []
    _ -> Nothing
solveAddPolarityOp _ _ _ _ = Nothing

solveMaxPolarityOp :: PolaritySolver
solveMaxPolarityOp info [arg1, arg2, res] = case (arg1, arg2) of
  (VPolarityExpr pol1, VPolarityExpr pol2) -> Just $ do
    let pol3 = VPolarityExpr $ maxPolarityOp pol1 pol2
    resEq <- createInstanceUnification info res pol3
    return $ Progress [resEq] []
  (_, VPolarityExpr Unquantified) -> Just $ do
    resEq <- createInstanceUnification info arg1 res
    return $ Progress [resEq] []
  (VPolarityExpr Unquantified, _) -> Just $ do
    resEq <- createInstanceUnification info arg2 res
    return $ Progress [resEq] []
  (getNMeta -> Just m1, _) -> blockOn [m1]
  (_, getNMeta -> Just m2) -> blockOn [m2]
  _ -> Nothing
solveMaxPolarityOp _ _ = Nothing

solveImplPolarity :: PolaritySolver
solveImplPolarity info@(ctx, _) [arg1, arg2, res] = case (arg1, arg2) of
  (VPolarityExpr pol1, VPolarityExpr pol2) -> Just $ do
    let pol3 = VPolarityExpr $ implPolarityOp (provenanceOf ctx) pol1 pol2
    resEq <- createInstanceUnification info res pol3
    return $ Progress [resEq] []
  (getNMeta -> Just m1, _) -> blockOn [m1]
  (_, getNMeta -> Just m2) -> blockOn [m2]
  _ -> Nothing
solveImplPolarity _ _ = Nothing

solveFunctionPolarity :: FunctionPosition -> PolaritySolver
solveFunctionPolarity functionPosition info@(ctx, _) [arg, res] = case (arg, res) of
  (getNMeta -> Just m1, _) -> blockOn [m1]
  (VPolarityExpr pol, _) -> Just $ do
    let p = provenanceOf ctx
    let addFuncProv pp = PolFunctionProvenance p pp functionPosition
    let pol3 = VPolarityExpr $ mapPolarityProvenance addFuncProv pol
    resEq <- createInstanceUnification info res pol3
    return $ Progress [resEq] []
  (VPi binder1 closure1, VPi binder2 closure2) -> Just $ do
    let tc = PolarityRelation $ FunctionPolarity functionPosition
    (_, binderConstraint) <- createDerivedInstanceConstraint info Irrelevant (VBuiltin tc (explicit <$> [typeOf binder1, typeOf binder2]))
    let namedCtx = toNamedBoundCtx $ boundContext ctx
    body1 <- normaliseClosure namedCtx binder1 closure1
    body2 <- normaliseClosure namedCtx binder2 closure2
    (_, bodyConstraint) <- createDerivedInstanceConstraint info Irrelevant (VBuiltin tc (explicit <$> [body1, body2]))
    return $ Progress [] [binderConstraint, bodyConstraint]
  _ -> Nothing
solveFunctionPolarity _ _ _ = Nothing

solveIfCondPolarity :: PolaritySolver
solveIfCondPolarity info@(ctx, _) [pCond, pArg1, pArg2, pRes] = case pCond of
  (getNMeta -> Just m1) -> blockOn [m1]
  VPolarityExpr pol -> case pol of
    Unquantified -> solveMaxPolarityOp info [pArg1, pArg2, pRes]
    _ -> Just $ throwError $ QuantifiedIfCondition ctx
  _ -> Nothing
solveIfCondPolarity _ _ = Nothing

--------------------------------------------------------------------------------
-- Operations over polarities

negPolarityOp ::
  (PolarityProvenance -> PolarityProvenance) ->
  Polarity ->
  Polarity
negPolarityOp modProv pol =
  case pol of
    Unquantified -> Unquantified
    Quantified q pp -> Quantified (neg q) (modProv pp)
    MixedParallel pp1 pp2 -> MixedParallel (modProv pp2) (modProv pp1)
    -- We don't negate a mixed sequential polarity as its the top of the polarity
    -- lattice and we want to give as meaningful and localised error messages
    -- as possible.
    MixedSequential {} -> pol

negatePolarity ::
  Provenance ->
  Polarity ->
  Polarity
negatePolarity p = negPolarityOp (NegateProvenance p)

addPolarityOp :: Provenance -> Quantifier -> Polarity -> Polarity
addPolarityOp p q pol = case pol of
  Unquantified -> Quantified q (QuantifierProvenance p)
  Quantified q' pp -> if q == q' then pol else MixedSequential q p pp
  MixedParallel pp1 pp2 -> MixedSequential q p (if q == Forall then pp2 else pp1)
  MixedSequential {} -> pol

maxPolarityOp :: Polarity -> Polarity -> Polarity
maxPolarityOp pol1 pol2 = case (pol1, pol2) of
  (Unquantified, _) -> pol2
  (_, Unquantified) -> pol1
  (Quantified q1 pp1, Quantified q2 pp2)
    | q1 == q2 -> pol1
    | q1 == Forall -> MixedParallel pp1 pp2
    | otherwise -> MixedParallel pp2 pp1
  (Quantified {}, MixedParallel {}) -> pol2
  (MixedParallel {}, Quantified {}) -> pol1
  (MixedParallel {}, MixedParallel {}) -> pol1
  (MixedSequential {}, _) -> pol1
  (_, MixedSequential {}) -> pol2

implPolarityOp ::
  Provenance ->
  Polarity ->
  Polarity ->
  Polarity
implPolarityOp p pol1 pol2 =
  let negPol = negPolarityOp (LHSImpliesProvenance p)
   in -- `a => b` = not a or b
      maxPolarityOp (negPol pol1) pol2

--------------------------------------------------------------------------------
-- Other

getTypeClass :: (MonadCompile m) => InstanceGoal PolarityBuiltin -> m (PolarityRelation, Spine PolarityBuiltin)
getTypeClass = \case
  (InstanceGoal _ (PolarityRelation tc) args) -> return (tc, args)
  _ -> compilerDeveloperError "Unexpected non-type-class instance argument found."
