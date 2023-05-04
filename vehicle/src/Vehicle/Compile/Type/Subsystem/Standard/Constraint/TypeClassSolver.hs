module Vehicle.Compile.Type.Subsystem.Standard.Constraint.TypeClassSolver
  ( solveTypeClassConstraint,
  )
where

import Control.Monad (MonadPlus (..))
import Control.Monad.Except (throwError)
import Data.Maybe (mapMaybe)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (forceHead)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta (MetaSet)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Subsystem.Standard.Constraint.Core
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Compile.Type.Subsystem.Standard.Patterns
import Vehicle.Expr.Normalised
import Vehicle.Libraries.StandardLibrary

--------------------------------------------------------------------------------
-- Solver

solveTypeClassConstraint :: (MonadInstance m) => WithContext StandardTypeClassConstraint -> m ()
solveTypeClassConstraint constraint@(WithContext (Has m tc spine) ctx) = do
  progress <- solve tc constraint spine
  case progress of
    Left metas -> do
      let blockedConstraint = blockConstraintOn (mapObject TypeClassConstraint constraint) metas
      addConstraints [blockedConstraint]
    Right (newConstraints, solution) -> do
      solveMeta m solution (boundContext ctx)
      addConstraints newConstraints

type MonadTypeClass m =
  ( TCM StandardBuiltinType m
  )

type TypeClassProgress = Either MetaSet ([WithContext StandardConstraint], StandardExpr)

-- | Function signature for constraints solved by type class resolution.
-- This should eventually be refactored out so all are solved by instance
-- search.
type TypeClassSolver =
  forall m.
  (MonadInstance m) =>
  WithContext StandardTypeClassConstraint ->
  [StandardNormType] ->
  m TypeClassProgress

solve :: StandardBuiltinType -> TypeClassSolver
solve (StandardTypeClass tc) = case tc of
  HasOrd ord -> solveHasOrd ord
  HasQuantifier q -> solveHasQuantifier q
  NatInDomainConstraint -> solveInDomain
  _ -> \_ _ ->
    compilerDeveloperError $
      "Expected the class" <+> quotePretty tc <+> "to be solved via instance search"
solve tc = \_ _ -> compilerDeveloperError $ "Invalid instance argument type" <+> quotePretty tc

--------------------------------------------------------------------------------
-- HasOrd

solveHasOrd :: OrderOp -> TypeClassSolver
solveHasOrd op c [arg1, arg2]
  | allOf args isNMeta = blockOnMetas args
  | anyOf args isIndexType = solveIndexComparisonOp ctx arg1 arg2 (Order OrderIndex op)
  | anyOf args isNatType = solveSimpleComparisonOp ctx arg1 arg2 (Order OrderNat op)
  | anyOf args isIntType = solveSimpleComparisonOp ctx arg1 arg2 (Order OrderInt op)
  | anyOf args isRatType = solveSimpleComparisonOp ctx arg1 arg2 (Order OrderRat op)
  | otherwise = blockOrThrowErrors ctx args tcError
  where
    ctx = contextOf c
    args = [arg1, arg2]
    allowedTypes = fmap pretty [Index, Nat, Int, Rat]
    tcError =
      tcArgError ctx arg1 (OrderTC op) allowedTypes 1 2
        <> tcArgError ctx arg2 (OrderTC op) allowedTypes 1 2
solveHasOrd _ c _ = malformedConstraintError c

--------------------------------------------------------------------------------
-- HasQuantifier

solveHasQuantifier :: Quantifier -> TypeClassSolver
solveHasQuantifier _ _ [lamType]
  | isNMeta lamType = blockOnMetas [lamType]
solveHasQuantifier q c [VPi binder body]
  | isNMeta domain = blockOnMetas [domain]
  | isIndexType domain = solveIndexQuantifier q ctx binder body
  | isNatType domain = solveSimpleQuantifier QuantNat q ctx binder body
  | isIntType domain = solveSimpleQuantifier QuantInt q ctx binder body
  | isRatType domain = solveSimpleQuantifier QuantRat q ctx binder body
  | isVectorType domain = solveVectorQuantifier q ctx binder body
  | otherwise = blockOrThrowErrors ctx [domain] tcError
  where
    ctx = contextOf c
    domain = typeOf binder
    tcError = [FailedQuantifierConstraintDomain ctx domain q]
solveHasQuantifier _ c _ = malformedConstraintError c

type HasQuantifierSolver =
  forall m.
  (MonadTypeClass m) =>
  Quantifier ->
  StandardConstraintContext ->
  StandardNormBinder ->
  StandardNormType ->
  m TypeClassProgress

solveIndexQuantifier :: HasQuantifierSolver
solveIndexQuantifier q c domainBinder body = do
  let p = provenanceOf c

  (domainEq, indexSize) <- unifyWithIndexType c (typeOf domainBinder)
  bodyEq <- unify c body VBoolType

  let method = identifierOf $ if q == Forall then StdForallIndex else StdExistsIndex
  let solution =
        App
          p
          (FreeVar p method)
          [ ImplicitArg p indexSize
          ]

  return $ Right ([domainEq, bodyEq], solution)

solveSimpleQuantifier :: QuantifierDomain -> HasQuantifierSolver
solveSimpleQuantifier dom q c _domainBinder body = do
  let p = provenanceOf c
  bodyEq <- unify c body VBoolType
  let solution = NullaryBuiltinFunctionExpr p (Quantifier q dom)
  return $ Right ([bodyEq], solution)

solveVectorQuantifier :: HasQuantifierSolver
solveVectorQuantifier q c domainBinder body = do
  let p = provenanceOf c
  dim <- freshDimMeta c
  (domainEq, vecElem) <- unifyWithVectorType c dim (typeOf domainBinder)

  -- Recursively check that you can quantify over it.
  let elemDomainBinder = replaceBinderType (normalised vecElem) domainBinder
  (metaExpr, recTC) <- createTC c (StandardTypeClass (HasQuantifier q)) [VPi elemDomainBinder body]

  let solution =
        BuiltinFunctionExpr
          p
          (Quantifier q QuantVec)
          [ ImplicitArg p (unnormalised vecElem),
            ImplicitArg p (unnormalised dim),
            InstanceArg p metaExpr
          ]

  return $ Right ([domainEq, recTC], solution)

--------------------------------------------------------------------------------
-- InDomain

solveInDomain :: TypeClassSolver
solveInDomain c [value, typ] = case typ of
  (getNMeta -> Just {}) -> blockOnMetas [typ]
  VNatType {} -> return $ Right ([], UnitLiteral p)
  VIntType {} -> return $ Right ([], UnitLiteral p)
  VRatType {} -> return $ Right ([], UnitLiteral p)
  VIndexType size -> case size of
    (getNMeta -> Just {}) -> blockOnMetas [size]
    VNatLiteral m -> case value of
      (getNMeta -> Just {}) -> blockOnMetas [value]
      VNatLiteral n
        | m > n -> return $ irrelevant []
        | otherwise -> throwError $ FailedNatLitConstraintTooBig ctx n m
      _ -> malformedConstraintError c
    _ -> throwError $ FailedNatLitConstraintUnknown ctx value size
  _ -> malformedConstraintError c
  where
    ctx = contextOf c
    p = provenanceOf ctx
solveInDomain c _ = malformedConstraintError c

--------------------------------------------------------------------------------
-- Utilities

unifyWithIndexType ::
  (MonadTypeClass m) =>
  StandardConstraintContext ->
  StandardNormType ->
  m (WithContext StandardConstraint, StandardExpr)
unifyWithIndexType c t = do
  let p = provenanceOf c
  indexSize <- freshMetaExpr p (NatType p) (boundContext c)
  eq <- unify c t (VIndexType (normalised indexSize))
  return (eq, unnormalised indexSize)

unifyWithVectorType ::
  (MonadTypeClass m) =>
  StandardConstraintContext ->
  StandardGluedExpr ->
  StandardNormType ->
  m (WithContext StandardConstraint, StandardGluedType)
unifyWithVectorType c dim t = do
  let p = provenanceOf c
  elemType <- freshMetaExpr p (TypeUniverse p 0) (boundContext c)
  eq <- unify c t (VVectorType (normalised elemType) (normalised dim))
  return (eq, elemType)

freshDimMeta :: (MonadTypeClass m) => StandardConstraintContext -> m StandardGluedExpr
freshDimMeta c = do
  let p = provenanceOf c
  freshMetaExpr p (NatType p) (boundContext c)

solveSimpleComparisonOp ::
  (MonadTypeClass m) =>
  StandardConstraintContext ->
  StandardNormType ->
  StandardNormType ->
  BuiltinFunction ->
  m TypeClassProgress
solveSimpleComparisonOp c arg1 arg2 fn = do
  let p = provenanceOf c
  argEq <- unify c arg1 arg2
  let solution = NullaryBuiltinFunctionExpr p fn
  return $ Right ([argEq], solution)

solveIndexComparisonOp ::
  (MonadTypeClass m) =>
  StandardConstraintContext ->
  StandardNormType ->
  StandardNormType ->
  BuiltinFunction ->
  m TypeClassProgress
solveIndexComparisonOp c arg1 arg2 fn = do
  let p = provenanceOf c
  (arg1Eq, _size1) <- unifyWithIndexType c arg1
  (arg2Eq, _size2) <- unifyWithIndexType c arg2
  let solution = NullaryBuiltinFunctionExpr p fn
  return $ Right ([arg1Eq, arg2Eq], solution)

irrelevant :: [WithContext StandardConstraint] -> TypeClassProgress
irrelevant newConstraints = Right (newConstraints, UnitLiteral mempty)

blockOrThrowErrors ::
  (MonadTypeClass m) =>
  StandardConstraintContext ->
  [StandardNormExpr] ->
  [CompileError] ->
  m TypeClassProgress
blockOrThrowErrors ctx args err = do
  -- TODO forcing should be incorporated beforehand.
  blockingMetas <- MetaSet.unions . fmap snd <$> traverse (forceHead ctx) args
  if MetaSet.null blockingMetas
    then throwError (head err)
    else return $ Left blockingMetas

unless2 :: (MonadPlus m) => Bool -> a -> m a
unless2 p a = if not p then return a else mzero

tcArgError ::
  StandardConstraintContext ->
  StandardNormType ->
  TypeClassOp ->
  [UnAnnDoc] ->
  Int ->
  Int ->
  [CompileError]
tcArgError c arg op allowedTypes argIndex numberOfArgs =
  unless2
    (isNMeta arg)
    (FailedBuiltinConstraintArgument c op arg allowedTypes argIndex numberOfArgs)

blockOnMetas :: (MonadCompile m) => [StandardNormExpr] -> m TypeClassProgress
blockOnMetas args = do
  let metas = mapMaybe getNMeta args
  return $ Left $ MetaSet.fromList metas

isIndexType :: StandardNormExpr -> Bool
isIndexType (VBuiltinType Index _) = True
isIndexType _ = False

isNatType :: StandardNormExpr -> Bool
isNatType (VBuiltinType Nat _) = True
isNatType _ = False

isIntType :: StandardNormExpr -> Bool
isIntType (VBuiltinType Int _) = True
isIntType _ = False

isRatType :: StandardNormExpr -> Bool
isRatType (VBuiltinType Rat _) = True
isRatType _ = False

isVectorType :: StandardNormExpr -> Bool
isVectorType (VBuiltinType Vector _) = True
isVectorType _ = False
