module Vehicle.Compile.Type.Subsystem.Standard.Constraint.TypeClassSolver
  ( solveTypeClassConstraint,
  )
where

import Control.Monad.Except (throwError)
import Data.Maybe (mapMaybe)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Force (forceHead)
import Vehicle.Compile.Type.Meta (MetaSet)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Meta.Substitution
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Compile.Type.Subsystem.Standard.Interface
import Vehicle.Compile.Type.Subsystem.Standard.Patterns
import Vehicle.Expr.Normalised
import Vehicle.Libraries.StandardLibrary

--------------------------------------------------------------------------------
-- Solver

solveTypeClassConstraint :: (MonadTypeClass m) => WithContext StandardInstanceConstraint -> m ()
solveTypeClassConstraint constraint = do
  normConstraint@(WithContext (Has m _ expr) ctx) <- substMetas constraint
  logDebug MaxDetail $ "Forced type-class:" <+> prettyFriendly normConstraint

  (tc, spine) <- getTypeClass expr
  progress <- solve tc normConstraint (mapMaybe getExplicitArg spine)
  case progress of
    Left metas -> do
      let blockedConstraint = blockConstraintOn (mapObject InstanceConstraint normConstraint) metas
      addConstraints [blockedConstraint]
    Right (newConstraints, solution) -> do
      solveMeta m solution (boundContext ctx)
      addConstraints newConstraints

getTypeClass :: (MonadCompile m) => StandardNormExpr -> m (TypeClass, StandardSpine)
getTypeClass = \case
  VBuiltin (TypeClass tc) args -> return (tc, args)
  _ -> compilerDeveloperError "Unexpected non-type-class instance argument found."

type MonadTypeClass m =
  ( TCM StandardBuiltin m
  )

type TypeClassProgress = Either MetaSet ([WithContext StandardConstraint], StandardExpr)

-- | Function signature for constraints solved by type class resolution.
-- This should eventually be refactored out so all are solved by instance
-- search.
type TypeClassSolver =
  forall m.
  (MonadTypeClass m) =>
  WithContext StandardInstanceConstraint ->
  [StandardNormType] ->
  m TypeClassProgress

solve :: TypeClass -> TypeClassSolver
solve tc = case tc of
  HasQuantifier q -> solveHasQuantifier q
  _ -> \_ _ ->
    compilerDeveloperError $
      "Expected the class" <+> quotePretty tc <+> "to be solved via instance search"

--------------------------------------------------------------------------------
-- HasQuantifier

solveHasQuantifier :: Quantifier -> TypeClassSolver
solveHasQuantifier _ _ [lamType]
  | isNMeta lamType = blockOnMetas [lamType]
solveHasQuantifier q c [VPi binder body]
  | isNMeta domain = blockOnMetas [domain]
  | isIndexType domain = solveIndexQuantifier q ctx binder body
  | isRatType domain = solveRatQuantifier q ctx binder body
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
          [ RelevantExplicitArg p indexSize
          ]

  return $ Right ([domainEq, bodyEq], solution)

solveRatQuantifier :: HasQuantifierSolver
solveRatQuantifier q c _domainBinder body = do
  let p = provenanceOf c
  bodyEq <- unify c body VBoolType
  let solution = NullaryBuiltinFunctionExpr p (Quantifier q)
  return $ Right ([bodyEq], solution)

solveVectorQuantifier :: HasQuantifierSolver
solveVectorQuantifier q c domainBinder body = do
  let p = provenanceOf c
  dim <- freshDimMeta c
  (domainEq, vecElem) <- unifyWithVectorType c dim (typeOf domainBinder)

  -- Recursively check that you can quantify over it.
  let elemDomainBinder = replaceBinderType (normalised vecElem) domainBinder
  let expr = VBuiltin (TypeClass (HasQuantifier q)) [RelevantExplicitArg mempty (VPi elemDomainBinder body)]
  (metaExpr, recTC) <- createTC c Relevant expr

  let solution =
        BuiltinFunctionExpr
          p
          (Quantifier q)
          [ RelevantImplicitArg p (unnormalised vecElem),
            RelevantImplicitArg p (unnormalised dim),
            RelevantInstanceArg p metaExpr
          ]

  return $ Right ([domainEq, recTC], solution)

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

blockOnMetas :: (MonadCompile m) => [StandardNormExpr] -> m TypeClassProgress
blockOnMetas args = do
  let metas = mapMaybe getNMeta args
  return $ Left $ MetaSet.fromList metas

isIndexType :: StandardNormExpr -> Bool
isIndexType (VBuiltinType Index _) = True
isIndexType _ = False

isRatType :: StandardNormExpr -> Bool
isRatType (VBuiltinType Rat _) = True
isRatType _ = False

isVectorType :: StandardNormExpr -> Bool
isVectorType (VBuiltinType Vector _) = True
isVectorType _ = False
