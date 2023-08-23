{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Backend.LossFunction.TypeSystem
  ( module Core,
  )
where

import Control.Monad (when)
import Vehicle.Backend.LossFunction.TypeSystem.Core as Core
import Vehicle.Backend.LossFunction.TypeSystem.InstanceDefaults ()
import Vehicle.Backend.LossFunction.TypeSystem.Type
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Quote (unnormalise)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Constraint.IndexSolver
import Vehicle.Compile.Type.Constraint.InstanceDefaultSolver (addNewConstraintUsingDefaults)
import Vehicle.Compile.Type.Constraint.InstanceSolver
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Subsystem.Standard.Core qualified as S
import Vehicle.Expr.BuiltinInterface
import Vehicle.Expr.Normalised (GluedExpr (..), GluedType, VType, Value (..), isNMeta)
import Vehicle.Syntax.Builtin (BuiltinType (..))

instance TypableBuiltin LossBuiltin where
  typeBuiltin = typeLossBuiltin

instance HasTypeSystem LossBuiltin where
  convertFromStandardBuiltins = convertToLossTypes
  useDependentMetas _ = False
  restrictNetworkType = checkNetworkType
  restrictDatasetType = checkDatasetType
  restrictParameterType = checkParameterType
  restrictPropertyType = checkPropertyType
  solveInstance = solveInstanceConstraint
  addAuxiliaryInputOutputConstraints = return
  generateDefaultConstraint = addNewConstraintUsingDefaults

solveInstanceConstraint ::
  (TCM LossBuiltin m) =>
  InstanceCandidateDatabase LossBuiltin ->
  WithContext (InstanceConstraint LossBuiltin) ->
  m ()
solveInstanceConstraint database constraint@(WithContext (Resolve _ _ _ goal) _) = do
  case goal of
    VBuiltin NatInDomainConstraint _ -> solveIndexConstraint constraint
    _ -> resolveInstance database constraint

convertToLossTypes ::
  forall m.
  (MonadTypeChecker LossBuiltin m) =>
  BuiltinUpdate m Ix S.Builtin LossBuiltin
convertToLossTypes p1 p2 b args = case b of
  S.BuiltinType t -> case t of
    S.Bool -> do
      -- We need to add a meta variable to the application as
      -- the instance insertion mechanism doesn't kick in unless the
      -- type-class operation has at least one arg.
      meta <- unnormalised <$> freshMetaExpr p1 (TypeUniverse p1 0) mempty
      let constraintTC = BuiltinExpr p1 (LossTC IsBoolType) [Arg p1 Explicit Relevant meta]
      _ <- createFreshInstanceConstraint mempty (BoolType p1, mempty, TypeUniverse p1 0) Irrelevant constraintTC
      return meta
    _ -> return $ normAppList p1 (Builtin p2 (BuiltinType t)) args
  S.BuiltinFunction f -> case f of
    S.Order S.OrderRat op -> return $ mkTypeClassOp (RatOrderTC op) args
    S.Equals S.EqRat op -> return $ mkTypeClassOp (RatEqTC op) args
    S.And -> return $ mkTypeClassOp AndTC args
    S.Or -> return $ mkTypeClassOp OrTC args
    S.Implies -> return $ mkTypeClassOp ImpliesTC args
    S.Not -> return $ mkTypeClassOp NotTC args
    S.Quantifier q -> return $ mkTypeClassOp (QuantTC q) args
    _ -> return $ normAppList p1 (Builtin p2 (BuiltinFunction f)) args
  S.BuiltinConstructor c -> case c of
    S.LBool v -> return $ mkTypeClassOp (LBoolTC v) args
    _ -> return $ normAppList p1 (Builtin p2 (BuiltinConstructor c)) args
  S.TypeClass {} -> monomorphisationError "TypeClass"
  S.TypeClassOp {} -> monomorphisationError "TypeClassOp"
  S.NatInDomainConstraint -> monomorphisationError "TypeClass"
  where
    monomorphisationError :: Doc () -> m a
    monomorphisationError name =
      compilerDeveloperError $
        "Monomorphisation should have got rid of" <+> name <+> "but found" <+> prettyVerbose args

    mkTypeClassOp :: LossTypeClassOp -> [Arg Ix LossBuiltin] -> Expr Ix LossBuiltin
    mkTypeClassOp op = normAppList p1 (Builtin p2 (LossTCOp op))

checkNetworkType ::
  forall m.
  (MonadTypeChecker LossBuiltin m) =>
  DeclProvenance ->
  GluedType LossBuiltin ->
  m (Type Ix LossBuiltin)
checkNetworkType _ networkType = return $ unnormalised networkType

checkDatasetType ::
  forall m.
  (MonadTypeChecker LossBuiltin m) =>
  DeclProvenance ->
  GluedType LossBuiltin ->
  m (Type Ix LossBuiltin)
checkDatasetType _ datasetType = return $ unnormalised datasetType

checkParameterType ::
  (MonadTypeChecker LossBuiltin m) =>
  ParameterSort ->
  DeclProvenance ->
  GluedType LossBuiltin ->
  m (Type Ix LossBuiltin)
checkParameterType _sort (_, p) parameterType = do
  let unnormType = unnormalised parameterType
  -- Boolean parameters should remain booleans, rather than be converted to a loss.
  when (isNMeta $ normalised parameterType) $ do
    createFreshUnificationConstraint p mempty CheckingAuxiliary (BoolType p) unnormType
  return unnormType

checkPropertyType ::
  forall m.
  (MonadTypeChecker LossBuiltin m) =>
  DeclProvenance ->
  GluedType LossBuiltin ->
  m ()
checkPropertyType (ident, p) parameterType = go (normalised parameterType)
  where
    -- We need to recurse into the element type here, as we're converting all `Bool`s
    -- from the standard typing system to meta-variables. Therefore if we simply constrain
    -- the overall type, the instance solver can't reject unifying that meta-variable
    -- with a `Vector` type.
    go :: VType LossBuiltin -> m ()
    go = \case
      VBuiltinType Vector [tElem] -> go (argExpr tElem)
      typ -> do
        -- The basic element of properties should always have the loss type.
        let unnormType = unnormalise 0 typ
        let constraintType = App p (Builtin p (LossTC ValidPropertyBaseType)) [Arg p Explicit Relevant unnormType]
        let origin = (FreeVar p ident, mempty, constraintType)
        _ <- createFreshInstanceConstraint mempty origin Irrelevant constraintType
        return ()
