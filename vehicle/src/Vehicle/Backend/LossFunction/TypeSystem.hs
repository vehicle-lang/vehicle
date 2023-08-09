{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Backend.LossFunction.TypeSystem
  ( module Core,
  )
where

import Vehicle.Backend.LossFunction.TypeSystem.AnnotationRestrictions (checkDatasetType, checkNetworkType, checkParameterType)
import Vehicle.Backend.LossFunction.TypeSystem.Core as Core
import Vehicle.Backend.LossFunction.TypeSystem.Type
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Constraint.IndexSolver
import Vehicle.Compile.Type.Constraint.InstanceSolver
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Monad.Class
import Vehicle.Compile.Type.Subsystem.Standard.Core qualified as S
import Vehicle.Expr.DeBruijn (Ix)
import Vehicle.Expr.Normalised (GluedExpr (..), Value (..))

instance TypableBuiltin LossBuiltin where
  convertFromStandardTypes = convertToLossTypes
  useDependentMetas _ = False
  typeBuiltin = typeLossBuiltin
  restrictNetworkType = checkNetworkType
  restrictDatasetType = checkDatasetType
  restrictParameterType = checkParameterType
  restrictPropertyType _ _ = return ()
  solveInstance = solveInstanceConstraint
  addAuxiliaryInputOutputConstraints = return
  generateDefaultConstraint = const $ return False

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
    S.Bool -> unnormalised <$> freshLossMeta p2
    _ -> return $ normAppList p1 (Builtin p2 (BuiltinType t)) args
  S.BuiltinFunction f -> case f of
    S.Order S.OrderRat op -> return $ mkTypeClassOp (RatOrderTC op)
    S.Equals S.EqRat op -> return $ mkTypeClassOp (RatEqTC op)
    S.And -> return $ mkTypeClassOp AndTC
    S.Or -> return $ mkTypeClassOp OrTC
    S.Implies -> return $ mkTypeClassOp ImpliesTC
    S.Not -> return $ mkTypeClassOp NotTC
    S.Quantifier q -> return $ mkTypeClassOp (QuantTC q)
    _ -> return $ normAppList p1 (Builtin p2 (BuiltinFunction f)) args
  S.BuiltinConstructor c -> return $ normAppList p1 (Builtin p2 (BuiltinConstructor c)) args
  S.TypeClass {} ->
    monomorphisationError "TypeClass"
  S.TypeClassOp {} ->
    compilerDeveloperError "Type class operations should have been resolved before converting to other type systems"
  S.NatInDomainConstraint -> monomorphisationError "TypeClass"
  where
    monomorphisationError :: Doc () -> m a
    monomorphisationError name =
      compilerDeveloperError $
        "Monomorphisation should have got rid of partially applied" <+> name <+> "types but found" <+> prettyVerbose args

    mkTypeClassOp :: LossTypeClassOp -> Expr Ix LossBuiltin
    mkTypeClassOp op = normAppList p1 (Builtin p2 (LossTCOp op)) args

freshLossMeta :: (MonadTypeChecker LossBuiltin m) => Provenance -> m (GluedExpr LossBuiltin)
freshLossMeta p = snd <$> freshMeta p (TypeUniverse p 0) mempty
