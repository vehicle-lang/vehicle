module Vehicle.Compile.Type.System where

import Vehicle.Compile.Context.Free (MonadFreeContext)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Builtin (NormalisableBuiltin)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Type.Builtin (TypableBuiltin)
import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Constraint.IndexSolver (solveDefaultIndexConstraints, solveIndexConstraint)
import Vehicle.Compile.Type.Constraint.InstanceDefaultSolver (addNewConstraintUsingDefaults)
import Vehicle.Compile.Type.Constraint.InstanceSolver
import Vehicle.Compile.Type.Constraint.LinearityAnnotationRestrictions
import Vehicle.Compile.Type.Constraint.LinearitySolver
import Vehicle.Compile.Type.Constraint.PolarityAnnotationRestrictions
import Vehicle.Compile.Type.Constraint.PolaritySolver
import Vehicle.Compile.Type.Constraint.StandardAnnotationRestrictions
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad (freshMetaExpr)
import Vehicle.Compile.Type.Monad.Class (MonadTypeChecker)
import Vehicle.Compile.Type.Subsystem.InputOutputInsertion (addFunctionAuxiliaryInputOutputConstraints)
import Vehicle.Data.Builtin.Linearity
import Vehicle.Data.Builtin.Polarity
import Vehicle.Data.Builtin.Standard (Builtin (..), BuiltinType (..))
import Vehicle.Data.Code.Value

-- | The type-checking monad.
type TCM builtin m =
  ( MonadTypeChecker builtin m,
    HasTypeSystem builtin
  )

-- | A class that provides an abstract interface for a set of builtins.
class (Eq builtin, NormalisableBuiltin builtin, TypableBuiltin builtin) => HasTypeSystem builtin where
  convertFromStandardBuiltins ::
    (MonadTypeChecker builtin m) =>
    BuiltinUpdate m Builtin builtin

  restrictNetworkType ::
    (MonadTypeChecker builtin m) =>
    DeclProvenance ->
    GluedType builtin ->
    m (Type builtin)

  restrictDatasetType ::
    (MonadTypeChecker builtin m) =>
    DeclProvenance ->
    GluedType builtin ->
    m (Type builtin)

  restrictParameterType ::
    (MonadTypeChecker builtin m) =>
    ParameterSort ->
    DeclProvenance ->
    GluedType builtin ->
    m (Type builtin)

  restrictPropertyType ::
    (MonadTypeChecker builtin m) =>
    DeclProvenance ->
    GluedType builtin ->
    m ()

  addAuxiliaryInputOutputConstraints ::
    (MonadTypeChecker builtin m) => Decl builtin -> m (Decl builtin)

  generateDefaultConstraint ::
    (MonadTypeChecker builtin m) =>
    InstanceDatabase builtin ->
    Maybe (Decl builtin) ->
    m Bool

  -- | Solves a type-class constraint
  solveInstance ::
    (MonadTypeChecker builtin m, MonadFreeContext builtin m) =>
    InstanceDatabase builtin ->
    WithContext (InstanceConstraint builtin) ->
    m ()

-----------------------------------------------------------------------------
-- Standard builtins
-----------------------------------------------------------------------------

instance HasTypeSystem Builtin where
  convertFromStandardBuiltins = convertToTypingBuiltins
  restrictNetworkType = restrictStandardNetworkType
  restrictDatasetType = restrictStandardDatasetType
  restrictParameterType = restrictStandardParameterType
  restrictPropertyType = restrictStandardPropertyType
  solveInstance = solveStandardInstanceConstraint
  addAuxiliaryInputOutputConstraints = return
  generateDefaultConstraint = addNewConstraintUsingDefaults solveDefaultIndexConstraints

convertToTypingBuiltins :: (MonadCompile m) => BuiltinUpdate m Builtin Builtin
convertToTypingBuiltins p t args = return $ normAppList (Builtin p t) args

solveStandardInstanceConstraint ::
  (MonadTypeChecker Builtin m) =>
  InstanceDatabase Builtin ->
  WithContext (InstanceConstraint Builtin) ->
  m ()
solveStandardInstanceConstraint database constraint = do
  case instanceGoal (objectIn constraint) of
    VBuiltin NatInDomainConstraint _ -> solveIndexConstraint constraint
    VBuiltin {} -> solveInstanceConstraint database constraint
    _ -> malformedConstraintError constraint

-------------------------------------------------------------------------------
-- Linearity

instance HasTypeSystem LinearityBuiltin where
  convertFromStandardBuiltins = convertToLinearityTypes
  restrictNetworkType = checkLinearityNetworkType
  restrictDatasetType = assertConstantLinearity
  restrictParameterType = const assertConstantLinearity
  restrictPropertyType _ _ = return ()
  solveInstance = solveLinearityConstraint
  addAuxiliaryInputOutputConstraints = addFunctionAuxiliaryInputOutputConstraints (LinearityRelation . FunctionLinearity)
  generateDefaultConstraint _ _ = return False

freshLinearityMeta :: (MonadTypeChecker LinearityBuiltin m) => Provenance -> m (GluedExpr LinearityBuiltin)
freshLinearityMeta p = freshMetaExpr p (TypeUniverse p 0) mempty

convertToLinearityTypes ::
  forall m.
  (MonadTypeChecker LinearityBuiltin m) =>
  BuiltinUpdate m Builtin LinearityBuiltin
convertToLinearityTypes p b args = case b of
  BuiltinFunction f -> return $ normAppList (Builtin p (LinearityFunction f)) args
  BuiltinConstructor c -> return $ normAppList (Builtin p (LinearityConstructor c)) args
  BuiltinType s -> case s of
    Unit -> return $ Builtin p $ Linearity Constant
    Bool -> unnormalised <$> freshLinearityMeta p
    Index -> unnormalised <$> freshLinearityMeta p
    Nat -> unnormalised <$> freshLinearityMeta p
    Rat -> unnormalised <$> freshLinearityMeta p
    List -> case args of
      [tElem] -> return $ argExpr tElem
      _ -> monomorphisationError "List"
    Vector -> case args of
      [tElem] -> return $ argExpr tElem
      _ -> monomorphisationError "Vector"
  TypeClass {} -> monomorphisationError "TypeClass"
  TypeClassOp {} -> monomorphisationError "TypeClassOp"
  NatInDomainConstraint -> monomorphisationError "IndexConstraints"
  where
    monomorphisationError :: Doc () -> m a
    monomorphisationError name =
      compilerDeveloperError $
        "Monomorphisation should have got rid of" <+> squotes name <+> "s but found" <+> prettyVerbose args

-------------------------------------------------------------------------------
-- Polarity

instance HasTypeSystem PolarityBuiltin where
  convertFromStandardBuiltins = convertToPolarityTypes
  restrictNetworkType = checkNetworkType
  restrictDatasetType = assertUnquantifiedPolarity
  restrictParameterType = const assertUnquantifiedPolarity
  restrictPropertyType _ _ = return ()
  solveInstance = solvePolarityConstraint
  addAuxiliaryInputOutputConstraints = addFunctionAuxiliaryInputOutputConstraints (PolarityRelation . FunctionPolarity)
  generateDefaultConstraint _ _ = return False

freshPolarityMeta :: (MonadTypeChecker PolarityBuiltin m) => Provenance -> m (GluedExpr PolarityBuiltin)
freshPolarityMeta p = freshMetaExpr p (TypeUniverse p 0) mempty

convertToPolarityTypes ::
  forall m.
  (MonadTypeChecker PolarityBuiltin m) =>
  BuiltinUpdate m Builtin PolarityBuiltin
convertToPolarityTypes p b args = case b of
  BuiltinConstructor c -> return $ normAppList (Builtin p (PolarityConstructor c)) args
  BuiltinFunction f -> return $ normAppList (Builtin p (PolarityFunction f)) args
  BuiltinType s -> case s of
    Unit -> return $ PolarityExpr p Unquantified
    Bool -> unnormalised <$> freshPolarityMeta p
    Index -> return $ PolarityExpr p Unquantified
    Nat -> return $ PolarityExpr p Unquantified
    Rat -> unnormalised <$> freshPolarityMeta p
    List -> case args of
      [tElem] -> return $ argExpr tElem
      _ -> monomorphisationError "List"
    Vector -> case args of
      [tElem] -> return $ argExpr tElem
      _ -> monomorphisationError "Vector"
  TypeClass {} -> monomorphisationError "TypeClass"
  TypeClassOp {} -> monomorphisationError "TypeClassOp"
  NatInDomainConstraint -> monomorphisationError "IndexConstraints"
  where
    monomorphisationError :: Doc () -> m a
    monomorphisationError name =
      compilerDeveloperError $
        "Monomorphisation should have got rid of partially applied" <+> name <+> "types but found" <+> prettyVerbose args
