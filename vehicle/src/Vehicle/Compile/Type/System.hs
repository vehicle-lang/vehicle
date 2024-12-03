module Vehicle.Compile.Type.System where

import Vehicle.Compile.Context.Free (MonadFreeContext, getFreeEnv)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Builtin (NormalisableBuiltin)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Type.Builtin (TypableBuiltin)
import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Constraint.IndexSolver (solveDefaultIndexConstraints, solveIndexConstraint)
import Vehicle.Compile.Type.Constraint.InstanceDefaultSolver (addNewConstraintUsingDefaults)
import Vehicle.Compile.Type.Constraint.InstanceSolver
import Vehicle.Compile.Type.Constraint.LinearitySolver
import Vehicle.Compile.Type.Constraint.PolaritySolver
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad (createFreshInstanceConstraint, createFreshUnificationConstraint, freshMetaExpr)
import Vehicle.Compile.Type.Monad.Class (MonadTypeChecker)
import Vehicle.Compile.Type.Subsystem.InputOutputInsertion (addFunctionAuxiliaryInputOutputConstraints)
import Vehicle.Data.Builtin.Linearity
import Vehicle.Data.Builtin.Polarity
import Vehicle.Data.Builtin.Standard (Builtin (..), BuiltinType (..), TypeClass (..))
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

  restrictDeclType ::
    (MonadTypeChecker builtin m) =>
    RestrictedDecl ->
    DeclProvenance ->
    Type builtin ->
    m (Type builtin)

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
  restrictDeclType = restrictStandardDeclType
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

restrictStandardDeclType ::
  forall m.
  (MonadTypeChecker Builtin m) =>
  RestrictedDecl ->
  DeclProvenance ->
  Type Builtin ->
  m (Type Builtin)
restrictStandardDeclType declSort (ident, p) typ = do
  env <- getFreeEnv
  let tc = case declSort of
        RestrictedProperty -> ValidPropertyType
        RestrictedParameter s -> ValidParameterType s
        RestrictedDataset -> ValidDatasetType
        RestrictedNetwork -> ValidNetworkType

  let expr = BuiltinExpr p (TypeClass tc) [explicit typ]
  let origin = InstanceTypeRestrictionOrigin $ TypeRestrictionOrigin env (ident, provenanceOf typ) declSort typ
  _ <- createFreshInstanceConstraint mempty p origin Irrelevant expr
  return typ

-------------------------------------------------------------------------------
-- Linearity

instance HasTypeSystem LinearityBuiltin where
  convertFromStandardBuiltins = convertToLinearityTypes
  restrictDeclType = \case
    RestrictedNetwork -> restrictLinearityNetworkType
    RestrictedDataset -> assertConstantLinearity
    RestrictedParameter {} -> assertConstantLinearity
    RestrictedProperty {} -> const return
  solveInstance = solveLinearityConstraint
  addAuxiliaryInputOutputConstraints = addFunctionAuxiliaryInputOutputConstraints (LinearityRelation . FunctionLinearity)
  generateDefaultConstraint _ _ = return False

freshLinearityMeta :: (MonadTypeChecker LinearityBuiltin m) => Provenance -> m (Expr LinearityBuiltin)
freshLinearityMeta p = unnormalised <$> freshMetaExpr p (TypeUniverse p 0) mempty

convertToLinearityTypes ::
  forall m.
  (MonadTypeChecker LinearityBuiltin m) =>
  BuiltinUpdate m Builtin LinearityBuiltin
convertToLinearityTypes p b args = case b of
  BuiltinFunction f -> return $ normAppList (Builtin p (LinearityFunction f)) args
  BuiltinConstructor c -> return $ normAppList (Builtin p (LinearityConstructor c)) args
  BuiltinType s -> case s of
    Unit -> return $ Builtin p $ Linearity Constant
    Bool -> freshLinearityMeta p
    Index -> freshLinearityMeta p
    Nat -> freshLinearityMeta p
    Rat -> freshLinearityMeta p
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

restrictLinearityNetworkType ::
  forall m.
  (MonadTypeChecker LinearityBuiltin m) =>
  DeclProvenance ->
  Type LinearityBuiltin ->
  m (Type LinearityBuiltin)
restrictLinearityNetworkType (ident, p) networkType = do
  inputLin <- freshLinearityMeta p
  outputLin <- freshLinearityMeta p

  let inputLinBinder = Binder p (BinderDisplayForm OnlyType False) Explicit Relevant inputLin
  let functionNetworkType = Pi p inputLinBinder outputLin
  createFreshUnificationConstraint p mempty CheckingAuxiliary networkType functionNetworkType

  -- The linearity of the output of a network is the max of 1) Linear (as outputs
  -- are also variables) and 2) the linearity of its input. So prepend this
  -- constraint to the front of the type.
  logDebug MaxDetail "Appending `MaxLinearity` constraint to network type"
  let outputLinProvenance = Linear $ NetworkOutputProvenance p (nameOf ident)
  let linConstraintArgs = [LinearityExpr p outputLinProvenance, inputLin, outputLin]
  let linConstraint = App (Builtin p (LinearityRelation MaxLinearity)) (Arg p Explicit Relevant <$> linConstraintArgs)
  let linConstraintBinder = Binder p (BinderDisplayForm OnlyType False) (Instance True) Irrelevant linConstraint

  return $ Pi p linConstraintBinder functionNetworkType

assertConstantLinearity ::
  (MonadTypeChecker LinearityBuiltin m) =>
  DeclProvenance ->
  Type LinearityBuiltin ->
  m (Type LinearityBuiltin)
assertConstantLinearity (_, p) t = do
  createFreshUnificationConstraint p mempty CheckingAuxiliary (LinearityExpr p Constant) t
  return t

-------------------------------------------------------------------------------
-- Polarity

instance HasTypeSystem PolarityBuiltin where
  convertFromStandardBuiltins = convertToPolarityTypes
  restrictDeclType = \case
    RestrictedNetwork -> restrictPolarityNetworkType
    RestrictedDataset -> assertUnquantifiedPolarity
    RestrictedParameter {} -> assertUnquantifiedPolarity
    RestrictedProperty -> const return
  solveInstance = solvePolarityConstraint
  addAuxiliaryInputOutputConstraints = addFunctionAuxiliaryInputOutputConstraints (PolarityRelation . FunctionPolarity)
  generateDefaultConstraint _ _ = return False

freshPolarityMeta :: (MonadTypeChecker PolarityBuiltin m) => Provenance -> m (Expr PolarityBuiltin)
freshPolarityMeta p = unnormalised <$> freshMetaExpr p (TypeUniverse p 0) mempty

convertToPolarityTypes ::
  forall m.
  (MonadTypeChecker PolarityBuiltin m) =>
  BuiltinUpdate m Builtin PolarityBuiltin
convertToPolarityTypes p b args = case b of
  BuiltinConstructor c -> return $ normAppList (Builtin p (PolarityConstructor c)) args
  BuiltinFunction f -> return $ normAppList (Builtin p (PolarityFunction f)) args
  BuiltinType s -> case s of
    Unit -> return $ PolarityExpr p Unquantified
    Bool -> freshPolarityMeta p
    Index -> return $ PolarityExpr p Unquantified
    Nat -> return $ PolarityExpr p Unquantified
    Rat -> freshPolarityMeta p
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

restrictPolarityNetworkType ::
  forall m.
  (MonadTypeChecker PolarityBuiltin m) =>
  DeclProvenance ->
  Type PolarityBuiltin ->
  m (Type PolarityBuiltin)
restrictPolarityNetworkType (_, p) networkType = do
  let inputPol = PolarityExpr p Unquantified
  let outputPol = PolarityExpr p Unquantified

  let inputPolBinder = Binder p (BinderDisplayForm OnlyType False) Explicit Relevant inputPol
  let functionNetworkType = Pi p inputPolBinder outputPol
  createFreshUnificationConstraint p mempty CheckingAuxiliary networkType functionNetworkType
  return networkType

assertUnquantifiedPolarity ::
  (MonadTypeChecker PolarityBuiltin m) =>
  DeclProvenance ->
  Type PolarityBuiltin ->
  m (Type PolarityBuiltin)
assertUnquantifiedPolarity (_, p) t = do
  createFreshUnificationConstraint p mempty CheckingAuxiliary (PolarityExpr p Unquantified) t
  return t
