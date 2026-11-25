{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Data.Builtin.Standard.Type () where

import Data.Foldable (traverse_)
import Data.Foldable qualified as Foldable
import Data.Proxy (Proxy (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Scope.Core
import Vehicle.Compile.Scope.Records
import Vehicle.Compile.Sugar.Core
import Vehicle.Compile.Type.Bidirectional (createFreshUnificationConstraint)
import Vehicle.Compile.Type.Constraint.InstanceDefaultSolver
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Monad.Class
import Vehicle.Compile.Type.System
import Vehicle.Data.AST.Expr.Desugared qualified as D (Expr (..), normAppList)
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Builtin.Interface.Type
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Builtin.Standard.IndexSolver
import Vehicle.Data.Builtin.Standard.Normalise ()
import Vehicle.Data.Code.DSL
import Vehicle.Data.DSL
import Vehicle.Data.Tensor
import Vehicle.Data.Variable.Free.Context (MonadFreeContext (..))
import Vehicle.Libraries.Stdlib
import Prelude hiding (pi)

--------------------------------------------------------------------------------
-- Scoping
--------------------------------------------------------------------------------

instance DesugarableBuiltin Builtin where
  elabUnitLiteral p = D.Builtin p $ BuiltinConstructor UnitLiteral
  elabBoolLiteral p = D.Builtin p . BuiltinConstructor . BoolTensorLiteral . ZeroDimTensor
  elabNatLiteral p n = do
    let fromNat = D.Builtin p (TypeClassOp FromNatTC)
    D.normAppList fromNat $ fmap explicit [D.Builtin p $ BuiltinConstructor $ NatLiteral n]
  elabDecimalLiteral p r = do
    let fromRat = D.Builtin p (TypeClassOp FromRatTC)
    D.normAppList fromRat $ fmap explicit [D.Builtin p $ BuiltinConstructor $ RatTensorLiteral $ ZeroDimTensor r]

instance ScopableBuiltin Builtin where
  generateAuxiliaryRecordDefinitions = generateBuiltinAuxiliaryRecordDefinitions

--------------------------------------------------------------------------------
-- Typing
--------------------------------------------------------------------------------

-- See https://github.com/joelberkeley/spidr/blob/master/spidr/src/Tensor.idr for a dependent tensor type-system

instance TypableBuiltin Builtin where
  typeBuiltin = typeStandardBuiltin
  useDependentMetas _ = True
  isConstructor = isStandardConstructor
  isCastConstraint e = case e of
    Right (TypeClass c) -> c `elem` ([IsTensorType, HasNatLits, HasRatLits, HasVecLits] :: [TypeClass])
    _ -> False

-- | Return the type of the provided builtin.
isStandardConstructor :: Builtin -> Bool
isStandardConstructor = \case
  BuiltinConstructor {} -> True
  BuiltinFunction {} -> False
  BuiltinCast {} -> False
  TypeClassOp {} -> False
  TypeClass {} -> True
  BuiltinType {} -> True
  NatInDomainConstraint {} -> True
  DerivedFunction {} -> False

-- | Return the type of the provided builtin.
typeStandardBuiltin :: (MonadTypeChecker Builtin m) => Provenance -> Builtin -> m (Type Builtin)
typeStandardBuiltin p = \case
  DerivedFunction f -> getDeclType (Proxy @Builtin) (identifierOf f)
  BuiltinType s -> return $ fromDSL p $ typeOfBuiltinType s
  BuiltinConstructor c -> return $ fromDSL p $ typeOfBuiltinConstructor c
  BuiltinFunction f -> return $ fromDSL p $ typeOfBuiltinFunction f
  BuiltinCast c -> return $ fromDSL p $ typeOfBuiltinCast c
  TypeClassOp tcOp -> return $ fromDSL p $ typeOfTypeClassOp tcOp
  TypeClass tc -> return $ fromDSL p $ typeOfTypeClass tc
  NatInDomainConstraint {} -> return $ fromDSL p typeOfNatInDomainConstraint

typeOfTypeClass :: TypeClass -> DSLExpr Builtin
typeOfTypeClass tc = case tc of
  HasNatLits {} -> type0 ~> type0
  HasRatLits -> type0 ~> type0
  HasVecLits {} -> type0 ~> type0 ~> tNat ~> type0
  IsTensorType {} -> typeOfBuiltinType TensorType

typeOfTypeClassOp :: TypeClassOp -> DSLExpr Builtin
typeOfTypeClassOp b = case b of
  TensorTypeTC ->
    forAllExpl "t" type0 $ \t ->
      pi (Just "ds") Explicit Irrelevant tDims $ \ds ->
        isTensorType t ds ~~~> type0
  FromNatTC -> forAllTypes $ \t -> hasNatLits t ~~~> typeOfFromNat t
  FromRatTC -> forAllTypes $ \t -> hasRatLits t ~~~> typeOfFromRat t
  VecLiteralTC -> typeOfVectorLiteral

typeOfBuiltinCast :: BuiltinCast -> DSLExpr Builtin
typeOfBuiltinCast = \case
  FromNat dom -> case dom of
    FromNatToNat -> typeOfFromNat tNat
    FromNatToIndex -> forAllIrrelevantNat "n" $ \s -> typeOfFromNat (tIndex s)
    FromNatToRat -> typeOfFromNat (tRatTensor dimNil)
  FromRat dom -> case dom of
    FromRatToRat -> typeOfFromRat (tRatTensor dimNil)
  FromVectorToList -> typeOfFromVectorToList

typeOfFromVectorToList :: (HasStandardBuiltins builtin) => DSLExpr builtin
typeOfFromVectorToList =
  forAllTypes $ \t ->
    forAllDim Relevant $ \d ->
      typeOfVecLiteralCast (tList t) t d

typeOfNatInDomainConstraint :: (HasStandardBuiltins builtin) => DSLExpr builtin
typeOfNatInDomainConstraint = forAll "A" type0 $ \t -> tNat ~> t ~> type0

natInDomainConstraint :: DSLExpr Builtin -> DSLExpr Builtin -> DSLExpr Builtin
natInDomainConstraint n t = builtin NatInDomainConstraint @@ [n, t]

typeOfFromNat :: DSLExpr Builtin -> DSLExpr Builtin
typeOfFromNat t = forAllExpl "n" tNat $ \n -> natInDomainConstraint n t .~~~> t

typeOfFromRat :: DSLExpr Builtin -> DSLExpr Builtin
typeOfFromRat t = tRatTensor dimNil ~> t

typeOfVectorLiteral :: DSLExpr Builtin
typeOfVectorLiteral =
  forAll "tCont" type0 $ \tCont ->
    forAll "tElem" type0 $ \tElem ->
      forAllDim Relevant $ \d ->
        hasVecLits tCont tElem d
          ~~~> typeOfVecLiteralCast tCont tElem d

--------------------------------------------------------------------------------
-- Type system
--------------------------------------------------------------------------------

instance HasTypeSystem Builtin where
  convertFromStandardBuiltins = return
  restrictDeclType = restrictStandardDeclType
  restrictRecordAnnotatedAsTensor = restrictStandardRecordAnnotatedAsTensorType
  isAuxiliaryConstraint e = case e of
    App (Builtin _ NatInDomainConstraint) _ -> True
    _ -> False

  solveAuxiliaryInstanceConstraint = solveIndexConstraint
  addAuxiliaryInputOutputConstraints = return
  generateDefaultAuxiliaryConstraint = addNewStandardAuxiliaryConstraintUsingDefaults

restrictStandardDeclType ::
  forall m.
  (MonadTypeChecker Builtin m) =>
  RestrictedDecl ->
  DeclProvenance ->
  Type Builtin ->
  m (Type Builtin)
restrictStandardDeclType declSort (ident, p) typ = do
  env <- getFreeCtx (Proxy @Builtin)
  let tc = case declSort of
        RestrictedProperty -> FreeVar p validPropertyTypeIdent
        RestrictedParameter Inferable -> FreeVar p validInferableParameterTypeIdent
        RestrictedParameter NonInferable -> FreeVar p validNonInferableParameterTypeIdent
        RestrictedDataset -> FreeVar p validDatasetTypeIdent
        RestrictedNetwork -> FreeVar p validNetworkTypeIdent

  let expr = App tc [explicit typ]
  let origin = InstanceTypeRestrictionOrigin $ TypeRestrictionOrigin env (ident, provenanceOf typ) (Left declSort) typ
  _ <- createFreshInstanceConstraint False mempty p origin Irrelevant expr
  return typ

restrictStandardRecordAnnotatedAsTensorType ::
  forall m.
  (MonadTypeChecker Builtin m) =>
  DeclProvenance ->
  [RecordField Builtin] ->
  m ()
restrictStandardRecordAnnotatedAsTensorType (ident, p) fields = case fields of
  [] -> return ()
  (firstFieldName, firstFieldType) : restFields -> do
    env <- getFreeCtx (Proxy @Builtin)
    let expr = App (FreeVar p validTensorLikeTypeIdent) [explicit firstFieldType]
    let restrictionDetails = Right (FieldTypeIsAllowed firstFieldName)
    let origin = InstanceTypeRestrictionOrigin $ TypeRestrictionOrigin env (ident, p) restrictionDetails firstFieldType
    _ <- createFreshInstanceConstraint False mempty p origin Irrelevant expr

    traverse_ (checkRecordFieldTypesMatch (ident, p) (firstFieldName, firstFieldType)) restFields
    return ()

checkRecordFieldTypesMatch ::
  forall m.
  (MonadTypeChecker Builtin m) =>
  DeclProvenance ->
  RecordField Builtin ->
  RecordField Builtin ->
  m ()
checkRecordFieldTypesMatch (ident, p) (firstFieldName, firstFieldType) (currFieldName, currFieldType) = do
  env <- getFreeCtx (Proxy @Builtin)
  let restrictionDetails = Right (FieldTypesMatch firstFieldName currFieldName)
  let origin = InstanceTypeRestrictionOrigin $ TypeRestrictionOrigin env (ident, p) restrictionDetails firstFieldType
  _ <- createFreshUnificationConstraint p mempty (CheckingInstanceType origin) firstFieldType currFieldType
  return ()

-- | Tries to add new unification constraints using default values.
addNewStandardAuxiliaryConstraintUsingDefaults ::
  (MonadTypeChecker Builtin m) =>
  Proxy Builtin ->
  m Bool
addNewStandardAuxiliaryConstraintUsingDefaults proxy = do
  -- Calculate the set of candidate constraints
  auxiliaryConstraints <- getActiveAuxiliaryInstanceConstraints
  defaultableConstraints <- getDefaultableConstraints proxy (Foldable.toList auxiliaryConstraints)
  solveDefaultIndexConstraints defaultableConstraints
