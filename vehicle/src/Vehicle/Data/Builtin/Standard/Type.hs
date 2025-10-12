{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Data.Builtin.Standard.Type () where

import Data.Proxy (Proxy (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Bidirectional (createFreshUnificationConstraint)
import Vehicle.Compile.Type.Constraint.InstanceDefaultSolver
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Monad.Class
import Vehicle.Compile.Type.System
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Builtin.Interface.Type
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Builtin.Standard.IndexSolver
import Vehicle.Data.Builtin.Standard.Normalise ()
import Vehicle.Data.Code.DSL
import Vehicle.Data.DSL
import Vehicle.Data.Variable.Free.Context (MonadFreeContext, getDeclType, getFreeEnv)
import Prelude hiding (iterate, pi)

--------------------------------------------------------------------------------
-- Typing
--------------------------------------------------------------------------------

-- See https://github.com/joelberkeley/spidr/blob/master/spidr/src/Tensor.idr for a dependent tensor type-system

instance TypableBuiltin Builtin where
  typeBuiltin = typeStandardBuiltin
  useDependentMetas _ = True
  isConstructor = isStandardConstructor
  isCastConstraint e = case e of
    TypeClass c -> c `elem` ([IsTensorType, HasNatLits, HasRatLits, HasVecLits] :: [TypeClass])
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
typeStandardBuiltin :: (MonadFreeContext Builtin m) => Provenance -> Builtin -> m (Type Builtin)
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
  HasCompare {} -> type0 ~> type0 ~> type0
  HasQuantifier {} -> type0 ~> type0 ~> type0
  HasAdd -> type0 ~> type0 ~> type0 ~> type0
  HasSub -> type0 ~> type0 ~> type0 ~> type0
  HasMul -> type0 ~> type0 ~> type0 ~> type0
  HasDiv -> type0 ~> type0 ~> type0 ~> type0
  HasNeg -> type0 ~> type0 ~> type0
  HasAt -> type0 ~> type0 ~> type0 ~> type0
  HasForeach -> type0 ~> type0 ~> type0 ~> type0
  HasMap -> (type0 ~> type0) ~> type0
  HasFold -> (type0 ~> type0) ~> type0
  HasQuantifierIn {} -> type0 ~> type0 ~> type0
  HasNatLits {} -> type0 ~> type0
  HasRatLits -> type0 ~> type0
  HasVecLits {} -> tNat ~> (type0 ~> type0) ~> type0
  ValidPropertyType -> type0 ~> type0
  ValidParameterType {} -> type0 ~> type0
  ValidNetworkType -> type0 ~> type0
  ValidNetworkTensorType -> type0 ~> type0
  ValidDatasetType -> type0 ~> type0
  ValidDatasetListElementType -> type0 ~> type0
  ValidDatasetTensorElementType -> type0 ~> type0
  IsTensorType {} -> typeOfBuiltinType TensorType
  ValidTensorLikeType -> type0 ~> type0

typeOfTypeClassOp :: TypeClassOp -> DSLExpr Builtin
typeOfTypeClassOp b = case b of
  TensorTypeTC ->
    forAllExpl "t" type0 $ \t ->
      pi (Just "ds") Explicit Irrelevant tDims $ \ds ->
        isTensorType t ds ~~~> type0
  FromNatTC -> forAllTypes $ \t -> hasNatLits t ~~~> typeOfFromNat t
  FromRatTC -> forAllTypes $ \t -> hasRatLits t ~~~> typeOfFromRat t
  VecLiteralTC -> typeOfVectorLiteral
  NegTC -> typeOfTCOp1 hasNeg
  AddTC -> typeOfTCOp2 hasAdd
  SubTC -> typeOfTCOp2 hasSub
  MulTC -> typeOfTCOp2 hasMul
  DivTC -> typeOfTCOp2 hasDiv
  CompareTC op -> typeOfTCComparisonOp $ hasCompare op
  AtTC -> typeOfTCOp2 hasAt
  ForeachTC ->
    forAll "A" type0 $ \t1 ->
      forAll "B" type0 $ \t2 ->
        forAll "C" type0 $ \t3 ->
          hasForeach t1 t2 t3 ~~~> typeOfForeach t1 t2 t3
  MapTC -> forAll "f" (type0 ~> type0) $ \f -> hasMap f ~~~> typeOfMap f
  FoldTC -> forAll "f" (type0 ~> type0) $ \f -> hasFold f ~~~> typeOfFold f
  QuantifierTC q ->
    forAll "A" (type0 ~> type0) $ \t ->
      hasQuantifier q t ~~~> typeOfQuantifier t

typeOfBuiltinCast :: BuiltinCast -> DSLExpr Builtin
typeOfBuiltinCast = \case
  FromNat dom -> case dom of
    FromNatToNat -> typeOfFromNat tNat
    FromNatToIndex -> forAllIrrelevantNat "n" $ \s -> typeOfFromNat (tIndex s)
    FromNatToRat -> typeOfFromNat (tRatTensor dimNil)
  FromRat dom -> case dom of
    FromRatToRat -> typeOfFromRat (tRatTensor dimNil)
  FromVectorToList -> typeOfFromVectorToList

typeOfTCComparisonOp ::
  (BuiltinHasStandardTypes builtin) =>
  (DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin) ->
  DSLExpr builtin
typeOfTCComparisonOp constraint =
  forAllTypeTriples $ \t1 t2 t3 ->
    constraint t1 t2 t3
      ~~~> t1
      ~> t2
      ~> t3

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
  env <- getFreeEnv
  let tc = case declSort of
        RestrictedProperty -> ValidPropertyType
        RestrictedParameter s -> ValidParameterType s
        RestrictedDataset -> ValidDatasetType
        RestrictedNetwork -> ValidNetworkType

  let expr = BuiltinExpr p (TypeClass tc) [explicit typ]
  let origin = InstanceTypeRestrictionOrigin $ TypeRestrictionOrigin env (ident, provenanceOf typ) (Left declSort) typ
  _ <- createFreshInstanceConstraint False mempty p origin Irrelevant expr
  return typ

restrictStandardRecordAnnotatedAsTensorType ::
  forall m.
  (MonadTypeChecker Builtin m) =>
  DeclProvenance ->
  [RecordField (Type Builtin)] ->
  m ()
restrictStandardRecordAnnotatedAsTensorType (ident, p) fields = case fields of
  [] -> return ()
  (firstFieldName, firstFieldType) : restFields -> do
    env <- getFreeEnv
    let expr = BuiltinExpr p (TypeClass ValidTensorLikeType) [explicit firstFieldType]
    let restrictionDetails = Right (FieldTypeIsAllowed firstFieldName)
    let origin = InstanceTypeRestrictionOrigin $ TypeRestrictionOrigin env (ident, p) restrictionDetails firstFieldType
    _ <- createFreshInstanceConstraint False mempty p origin Irrelevant expr

    _ <- traverse (checkRecordFieldTypesMatch (ident, p) (firstFieldName, firstFieldType)) restFields
    return ()

checkRecordFieldTypesMatch ::
  forall m.
  (MonadTypeChecker Builtin m) =>
  DeclProvenance ->
  RecordField (Type Builtin) ->
  RecordField (Type Builtin) ->
  m ()
checkRecordFieldTypesMatch (ident, p) (firstFieldName, firstFieldType) (currFieldName, currFieldType) = do
  env <- getFreeEnv
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
  defaultableConstraints <- getDefaultableConstraints proxy auxiliaryConstraints
  solveDefaultIndexConstraints defaultableConstraints
