{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Data.Builtin.Decidability.Type
  ( typeDecidabilityBuiltin,
  )
where

import Data.Proxy (Proxy (..))
import Vehicle.Compile.Context.Free (getDeclType, getFreeEnv)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.System
import Vehicle.Data.Builtin.Decidability
import Vehicle.Data.Builtin.Interface.Type
import Vehicle.Data.Builtin.Standard (BuiltinConstructor (..), BuiltinFunction (..), BuiltinType (..), DerivedFunction (..))
import Vehicle.Data.Code.DSL
import Vehicle.Data.DSL
import Vehicle.Syntax.Builtin (Builtin (..))
import Prelude hiding (iterate, pi)

--------------------------------------------------------------------------------
-- Typing
--------------------------------------------------------------------------------

instance TypableBuiltin DecidabilityBuiltin where
  typeBuiltin p b = return $ fromDSL p $ typeDecidabilityBuiltin b
  useDependentMetas _ = True
  isConstructor = isDecidabilityConstructor

  isCastConstraint e = case e of
    DecidabilityBuiltinTypeClass (HasTensorTypeClassField FieldFromBoolTensorLiteral) -> True
    _ -> False

isDecidabilityConstructor :: DecidabilityBuiltin -> Bool
isDecidabilityConstructor = \case
  StandardBuiltinType {} -> False
  StandardBuiltinFunction {} -> False
  StandardBuiltinConstructor {} -> True
  StandardBuiltinDerivedFunction {} -> True
  DecidabilityBuiltinTypeClass {} -> False
  DecidabilityBuiltinTypeClassOp {} -> False
  DecidabilityBuiltinFunction {} -> False

typeDecidabilityBuiltin :: DecidabilityBuiltin -> DSLExpr DecidabilityBuiltin
typeDecidabilityBuiltin = \case
  StandardBuiltinType t -> typeOfBuiltinType t
  StandardBuiltinConstructor c -> typeOfBuiltinConstructor c
  StandardBuiltinFunction f -> case f of
    QuantifyRatTensor {} -> forAllDims $ \_dims -> forAllTypes $ \t -> (t ~> tProp) ~> tProp
    _ -> typeOfBuiltinFunction f
  StandardBuiltinDerivedFunction f -> typeOfDerivedFunction f
  DecidabilityBuiltinTypeClass t -> typeDecidableTypeClass t
  DecidabilityBuiltinTypeClassOp t -> typeDecidableTypeClassOp t
  DecidabilityBuiltinFunction f -> typeDecidableFunction f

typeOfDerivedFunction :: DerivedFunction -> DSLExpr DecidabilityBuiltin
typeOfDerivedFunction = \case
  TypeAnn -> forAllExpl "t" type0 $ \t -> t ~> t
  QuantifyIndex {} -> forAllDim Relevant $ \d -> (tIndex d ~> tBool) ~> tBool
  QuantifyInList {} -> forAllTypes $ \t -> (t ~> tBool) ~> tList t ~> tBool
  CompareRatTensorReduced {} ->
    forAllDim Irrelevant $ \d ->
      forAllDims $ \ds ->
        tRatTensor (dimCons d ds) ~> tRatTensor (dimCons d ds) ~> tBoolTensor dimNil

typeDecidableTypeClass :: DecidabilityBuiltinTypeClass -> DSLExpr DecidabilityBuiltin
typeDecidableTypeClass = \case
  IsTensorType -> type0
  IsVectorType -> type0
  HasTensorTypeClassField _f -> absTensorType ~> type0
  HasVectorTypeClassField _f -> absVectorType ~> type0
  ValidPropertyType -> type0 ~> type0
  ValidNetworkType -> type0 ~> type0

typeDecidableTypeClassOp :: DecidabilityBuiltinTypeClassOp -> DSLExpr DecidabilityBuiltin
typeDecidableTypeClassOp = \case
  TensorTypeTC -> isTensorType ~~~> absTensorType
  VectorTypeTC -> isVectorType ~~~> absVectorType
  VectorTypeClassFieldTC field ->
    forAll "vector" absVectorType $ \vectorSol ->
      let vector e d = vectorSol @@ [e] .@@ [d]
       in builtinDecidableTypeClass (HasVectorTypeClassField field)
            @@ [vectorSol]
            ~~~> case field of
              FieldFromVectorLiteral ->
                forAllTypes $ \tElem ->
                  forAllDim Irrelevant $ \d ->
                    tVector tElem d ~> vector tElem d
              FieldForeachVector ->
                forAllTypes $ \tElem ->
                  forAllDim Relevant $ \d ->
                    (tIndex d ~> tElem) ~> vector tElem d
              FieldAtVector ->
                forAllTypes $ \tElem ->
                  forAllDim Relevant $ \d ->
                    vector tElem d ~> (tIndex d ~> tElem)
  TensorTypeClassFieldTC field ->
    forAll "tensor" absTensorType $ \tensorSol ->
      let tensor e ds = tensorSol @@ [e] .@@ [ds]
       in builtinDecidableTypeClass (HasTensorTypeClassField field)
            @@ [tensorSol]
            ~~~> case field of
              FieldFromBoolTensorLiteral -> forAllDims $ \ds -> tBoolTensor ds ~> tensor tBool ds
              FieldNot -> forAllDims $ \ds -> typeOp1 (tensor tBool ds)
              FieldAnd -> forAllDims $ \ds -> typeOp2 (tensor tBool ds)
              FieldOr -> forAllDims $ \ds -> typeOp2 (tensor tBool ds)
              FieldImplies -> forAllDims $ \ds -> typeOp2 (tensor tBool ds)
              FieldReduceAnd -> forAllDims $ \ds -> tensor tBool dimNil ~> tensor tBool ds ~> tensor tBool dimNil
              FieldReduceOr -> forAllDims $ \ds -> tensor tBool dimNil ~> tensor tBool ds ~> tensor tBool dimNil
              FieldForeachTensor -> forAllTypes $ \tElem -> forAllDim Relevant $ \d -> forAllDims $ \ds -> (tIndex d ~> tensor tElem ds) ~> tensor tElem (dimCons d ds)
              FieldAtTensor -> forAllTypes $ \tElem -> forAllDim Relevant $ \d -> forAllDims $ \ds -> tensor tElem (dimCons d ds) ~> (tIndex d ~> tensor tElem ds)
              FieldCompareIndex {} -> typeOfCompareIndex (tensor tBool dimNil)
              FieldCompareNat {} -> typeOfCompareNat (tensor tBool dimNil)
              FieldCompareRatTensorPointwise {} -> forAllDims $ \ds -> tTensor tRat ds ~> tTensor tRat ds ~> tensor tBool ds
              FieldCompareRatTensorReduced {} -> forAllDim Irrelevant $ \d -> forAllDims $ \ds -> tTensor tRat (dimCons d ds) ~> tTensor tRat (dimCons d ds) ~> tensor tBool dimNil
              FieldQuantifyInList {} -> typeOfQuantifyInList tensorSol
              FieldQuantifyIndex {} -> typeOfQuantifyIndex tensorSol

absTensorType :: DSLExpr DecidabilityBuiltin
absTensorType = type0 ~> tDims .~> type0

absVectorType :: DSLExpr DecidabilityBuiltin
absVectorType = type0 ~> tDim .~> type0

typeDecidableFunction :: DecidabilityBuiltinFunction -> DSLExpr DecidabilityBuiltin
typeDecidableFunction = \case
  PropType -> type0
  BoolTensorToProp -> typeOfCastBoolTensor tProp
  BoolVectorToProp -> typeOfCastVector tProp tProp
  PropTrue -> tProp
  PropFalse -> tProp
  PropNot -> typeOp1 tProp
  PropAnd -> typeOp2 tProp
  PropOr -> typeOp2 tProp
  PropImplies -> typeOp2 tProp
  PropCompareIndex _op -> typeOfCompareIndex tProp
  PropCompareNat _op -> typeOfCompareNat tProp
  PropCompareRatTensorPointwise _op -> forAllDims $ \ds -> tTensor tRat ds ~> tTensor tRat ds ~> tProp
  PropQuantifyIndex _q -> typeOfQuantifyIndex propTensor
  PropQuantifyInList _q -> typeOfQuantifyInList propTensor
  PropNaryProduct -> developerError "PropNaryProduct not supported"
  PropNaryProductAt -> developerError "PropNaryProduct not supported"
  PropNaryProductForeach -> developerError "PropNaryProduct not supported"

typeOfCompareIndex :: DSLExpr DecidabilityBuiltin -> DSLExpr DecidabilityBuiltin
typeOfCompareIndex tRes =
  forAllIrrelevantNat "n1" $ \n1 ->
    forAllIrrelevantNat "n2" $ \n2 ->
      tIndex n1 ~> tIndex n2 ~> tRes

typeOfCompareNat :: DSLExpr DecidabilityBuiltin -> DSLExpr DecidabilityBuiltin
typeOfCompareNat tRes = tNat ~> tNat ~> tRes

typeOfCastBoolTensor :: DSLExpr DecidabilityBuiltin -> DSLExpr DecidabilityBuiltin
typeOfCastBoolTensor t = forAllDims $ \dims -> tBoolTensor dims ~> t .@@ [dims]

typeOfCastVector :: DSLExpr DecidabilityBuiltin -> DSLExpr DecidabilityBuiltin -> DSLExpr DecidabilityBuiltin
typeOfCastVector tElem tRes = forAllDim Irrelevant $ \d -> tVector tElem d ~> tRes .@@ [d]

typeOfQuantifyIndex :: DSLExpr DecidabilityBuiltin -> DSLExpr DecidabilityBuiltin
typeOfQuantifyIndex t = forAllDim Relevant $ \d -> (tIndex d ~> t @@ [tBool] .@@ [dimNil]) ~> t @@ [tBool] .@@ [dimNil]

typeOfQuantifyInList :: DSLExpr DecidabilityBuiltin -> DSLExpr DecidabilityBuiltin
typeOfQuantifyInList t = forAllTypes $ \tElem -> (tElem ~> t @@ [tBool] .@@ [dimNil]) ~> tList tElem ~> t @@ [tBool] .@@ [dimNil]

typeOp1 :: DSLExpr DecidabilityBuiltin -> DSLExpr DecidabilityBuiltin
typeOp1 t = t ~> t

typeOp2 :: DSLExpr DecidabilityBuiltin -> DSLExpr DecidabilityBuiltin
typeOp2 t = t ~> t ~> t

--------------------------------------------------------------------------------
-- TypeSystem
--------------------------------------------------------------------------------

instance HasTypeSystem DecidabilityBuiltin where
  convertFromStandardBuiltins x = traverseFreeVarsM (const id) convertToDecidabilityFreeVars =<< traverseBuiltinsM convertToDecidabilityBuiltins x
  restrictDeclType = restrictDecidabilityDeclType
  restrictRecordAnnotatedAsTensor = restrictDecidabilityRecordAnnotatedAsTensor
  isAuxiliaryConstraint _ = False

  solveAuxiliaryInstanceConstraint _ = return ()
  addAuxiliaryInputOutputConstraints = return
  generateDefaultAuxiliaryConstraint _ = return False

convertToDecidabilityFreeVars ::
  forall m.
  (MonadTypeChecker DecidabilityBuiltin m) =>
  FreeVarUpdate m DecidabilityBuiltin
convertToDecidabilityFreeVars f p ident args = do
  declType <- getDeclType (Proxy @DecidabilityBuiltin) ident
  args' <- traverseArgs f args
  finalArgs <- insertNewArgs args' declType
  return $ normAppList (FreeVar p ident) finalArgs
  where
    insertNewArgs :: [Arg DecidabilityBuiltin] -> Type DecidabilityBuiltin -> m [Arg DecidabilityBuiltin]
    insertNewArgs as = \case
      Pi _ binder result -> do
        if wasInsertedByCompiler binder && isImplicit binder
          then (argFromBinder binder (Hole p "_") :) <$> insertNewArgs as result
          else return as
      _ -> return as

convertToDecidabilityBuiltins ::
  forall m.
  (MonadTypeChecker DecidabilityBuiltin m) =>
  BuiltinUpdate m Builtin DecidabilityBuiltin
convertToDecidabilityBuiltins p b args = return $
  case b of
    BuiltinFunction f -> do
      case f of
        -- Convert to type-classes for resolution
        Not -> insertTypeArgumentAndConvertTo (TensorTypeClassFieldTC FieldNot)
        And -> insertTypeArgumentAndConvertTo (TensorTypeClassFieldTC FieldAnd)
        Or -> insertTypeArgumentAndConvertTo (TensorTypeClassFieldTC FieldOr)
        Implies -> insertTypeArgumentAndConvertTo (TensorTypeClassFieldTC FieldImplies)
        CompareRatTensorPointwise op -> insertTypeArgumentAndConvertTo (TensorTypeClassFieldTC $ FieldCompareRatTensorPointwise op)
        ForeachTensor -> insertTypeArgumentAndConvertTo (TensorTypeClassFieldTC FieldForeachTensor)
        ReduceAndTensor -> insertTypeArgumentAndConvertTo (TensorTypeClassFieldTC FieldReduceAnd)
        ReduceOrTensor -> insertTypeArgumentAndConvertTo (TensorTypeClassFieldTC FieldReduceOr)
        ForeachVector -> insertTypeArgumentAndConvertTo (VectorTypeClassFieldTC FieldForeachVector)
        AtVector -> insertTypeArgumentAndConvertTo (VectorTypeClassFieldTC FieldAtVector)
        AtTensor -> insertTypeArgumentAndConvertTo (TensorTypeClassFieldTC FieldAtTensor)
        CompareIndex op -> insertTypeArgumentAndConvertTo (TensorTypeClassFieldTC $ FieldCompareIndex op)
        CompareNat op -> insertTypeArgumentAndConvertTo (TensorTypeClassFieldTC $ FieldCompareNat op)
        -- Nothing needs to change
        QuantifyRatTensor {} -> sameFunction f
        If -> sameFunction f
        Neg {} -> sameFunction f
        Add {} -> sameFunction f
        Sub {} -> sameFunction f
        Mul {} -> sameFunction f
        Div {} -> sameFunction f
        Min {} -> sameFunction f
        Max {} -> sameFunction f
        PowRat -> sameFunction f
        ReduceAddRatTensor -> sameFunction f
        ReduceMulRatTensor -> sameFunction f
        ReduceMinRatTensor -> sameFunction f
        ReduceMaxRatTensor -> sameFunction f
        FoldList -> sameFunction f
        MapList -> sameFunction f
        Iterate -> sameFunction f
        StackTensor -> sameFunction f
        ConstTensor -> sameFunction f
    BuiltinConstructor c -> do
      let original = normAppList (Builtin p (StandardBuiltinConstructor c)) args
      case c of
        BoolTensorLiteral {} -> castWith (TensorTypeClassFieldTC FieldFromBoolTensorLiteral) original
        VectorLiteral {} -> castWith (VectorTypeClassFieldTC FieldFromVectorLiteral) original
        _ -> original
    BuiltinType s -> do
      let b' = case s of
            TensorType -> DecidabilityBuiltinTypeClassOp TensorTypeTC
            VectorType -> DecidabilityBuiltinTypeClassOp VectorTypeTC
            _ -> StandardBuiltinType s
      normAppList (Builtin p b') args
    DerivedFunction f -> case f of
      TypeAnn -> sameDerivedFunction f
      QuantifyIndex q -> insertTypeArgumentAndConvertTo (TensorTypeClassFieldTC $ FieldQuantifyIndex q)
      QuantifyInList q -> insertTypeArgumentAndConvertTo (TensorTypeClassFieldTC $ FieldQuantifyInList q)
      CompareRatTensorReduced op -> insertTypeArgumentAndConvertTo (TensorTypeClassFieldTC $ FieldCompareRatTensorReduced op)
    _ -> monomorphisationError b args
  where
    -- Nothing changes
    sameDerivedFunction f = normAppList (Builtin p (StandardBuiltinDerivedFunction f)) args
    sameFunction f = normAppList (Builtin p (StandardBuiltinFunction f)) args

    -- Apply a cast
    castWith f original = normAppList (Builtin p $ DecidabilityBuiltinTypeClassOp f) [explicit original]

    insertTypeArgumentAndConvertTo f = do
      let newArgs = implicit (Hole p "_") : args
      normAppList (Builtin p $ DecidabilityBuiltinTypeClassOp f) newArgs

restrictDecidabilityDeclType ::
  forall m.
  (MonadTypeChecker DecidabilityBuiltin m) =>
  RestrictedDecl ->
  DeclProvenance ->
  Type DecidabilityBuiltin ->
  m (Type DecidabilityBuiltin)
restrictDecidabilityDeclType declSort (ident, p) declType = do
  let maybeTypeClass = case declSort of
        RestrictedProperty -> Just ValidPropertyType
        RestrictedNetwork -> Just ValidNetworkType
        _ -> Nothing

  case maybeTypeClass of
    Nothing -> return ()
    Just tc -> do
      freeEnv <- getFreeEnv
      let expr = BuiltinExpr p (DecidabilityBuiltinTypeClass tc) [explicit declType]
      let origin = InstanceTypeRestrictionOrigin $ TypeRestrictionOrigin freeEnv (ident, provenanceOf declType) (Left declSort) declType
      _ <- createFreshInstanceConstraint False mempty p origin Irrelevant expr
      return ()

  return declType

restrictDecidabilityRecordAnnotatedAsTensor ::
  forall m.
  (MonadTypeChecker DecidabilityBuiltin m) =>
  DeclProvenance ->
  [RecordField (Type DecidabilityBuiltin)] ->
  m ()
restrictDecidabilityRecordAnnotatedAsTensor (_ident, _p) _fields =
  return ()
