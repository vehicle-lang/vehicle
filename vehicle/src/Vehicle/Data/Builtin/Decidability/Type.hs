{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Data.Builtin.Decidability.Type
  ( typeDecidabilityBuiltin,
  )
where

import Data.Proxy (Proxy (..))
import Vehicle.Compile.Context.Free (getDeclType, getFreeEnv)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Bidirectional (createFreshUnificationConstraint)
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
  CompareRatTensorReduced {} -> forAllDims $ \dims -> tRatTensor dims ~> tRatTensor dims ~> tBoolTensor dimNil

typeDecidableTypeClass :: DecidabilityBuiltinTypeClass -> DSLExpr DecidabilityBuiltin
typeDecidableTypeClass = \case
  IsBoolType -> type0 ~> type0
  IsTensorType -> type0 ~> type0
  IsVectorType -> type0 ~> type0
  HasTensorTypeClassField _f -> (tDim ~> type0) ~> type0
  HasVectorTypeClassField _f -> (tDim ~> type0) ~> type0

typeDecidableTypeClassOp :: DecidabilityBuiltinTypeClassOp -> DSLExpr DecidabilityBuiltin
typeDecidableTypeClassOp = \case
  BoolTypeTC -> constraint IsBoolType (const type0)
  TensorTypeTC ->
    forAllExpl "t" type0 $ \t ->
      isTensorType t
        ~~~> tDims
        .~> type0
  VectorTypeTC ->
    forAllExpl "t" type0 $ \t ->
      isVectorType t
        ~~~> tDim
        .~> type0
  VectorTypeClassFieldTC field ->
    forAll "t" (type0 ~> tDim ~> type0) $ \t ->
      builtinDecidableTypeClass (HasVectorTypeClassField field)
        @@ [t]
        ~~~> case field of
          FieldFromVectorLiteral ->
            forAllTypes $ \tElem ->
              forAllDim Irrelevant $ \d ->
                tVector tElem d ~> t @@ [tElem] .@@ [d]
          FieldForeachVector ->
            forAllTypes $ \tElem ->
              forAllDim Relevant $ \d ->
                (tIndex d ~> tElem) ~> t @@ [tElem] .@@ [d]
          FieldAtVector ->
            forAllTypes $ \tElem ->
              forAllDim Relevant $ \d ->
                t @@ [tElem] .@@ [d] ~> (tIndex d ~> tElem)
  TensorTypeClassFieldTC field ->
    forAll "t" (tDims ~> type0) $ \t ->
      builtinDecidableTypeClass (HasTensorTypeClassField field)
        @@ [t]
        ~~~> case field of
          FieldFromBoolTensorLiteral -> forAllDims $ \ds -> tBoolTensor ds ~> t .@@ [ds]
          FieldNot -> forAllDims $ \ds -> typeOp1 (t .@@ [ds])
          FieldAnd -> forAllDims $ \ds -> typeOp2 (t .@@ [ds])
          FieldOr -> forAllDims $ \ds -> typeOp2 (t .@@ [ds])
          FieldImplies -> forAllDims $ \ds -> typeOp2 (t .@@ [ds])
          FieldReduceAnd -> forAllDims $ \ds -> t .@@ [dimNil] ~> t .@@ [ds] ~> t .@@ [dimNil]
          FieldReduceOr -> forAllDims $ \ds -> t .@@ [dimNil] ~> t .@@ [ds] ~> t .@@ [dimNil]
          FieldForeachTensor -> forAllDim Relevant $ \d -> forAllDims $ \ds -> (tIndex d ~> t .@@ [ds]) ~> t .@@ [d, ds]
          FieldAtTensor -> forAllDim Relevant $ \d -> forAllDims $ \ds -> t .@@ [d, ds] ~> (tIndex d ~> t .@@ [ds])
          FieldCompareIndex {} -> typeOfCompareIndex (t .@@ [dimNil])
          FieldCompareNat {} -> typeOfCompareNat (t .@@ [dimNil])
          FieldCompareRatTensorPointwise {} -> forAllDims $ \ds -> tTensor tRat ds ~> tTensor tRat ds ~> t .@@ [ds]
          FieldCompareRatTensorReduced {} -> typeOfCompareRatTensorReduced (t .@@ [dimNil])
          FieldQuantifyInList {} -> typeOfQuantifyInList t
          FieldQuantifyIndex {} -> typeOfQuantifyIndex t

constraint :: DecidabilityBuiltinTypeClass -> (DSLExpr DecidabilityBuiltin -> DSLExpr DecidabilityBuiltin) -> DSLExpr DecidabilityBuiltin
constraint c f =
  forAllTypes $ \t ->
    builtinDecidableTypeClass c @@ [t] ~~~> f t

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
  PropCompareRatTensorPointwise _op -> typeOfCompareRatTensorPointwise propIgnoreDims tProp
  PropQuantifyIndex _q -> typeOfQuantifyIndex tProp
  PropQuantifyInList _q -> typeOfQuantifyInList tProp

typeOfCompareIndex :: DSLExpr DecidabilityBuiltin -> DSLExpr DecidabilityBuiltin
typeOfCompareIndex tRes =
  forAllIrrelevantNat "n1" $ \n1 ->
    forAllIrrelevantNat "n2" $ \n2 ->
      tIndex n1 ~> tIndex n2 ~> tRes

typeOfCompareNat :: DSLExpr DecidabilityBuiltin -> DSLExpr DecidabilityBuiltin
typeOfCompareNat tRes = tNat ~> tNat ~> tRes

typeOfCompareRatTensorPointwise :: DSLExpr DecidabilityBuiltin -> DSLExpr DecidabilityBuiltin -> DSLExpr DecidabilityBuiltin
typeOfCompareRatTensorPointwise tRes dims = tTensor tRat dims ~> tTensor tRat dims ~> tRes .@@ [dims]

typeOfCompareRatTensorReduced :: DSLExpr DecidabilityBuiltin -> DSLExpr DecidabilityBuiltin
typeOfCompareRatTensorReduced t = forAllDims $ \dims -> tTensor tRat dims ~> tTensor tRat dims ~> t

typeOfCastBoolTensor :: DSLExpr DecidabilityBuiltin -> DSLExpr DecidabilityBuiltin
typeOfCastBoolTensor t = forAllDims $ \dims -> tBoolTensor dims ~> t .@@ [dims]

typeOfCastVector :: DSLExpr DecidabilityBuiltin -> DSLExpr DecidabilityBuiltin -> DSLExpr DecidabilityBuiltin
typeOfCastVector tElem tRes = forAllDim Irrelevant $ \d -> tVector tElem d ~> tRes .@@ [d]

typeOfQuantifyIndex :: DSLExpr DecidabilityBuiltin -> DSLExpr DecidabilityBuiltin
typeOfQuantifyIndex t = forAllDim Relevant $ \d -> (tIndex d ~> t) ~> t

typeOfQuantifyInList :: DSLExpr DecidabilityBuiltin -> DSLExpr DecidabilityBuiltin
typeOfQuantifyInList t = forAllTypes $ \tElem -> (tElem ~> t) ~> tList tElem ~> t

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
convertToDecidabilityBuiltins p b args =
  case b of
    BuiltinFunction f -> do
      case f of
        -- Convert to type-classes for resolution
        Not -> convertTo (TensorTypeClassFieldTC FieldNot)
        And -> convertTo (TensorTypeClassFieldTC FieldAnd)
        Or -> convertTo (TensorTypeClassFieldTC FieldOr)
        Implies -> convertTo (TensorTypeClassFieldTC FieldImplies)
        CompareRatTensorPointwise op -> convertTo (TensorTypeClassFieldTC $ FieldCompareRatTensorPointwise op)
        ForeachTensor -> convertTo (TensorTypeClassFieldTC FieldForeachTensor)
        ReduceAndTensor -> convertToAndAddHoles (TensorTypeClassFieldTC FieldReduceAnd) 1
        ReduceOrTensor -> convertToAndAddHoles (TensorTypeClassFieldTC FieldReduceOr) 1
        CompareIndex op -> convertToAndAddHoles (TensorTypeClassFieldTC $ FieldCompareIndex op) 1
        CompareNat op -> convertTo (TensorTypeClassFieldTC $ FieldCompareNat op)
        ForeachVector -> convertTo (VectorTypeClassFieldTC FieldForeachVector)
        AtVector -> convertTo (VectorTypeClassFieldTC FieldAtVector)
        AtTensor -> convertTo (TensorTypeClassFieldTC FieldAtTensor)
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
        BoolTensorLiteral {} -> return $ castWith (TensorTypeClassFieldTC FieldFromBoolTensorLiteral) original
        VectorLiteral {} -> return $ castWith (VectorTypeClassFieldTC FieldFromVectorLiteral) original
        _ -> return original
    BuiltinType s -> do
      let b' = case s of
            BoolType -> DecidabilityBuiltinTypeClassOp BoolTypeTC
            TensorType -> DecidabilityBuiltinTypeClassOp TensorTypeTC
            VectorType -> DecidabilityBuiltinTypeClassOp VectorTypeTC
            _ -> StandardBuiltinType s
      return $ normAppList (Builtin p b') args
    DerivedFunction f -> case f of
      TypeAnn -> sameDerivedFunction f
      QuantifyIndex q -> convertToAndAddHoles (TensorTypeClassFieldTC $ FieldQuantifyIndex q) 1
      QuantifyInList q -> convertToAndAddHoles (TensorTypeClassFieldTC $ FieldQuantifyInList q) 1
      CompareRatTensorReduced op -> convertToAndAddHoles (TensorTypeClassFieldTC $ FieldCompareRatTensorReduced op) 1
    _ -> monomorphisationError b args
  where
    -- Nothing changes
    sameDerivedFunction f = return $ normAppList (Builtin p (StandardBuiltinDerivedFunction f)) args
    sameFunction f = return $ normAppList (Builtin p (StandardBuiltinFunction f)) args

    -- Apply a cast
    castWith f original = normAppList (Builtin p $ DecidabilityBuiltinTypeClassOp f) [explicit original]

    convertToAndAddHoles t numberOfHoles = do
      let holeArgs = replicate numberOfHoles (implicit (Hole p "_"))
      return $ normAppList (Builtin p (DecidabilityBuiltinTypeClassOp t)) (holeArgs <> args)
    convertTo t = convertToAndAddHoles t 0

restrictDecidabilityDeclType ::
  forall m.
  (MonadTypeChecker DecidabilityBuiltin m) =>
  RestrictedDecl ->
  DeclProvenance ->
  Type DecidabilityBuiltin ->
  m (Type DecidabilityBuiltin)
restrictDecidabilityDeclType rDecl declProv@(_, p) declType = do
  freeEnv <- getFreeEnv
  let origin = InstanceTypeRestrictionOrigin $ TypeRestrictionOrigin freeEnv declProv rDecl declType
  case rDecl of
    RestrictedProperty -> do
      let desiredType = Builtin mempty (DecidabilityBuiltinFunction PropType)
      createFreshUnificationConstraint p mempty (CheckingInstanceType origin) desiredType declType
      return declType
    _ -> return declType
