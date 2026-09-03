{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Data.Builtin.Loss.Type
  ( typeLossBuiltin,
  )
where

import Data.Proxy (Proxy (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Monad.Class (getDeclType, prependMissingFreeVarImplicitArgs)
import Vehicle.Compile.Type.System
import Vehicle.Data.Builtin.Interface.Type
import Vehicle.Data.Builtin.Loss
import Vehicle.Data.Builtin.Standard
  ( Builtin (..),
    BuiltinConstructor (..),
    BuiltinFunction (..),
    BuiltinType (..),
    DerivedFunction (..),
  )
import Vehicle.Data.Code.DSL
import Vehicle.Data.DSL
import Vehicle.Data.DifferentiableLogic (TensorDifferentiableLogicField (..))
import Vehicle.Data.Variable.Free.Context (MonadFreeContext (..))
import Vehicle.Libraries.StandardLibrary (differentiableTensorLogicIdent)
import Prelude hiding (iterate, pi)

--------------------------------------------------------------------------------
-- Typing
--------------------------------------------------------------------------------

instance TypableBuiltin (LossBuiltin 'Train) where
  typeBuiltin = typeLossBuiltin Train
  useDependentMetas _ = True
  isConstructor = isLossConstructor
  isCastConstraint = isLossCastConstraint

instance TypableBuiltin (LossBuiltin 'Search) where
  typeBuiltin = typeLossBuiltin Search
  useDependentMetas _ = True
  isConstructor = isLossConstructor
  isCastConstraint = isLossCastConstraint

isLossCastConstraint :: InstanceHead (LossBuiltin mode) -> Bool
isLossCastConstraint e = case e of
  Right (LossBuiltinTypeClass HasBoolLiterals) -> True
  _ -> False

isLossConstructor :: LossBuiltin mode -> Bool
isLossConstructor = \case
  StandardBuiltinType {} -> False
  StandardBuiltinConstructor {} -> True
  StandardBuiltinFunction {} -> False
  StandardDerivedFunction {} -> False
  LossBuiltinFunction {} -> False
  LossBuiltinType {} -> False
  LossBuiltinConstructor {} -> True
  LossBuiltinTypeClass {} -> False
  LossBuiltinTypeClassOp {} -> False
  LossBuiltinCast {} -> False

typeLossBuiltin :: (MonadTypeChecker (LossBuiltin mode) m) => LossMode -> Provenance -> LossBuiltin mode -> m (Expr (LossBuiltin mode))
typeLossBuiltin mode p = \case
  StandardDerivedFunction f -> getDeclType (Proxy @(LossBuiltin _)) (identifierOf f)
  b -> return $ fromDSL p $ case b of
    StandardBuiltinType t -> typeStandardBuiltinType t
    StandardBuiltinFunction f -> typeStandardFunction mode f
    StandardBuiltinConstructor c -> typeStandardConstructor c
    LossBuiltinType t -> typeLossBuiltinType t
    LossBuiltinConstructor c -> typeLossBuiltinConstructor c
    LossBuiltinTypeClass t -> typeLossTypeClass t
    LossBuiltinTypeClassOp t -> typeLossTypeClassOp mode t
    LossBuiltinCast t -> typeLossCast t
    LossBuiltinFunction t -> typeLossFunction t

typeLossBuiltinType :: LossBuiltinType -> DSLExpr (LossBuiltin mode)
typeLossBuiltinType = \case
  GradientType -> type0

typeLossCast :: LossBuiltinCast -> DSLExpr (LossBuiltin mode)
typeLossCast = \case
  FromBoolTensorToBoolTensor -> forAllDims $ \ds -> tBoolTensor ds ~> tBoolTensor ds
  FromBoolTensorToRatTensor -> forAllDims $ \ds -> tRatTensor dimNil ~> tRatTensor dimNil ~> tBoolTensor ds ~> tRatTensor ds

typeLossFunction :: LossBuiltinFunction -> DSLExpr (LossBuiltin mode)
typeLossFunction = \case
  IfRatTensorWithGradients -> typeIf tRatWithGradients

typeStandardBuiltinType :: BuiltinType -> DSLExpr (LossBuiltin mode)
typeStandardBuiltinType = \case
  UnitType -> type0
  BoolType -> type0
  IndexType -> tNat ~> type0
  NatType -> type0
  RatType -> tGradient .~> type0
  ListType -> type0 ~> type0
  VectorType -> type0 ~> tNat ~> type0
  TensorType -> type0 ~> tList tNat ~> type0

typeStandardConstructor :: BuiltinConstructor -> DSLExpr (LossBuiltin mode)
typeStandardConstructor c = case c of
  RatTensorLiteral rs -> tRatTensorWithoutGradients (shapeOf rs)
  -- The same....
  Nil -> typeOfBuiltinConstructor c
  Cons -> typeOfBuiltinConstructor c
  UnitLiteral -> typeOfBuiltinConstructor c
  IndexLiteral {} -> typeOfBuiltinConstructor c
  NatLiteral {} -> typeOfBuiltinConstructor c
  -- TODO: known bug. Have to separate out veclit into a specialised rat type that takes the max over gradients
  VectorLiteral -> typeOfBuiltinConstructor c
  BoolTensorLiteral {} -> typeOfBuiltinConstructor c
  NatTensorLiteral {} -> typeOfBuiltinConstructor c

typeLossBuiltinConstructor :: LossBuiltinConstructor -> DSLExpr (LossBuiltin mode)
typeLossBuiltinConstructor = \case
  WithGradients -> tGradient
  WithoutGradients -> tGradient

typeLossTypeClass :: LossBuiltinTypeClass -> DSLExpr (LossBuiltin mode)
typeLossTypeClass = \case
  HasBoolLiterals -> type0 ~> type0
  HasNot -> type0 ~> type0
  HasAnd -> type0 ~> type0 ~> type0 ~> type0
  HasOr -> type0 ~> type0 ~> type0 ~> type0
  HasImplies -> type0 ~> type0 ~> type0 ~> type0
  HasReduceAnd -> type0 ~> type0
  HasReduceOr -> type0 ~> type0
  HasRatTensorCompare {} -> type0 ~> type0 ~> type0 ~> type0
  HasExists -> type0 ~> type0
  HasIfRatTensor -> tGradient ~> tGradient ~> type0
  MaxGradients {} -> tGradient ~> tGradient ~> tGradient ~> tGradient
  ValidNetworkType -> type0 ~> type0
  ValidNetworkIOType -> tGradient ~> type0 ~> type0
  ValidDatasetType -> type0 ~> type0
  ValidParamType -> type0 ~> type0

typeLossTypeClassOp :: LossMode -> LossBuiltinTypeClassOp -> DSLExpr (LossBuiltin mode)
typeLossTypeClassOp mode = \case
  FromBoolTensorTC ->
    forAllTypes $ \t ->
      hasBoolLiterals t
        .~~~> forAllDims
          ( \dims ->
              tBoolTensor dims
                ~> tTensor t dims
          )
  NotTCOp -> unaryOp HasNot
  AndTCOp -> binaryOp hasAnd
  OrTCOp -> binaryOp hasOr
  ImpliesTCOp -> binaryOp hasImplies
  ReduceAndTCOp -> reductionOp HasReduceAnd
  ReduceOrTCOp -> reductionOp HasReduceOr
  CompareRatTensorTCOp op ->
    forAllTypeTriples $ \t1 t2 t3 ->
      hasRatTensorComparison op t1 t2 t3
        ~~~> typeOfCompareRatTensor t1 t2 t3
  IfRatTensorTCOp ->
    forAllTypes $ \t ->
      hasIfRatTensor t
        ~~~> typeIf t
  ExistsTCOp ->
    forAllTypes $ \t ->
      hasExists t
        ~~~> typeOfQuantifierOrSearch mode t
  where
    unaryOp tc =
      forAllTypes $ \t ->
        lossTypeClass tc
          @@ [t]
          ~~~> typeOfGenericOp1 t

    binaryOp tc =
      forAllTypeTriples $ \t1 t2 t3 ->
        tc t1 t2 t3
          ~~~> typeOfGenericOp2 t1 t2 t3

    reductionOp tc =
      forAllTypes $ \t ->
        lossTypeClass tc
          @@ [t]
          ~~~> forAllDims (\dims -> tTensor t dims ~> tTensor t dimNil)

typeStandardFunction :: LossMode -> BuiltinFunction -> DSLExpr (LossBuiltin mode)
typeStandardFunction mode f = case f of
  QuantifyRatTensor Exists -> typeOfQuantifierOrSearch mode tBool
  QuantifyRecord {} -> removed
  QuantifyRatTensor Forall -> removed
  CompareRatTensor {} -> typeOfCompareRatTensor tRatWithoutGradients tRatWithoutGradients tBool
  Neg NegRatTensor -> typeOfGenericGradOp1
  Add AddRatTensor -> typeOfGenericGradOp2
  Mul MulRatTensor -> typeOfGenericGradOp2
  Sub SubRatTensor -> typeOfGenericGradOp2
  Div DivRatTensor -> typeOfGenericGradOp2
  Min MinRatTensor -> typeOfGenericGradOp2
  Max MaxRatTensor -> typeOfGenericGradOp2
  Pow PowRatTensor -> typeOfPowRatTensor
  Log LogRatTensor -> typeOfGenericGradOp1
  Exp ExpRatTensor -> typeOfGenericGradOp1
  ReduceAddRatTensor -> typeOfReductionGradOp1
  ReduceMulRatTensor -> typeOfReductionGradOp1
  ReduceMinRatTensor -> typeOfReductionGradOp1
  ReduceMaxRatTensor -> typeOfReductionGradOp1
  SearchRatTensor {} -> forAllGradients $ \g -> typeOfQuantifierOrSearch mode (tRat .@@ [g])
  WhereTensor ->
    forAllGradients $ \g ->
      forAllDims $ \dims ->
        tTensor (tRat .@@ [g]) dims
          ~> tBoolTensor dims
          ~> tRatTensorWithoutGradients dimNil
          ~> tTensor (tRat .@@ [g]) dims
  -- TODO: known bug. Have to separate out stack into a specialised rat type that takes the max over gradients
  StackTensor -> typeOfBuiltinFunction f
  CompareIndex {} -> typeOfBuiltinFunction f
  CompareNat {} -> typeOfBuiltinFunction f
  Add AddNat -> typeOfBuiltinFunction f
  Mul MulNat -> typeOfBuiltinFunction f
  Not -> typeOfBuiltinFunction f
  And -> typeOfBuiltinFunction f
  Or -> typeOfBuiltinFunction f
  If -> typeOfBuiltinFunction f
  Implies -> typeOfBuiltinFunction f
  ReduceAndTensor -> typeOfBuiltinFunction f
  ReduceOrTensor -> typeOfBuiltinFunction f
  AtTensor -> typeOfBuiltinFunction f
  ConstTensor -> typeOfBuiltinFunction f
  Iterate -> typeOfBuiltinFunction f
  ForeachTensor -> typeOfBuiltinFunction f
  Transpose -> typeOfBuiltinFunction f
  AtVector -> typeOfBuiltinFunction f
  ForeachVector -> typeOfBuiltinFunction f
  FoldList -> typeOfBuiltinFunction f
  MapList -> typeOfBuiltinFunction f
  ReverseList -> typeOfBuiltinFunction f
  AppendList -> typeOfBuiltinFunction f
  where
    removed = developerError $ pretty f <+> "should have been removed prior to loss type-checking"

typeOp1 :: DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode)
typeOp1 t = t ~> t

typeOfGenericOp1 :: DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode)
typeOfGenericOp1 t = forAllDims $ \dims -> tTensor t dims ~> tTensor t dims

typeOfGenericOp2 :: DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode)
typeOfGenericOp2 t1 t2 t3 = forAllDims $ \dims -> tTensor t1 dims ~> tTensor t2 dims ~> tTensor t3 dims

typeOfGenericGradOp1 :: DSLExpr (LossBuiltin mode)
typeOfGenericGradOp1 =
  forAllGradients $ \g ->
    forAllDims $ \dims ->
      typeOp1 (tTensor (tRat .@@ [g]) dims)

typeOfReductionGradOp1 :: DSLExpr (LossBuiltin mode)
typeOfReductionGradOp1 =
  forAllGradients $ \g ->
    forAllDims $ \dims ->
      tTensor (tRat .@@ [g]) dims ~> tTensor (tRat .@@ [g]) dimNil

typeOfGenericGradOp2 :: DSLExpr (LossBuiltin mode)
typeOfGenericGradOp2 =
  forAllGradientTriples $ \g1 g2 g3 ->
    maxGradients g1 g2 g3
      .~~~> forAllDims
        ( \dims ->
            tTensor (tRat .@@ [g1]) dims
              ~> tTensor (tRat .@@ [g2]) dims
              ~> tTensor (tRat .@@ [g3]) dims
        )

typeOfPowRatTensor :: DSLExpr (LossBuiltin mode)
typeOfPowRatTensor =
  forAllGradients $ \g ->
    forAllDims $ \dims ->
      tTensor (tRat .@@ [g]) dims ~> tRat ~> tTensor (tRat .@@ [g]) dims

typeOfCompareRatTensor :: DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode)
typeOfCompareRatTensor t1 t2 t3 =
  forAllDims $ \pointwiseDims ->
    forAllDims $ \reduceDims ->
      tTensor t1 (append tNat pointwiseDims reduceDims)
        ~> tTensor t2 (append tNat pointwiseDims reduceDims)
        ~> tTensor t3 pointwiseDims

typeIf :: DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode)
typeIf inputType =
  forAllTypes $ \t ->
    tTensor inputType dimNil ~> t ~> t ~> t

typeOfQuantifierOrSearch :: LossMode -> DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode)
typeOfQuantifierOrSearch mode outputType = do
  let inputGradient = case mode of
        Train -> withoutGradients
        Search -> withGradients

  forAllDims $ \dims ->
    -- Lower bounds for search space
    tRatTensorWithoutGradients dims
      ~>
      -- Upper bounds for search space
      tRatTensorWithoutGradients dims
      ~>
      -- Function to optimise for
      (tTensor (tRat .@@ [inputGradient]) dims ~> tTensor outputType dimNil)
      ~>
      -- Return type
      tTensor outputType dimNil

--------------------------------------------------------------------------------
-- TypeSystem
--------------------------------------------------------------------------------

instance (TypableBuiltin (LossBuiltin mode)) => HasTypeSystem (LossBuiltin mode) where
  convertFromStandardBuiltins x = prependMissingFreeVarImplicitArgs =<< convertToLossBuiltins x
  restrictDeclType = restrictDecidabilityDeclType
  restrictRecordAnnotatedAsTensor = restrictDecidabilityRecordAnnotatedAsTensor
  isAuxiliaryConstraint _ = False

  solveAuxiliaryInstanceConstraint _ = return ()
  addAuxiliaryInputOutputConstraints = return
  generateDefaultAuxiliaryConstraint _ = return False

convertToLossBuiltins ::
  forall m mode.
  (MonadTypeChecker (LossBuiltin mode) m) =>
  Decl Builtin ->
  m (Decl (LossBuiltin mode))
convertToLossBuiltins decl = do
  -- General all occurrences of the type `Real` get replaced with `Real ?` with a hole
  -- for the gradient information to be inferred.
  let mkRealTypeArg = (`Hole` "_")

  case decl of
    -- The exception is the record declaration of a DifferentiableLogic whose operations we know
    -- will only be applied to places with gradient information and therefore `Real` is
    -- replaced with `Real WithGradients` and `Real WithoutGradients` otherwise.
    DefRecord p ident sort telescope fields ops | identifierOf decl == differentiableTensorLogicIdent -> do
      let mkRealTypeArgWithGradients p' = Builtin p' (LossBuiltinConstructor WithGradients)
      let mkRealTypeArgWithoutGradients p' = Builtin p' (LossBuiltinConstructor WithoutGradients)
      telescope' <- traverse (traverse (updateBuiltins mkRealTypeArg)) telescope

      let updateField (fieldName, fieldType) = do
            let isElement = nameOf fieldName `elem` ([nameOf TruthityElement, nameOf FalsityElement] :: [Name])
            let mkArgFn = if isElement then mkRealTypeArgWithoutGradients else mkRealTypeArgWithGradients
            fieldType' <- updateBuiltins mkArgFn fieldType
            return (fieldName, fieldType')

      fields' <- traverse updateField fields
      return $ DefRecord p ident sort telescope' fields' ops
    _ -> traverse (updateBuiltins mkRealTypeArg) decl
  where
    updateBuiltins :: (Provenance -> Expr (LossBuiltin mode)) -> Expr Builtin -> m (Expr (LossBuiltin mode))
    updateBuiltins mkRealTypeArg = traverseBuiltinsM (updateBuiltin mkRealTypeArg)

    updateBuiltin ::
      (Provenance -> Expr (LossBuiltin mode)) ->
      BuiltinUpdate m Builtin (LossBuiltin mode)
    updateBuiltin mkRatTypeArg p b args =
      case b of
        BuiltinFunction f -> do
          case f of
            -- Convert to type-classes for resolution
            Not -> convertTo 1 (LossBuiltinTypeClassOp NotTCOp)
            And -> convertTo 3 (LossBuiltinTypeClassOp AndTCOp)
            Or -> convertTo 3 (LossBuiltinTypeClassOp OrTCOp)
            Implies -> convertTo 3 (LossBuiltinTypeClassOp ImpliesTCOp)
            CompareRatTensor op -> convertTo 3 (LossBuiltinTypeClassOp $ CompareRatTensorTCOp op)
            ReduceAndTensor -> convertTo 1 (LossBuiltinTypeClassOp ReduceAndTCOp)
            ReduceOrTensor -> convertTo 1 (LossBuiltinTypeClassOp ReduceOrTCOp)
            If -> convertTo 1 (LossBuiltinTypeClassOp IfRatTensorTCOp)
            QuantifyRatTensor Exists -> convertTo 1 (LossBuiltinTypeClassOp ExistsTCOp)
            Neg NegRatTensor -> convertTo 1 (StandardBuiltinFunction f)
            Add AddRatTensor -> convertTo 3 (StandardBuiltinFunction f)
            Sub SubRatTensor -> convertTo 3 (StandardBuiltinFunction f)
            Mul MulRatTensor -> convertTo 3 (StandardBuiltinFunction f)
            Div DivRatTensor -> convertTo 3 (StandardBuiltinFunction f)
            Min MinRatTensor -> convertTo 3 (StandardBuiltinFunction f)
            Max MaxRatTensor -> convertTo 3 (StandardBuiltinFunction f)
            Pow PowRatTensor -> convertTo 1 (StandardBuiltinFunction f)
            Exp ExpRatTensor -> convertTo 1 (StandardBuiltinFunction f)
            Log LogRatTensor -> convertTo 1 (StandardBuiltinFunction f)
            ReduceAddRatTensor -> convertTo 1 (StandardBuiltinFunction f)
            ReduceMulRatTensor -> convertTo 1 (StandardBuiltinFunction f)
            ReduceMinRatTensor -> convertTo 1 (StandardBuiltinFunction f)
            ReduceMaxRatTensor -> convertTo 1 (StandardBuiltinFunction f)
            WhereTensor -> convertTo 1 (StandardBuiltinFunction f)
            SearchRatTensor -> convertTo 1 (StandardBuiltinFunction f)
            -- Nothing needs to change
            Add AddNat -> sameFunction f
            Mul MulNat -> sameFunction f
            FoldList -> sameFunction f
            MapList -> sameFunction f
            ReverseList -> sameFunction f
            AppendList -> sameFunction f
            Iterate -> sameFunction f
            Transpose -> sameFunction f
            StackTensor -> sameFunction f
            AtTensor -> sameFunction f
            ConstTensor -> sameFunction f
            ForeachTensor -> sameFunction f
            ForeachVector -> sameFunction f
            AtVector -> sameFunction f
            CompareIndex {} -> sameFunction f
            CompareNat {} -> sameFunction f
            -- Should have been eliminated
            QuantifyRatTensor Forall -> developerError "`forall`s should have been eliminated"
            QuantifyRecord {} -> developerError "quantifiers should have been eliminated"
        BuiltinConstructor c -> return $ case c of
          BoolTensorLiteral {} -> castWith FromBoolTensorTC (sameConstructor c)
          _ -> sameConstructor c
        BuiltinType t -> case t of
          RatType -> return $ normAppList (Builtin p $ StandardBuiltinType t) [explicitIrrelevant (mkRatTypeArg p)]
          BoolType -> return $ Hole p "_"
          _ -> return $ normAppList (Builtin p $ StandardBuiltinType t) args
        DerivedFunction f -> case f of
          TypeAnn -> convertTo 0 (StandardDerivedFunction f)
          QuantifyIndex {} -> convertTo 1 (StandardDerivedFunction f)
          QuantifyInList {} -> convertTo 1 (StandardDerivedFunction f)
        _ -> monomorphisationError b args
      where
        -- Nothing changes
        sameFunction f = return $ normAppList (Builtin p (StandardBuiltinFunction f)) args
        sameConstructor c = normAppList (Builtin p $ StandardBuiltinConstructor c) args

        -- Apply a cast
        prependHoles n xs = replicate n (implicit $ Hole p "_") <> xs
        castWith f original = normAppList (Builtin p $ LossBuiltinTypeClassOp f) [explicit original]
        convertTo n f = return $ normAppList (Builtin p f) (prependHoles n args)

restrictDecidabilityDeclType ::
  forall m mode.
  (MonadTypeChecker (LossBuiltin mode) m, TypableBuiltin (LossBuiltin mode)) =>
  RestrictedDecl ->
  DeclProvenance ->
  Type (LossBuiltin mode) ->
  m (Type (LossBuiltin mode))
restrictDecidabilityDeclType declSort (ident, p) declType = do
  maybeTypeClass <- case declSort of
    RestrictedNetwork -> return (Just ValidNetworkType)
    RestrictedDataset -> return (Just ValidDatasetType)
    RestrictedParameter {} -> return (Just ValidParamType)
    RestrictedProperty -> return Nothing

  case maybeTypeClass of
    Nothing -> return ()
    Just tc -> do
      freeEnv <- getFreeCtx (Proxy @(LossBuiltin mode))
      let expr = App (Builtin p (LossBuiltinTypeClass tc)) [explicit declType]
      let origin = InstanceTypeRestrictionOrigin $ TypeRestrictionOrigin freeEnv (ident, provenanceOf declType) (Left declSort) declType
      _ <- createFreshInstanceConstraint False mempty p origin Irrelevant expr
      return ()

  return declType

restrictDecidabilityRecordAnnotatedAsTensor ::
  forall m mode.
  (MonadTypeChecker (LossBuiltin mode) m) =>
  DeclProvenance ->
  [GenericRecordField (Type (LossBuiltin mode))] ->
  m ()
restrictDecidabilityRecordAnnotatedAsTensor (_ident, _p) _fields =
  return ()
