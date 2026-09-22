{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Data.Builtin.Loss.Type
  ( typeLossBuiltin,
  )
where

import Control.Monad.Writer.Strict (Writer, runWriter, tell)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid (Any (..))
import Data.Proxy (Proxy (..))
import Vehicle.Backend.ITP.Core (ComparisonType (..), decideIfPointwiseOrReductionComparison)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Monad.Class (getDeclType, prependMissingFreeVarImplicitArgs)
import Vehicle.Compile.Type.System
import Vehicle.Data.Builtin.Interface (Accessor (..))
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
import Vehicle.Data.Code.Interface.Args (IsArgs (..), StackTensorArgs (..), VectorLitArgs (..))
import Vehicle.Data.DSL
import Vehicle.Data.DifferentiableLogic (TensorDifferentiableLogicField (..))
import Vehicle.Data.Variable.Free.Context (MonadFreeContext (..))
import Vehicle.Libraries.StandardLibrary (differentiableTensorLogicIdent)
import Prelude hiding (iterate, pi)

--------------------------------------------------------------------------------
-- Typing
--------------------------------------------------------------------------------

instance TypableBuiltin (LossBuiltin 'Train) where
  typeBuiltin = typeLossBuiltin
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

typeLossBuiltin :: (MonadTypeChecker (LossBuiltin mode) m) => Provenance -> LossBuiltin mode -> m (Expr (LossBuiltin mode))
typeLossBuiltin p = \case
  StandardDerivedFunction f -> getDeclType (Proxy @(LossBuiltin _)) (identifierOf f)
  b -> return $ fromDSL p $ case b of
    StandardBuiltinType t -> typeStandardBuiltinType t
    StandardBuiltinFunction f -> typeStandardFunction f
    StandardBuiltinConstructor c -> typeStandardConstructor c
    LossBuiltinType t -> typeLossBuiltinType t
    LossBuiltinConstructor c -> typeLossBuiltinConstructor c
    LossBuiltinTypeClass t -> typeLossTypeClass t
    LossBuiltinTypeClassOp t -> typeLossTypeClassOp t
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
  StackRatTensorWithGradients -> typeOfStackRatTensorWithGradients

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
  -- Only reached when no `Real` is written in the element type; see `VectorLiteralWithGradients`.
  VectorLiteral -> typeOfBuiltinConstructor c
  BoolTensorLiteral {} -> typeOfBuiltinConstructor c
  NatTensorLiteral {} -> typeOfBuiltinConstructor c

typeLossBuiltinConstructor :: LossBuiltinConstructor -> DSLExpr (LossBuiltin mode)
typeLossBuiltinConstructor = \case
  WithGradients -> tGradient
  WithoutGradients -> tGradient
  VectorLiteralWithGradients -> typeOfVectorLiteralWithGradients

typeLossTypeClass :: LossBuiltinTypeClass -> DSLExpr (LossBuiltin mode)
typeLossTypeClass = \case
  HasBoolLiterals -> type0 ~> type0
  HasNot -> type0 ~> type0
  HasAnd -> type0 ~> type0 ~> type0 ~> type0
  HasOr -> type0 ~> type0 ~> type0 ~> type0
  HasImplies -> type0 ~> type0 ~> type0 ~> type0
  HasReduceAnd -> type0 ~> type0
  HasReduceOr -> type0 ~> type0
  HasPointwiseRatTensorCompare {} -> type0 ~> type0 ~> type0 ~> type0
  HasReducedRatTensorCompare {} -> type0 ~> type0 ~> type0 ~> type0
  HasExists -> type0 ~> type0
  HasIfRatTensor -> tGradient ~> tGradient ~> type0
  MaxGradients {} -> tGradient ~> tGradient ~> tGradient ~> tGradient
  ValidNetworkType -> type0 ~> type0
  ValidNetworkIOType -> tGradient ~> type0 ~> type0
  ValidDatasetType -> type0 ~> type0
  ValidParamType -> type0 ~> type0

typeLossTypeClassOp :: LossBuiltinTypeClassOp -> DSLExpr (LossBuiltin mode)
typeLossTypeClassOp = \case
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
  CompareRatTensorPointwiseTCOp op ->
    forAllTypeTriples $ \t1 t2 t3 ->
      hasPointwiseRatTensorComparison op t1 t2 t3
        ~~~> typeOfPointwiseCompareRatTensor t1 t2 t3
  CompareRatTensorReducedTCOp op ->
    forAllTypeTriples $ \t1 t2 t3 ->
      hasReducedRatTensorComparison op t1 t2 t3
        ~~~> typeOfReducedCompareRatTensor t1 t2 t3
  IfRatTensorTCOp ->
    forAllTypes $ \t ->
      hasIfRatTensor t
        ~~~> typeIf t
  ExistsTCOp ->
    forAllTypes $ \t ->
      hasExists t
        ~~~> typeOfQuantifierOrSearch t
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

typeStandardFunction :: BuiltinFunction -> DSLExpr (LossBuiltin mode)
typeStandardFunction f = case f of
  QuantifyRatTensor Exists -> typeOfQuantifierOrSearch tBool
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
  SearchRatTensor {} -> forAllGradients $ \g -> typeOfQuantifierOrSearch (tRat .@@ [g])
  WhereTensor ->
    forAllGradients $ \g ->
      forAllDims $ \dims ->
        tTensor (tRat .@@ [g]) dims
          ~> tBoolTensor dims
          ~> tRatTensorWithoutGradients dimNil
          ~> tTensor (tRat .@@ [g]) dims
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

typeOfStackRatTensorWithGradients :: DSLExpr (LossBuiltin mode)
typeOfStackRatTensorWithGradients =
  forAll "n" tNat $ \n ->
    forAllDim Relevant $ \d ->
      forAllDims $ \ds ->
        iterate (tGradient ~> type0) (accumulateGradient (\g -> tTensor (tRat .@@ [g]) ds)) n (finalTensor d ds)
          @@ [withoutGradients]
  where
    finalTensor d ds =
      explLam "g" tGradient $ \g ->
        tTensor (tRat .@@ [g]) (dimCons d ds)

typeOfVectorLiteralWithGradients :: DSLExpr (LossBuiltin mode)
typeOfVectorLiteralWithGradients =
  forAll "t" (tGradient .~> type0) $ \t ->
    forAllDim Relevant $ \d ->
      iterate (tGradient ~> type0) (accumulateGradient (\g -> t .@@ [g])) d (finalVector t d)
        @@ [withoutGradients]
  where
    finalVector t d =
      explLam "g" tGradient $ \g ->
        tVector (t .@@ [g]) d

accumulateGradient ::
  (DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode)) ->
  DSLExpr (LossBuiltin mode) ->
  DSLExpr (LossBuiltin mode) ->
  DSLExpr (LossBuiltin mode)
accumulateGradient elementType recurse base =
  explLam "g" tGradient $ \accSoFar ->
    forAllGradients $ \element ->
      forAllGradients $ \accNext ->
        maxGradients accSoFar element accNext
          .~~~> (elementType element ~> (recurse @@ [base]) @@ [accNext])

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

typeOfPointwiseCompareRatTensor :: DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode)
typeOfPointwiseCompareRatTensor t1 t2 t3 =
  forAllDims $ \dims ->
    tTensor t1 dims
      ~> tTensor t2 dims
      ~> tTensor t3 dims

typeOfReducedCompareRatTensor :: DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode)
typeOfReducedCompareRatTensor t1 t2 t3 =
  forAllDims $ \dims ->
    tTensor t1 dims
      ~> tTensor t2 dims
      ~> tTensor t3 dimNil

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

typeOfQuantifierOrSearch :: DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode)
typeOfQuantifierOrSearch outputType = do
  forAllDims $ \dims ->
    forAllGradientPairs $ \g1 g2 ->
      -- Lower bounds for search space
      tTensor (tRat .@@ [g1]) dims
        ~>
        -- Upper bounds for search space
        tTensor (tRat .@@ [g2]) dims
        ~>
        -- Function to optimise for. The input variable always has gradients
        -- as we will be using PGD to optimise over it.
        (tRatTensorWithGradients dims ~> tTensor outputType dimNil)
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

  -- A `Bool` is a proposition, which is translated either to a `Bool` or to a loss value
  -- depending on whether it has gradients, so the type is left to be inferred.
  let mkPropositionType = (`Hole` "_")

  case decl of
    -- The exception is the record declaration of a DifferentiableLogic whose operations we know
    -- will only be applied to places with gradient information and therefore `Real` is
    -- replaced with `Real WithGradients` and `Real WithoutGradients` otherwise.
    DefRecord p ident sort telescope fields ops | identifierOf decl == differentiableTensorLogicIdent -> do
      let mkRealTypeArgWithGradients p' = Builtin p' (LossBuiltinConstructor WithGradients)
      let mkRealTypeArgWithoutGradients p' = Builtin p' (LossBuiltinConstructor WithoutGradients)
      telescope' <- traverse (traverse (updateBuiltins mkRealTypeArg mkPropositionType)) telescope

      let updateField (fieldName, fieldType) = do
            let isElement = nameOf fieldName `elem` ([nameOf TruthityElement, nameOf FalsityElement] :: [Name])
            let mkArgFn = if isElement then mkRealTypeArgWithoutGradients else mkRealTypeArgWithGradients
            fieldType' <- updateBuiltins mkArgFn mkPropositionType fieldType
            return (fieldName, fieldType')

      fields' <- traverse updateField fields
      return $ DefRecord p ident sort telescope' fields' ops
    -- In a resource a `Bool` is data supplied from outside rather than a proposition, so it stays
    -- a `Bool`.
    DefAbstract _ _ sort _ | isAnnotatedAsExternalResource sort -> do
      let mkBoolType p' = Builtin p' (StandardBuiltinType BoolType)
      traverse (updateBuiltins mkRealTypeArg mkBoolType) decl
    _ -> traverse (updateBuiltins mkRealTypeArg mkPropositionType) decl
  where
    updateBuiltins ::
      (Provenance -> Expr (LossBuiltin mode)) ->
      (Provenance -> Expr (LossBuiltin mode)) ->
      Expr Builtin ->
      m (Expr (LossBuiltin mode))
    updateBuiltins mkRealTypeArg mkBoolTypeExpr = traverseBuiltinsM (updateBuiltin mkRealTypeArg mkBoolTypeExpr)

    updateBuiltin ::
      (Provenance -> Expr (LossBuiltin mode)) ->
      (Provenance -> Expr (LossBuiltin mode)) ->
      BuiltinUpdate m Builtin (LossBuiltin mode)
    updateBuiltin mkRatTypeArg mkBoolTypeExpr p b args =
      case b of
        BuiltinFunction f -> do
          case f of
            -- Convert to type-classes for resolution
            Not -> convertTo 1 (LossBuiltinTypeClassOp NotTCOp)
            And -> convertTo 3 (LossBuiltinTypeClassOp AndTCOp)
            Or -> convertTo 3 (LossBuiltinTypeClassOp OrTCOp)
            Implies -> convertTo 3 (LossBuiltinTypeClassOp ImpliesTCOp)
            CompareRatTensor op -> case decideIfPointwiseOrReductionComparison args of
              -- This is a hack as in order to implement this properly we need
              -- to equip the `reduce` operations with the dimensions to reduce so that
              -- we can define the correct general instance.
              Reduced rArgs -> return $ normAppList (Builtin p (LossBuiltinTypeClassOp $ CompareRatTensorReducedTCOp op)) (prependHoles 3 rArgs)
              Pointwise pArgs -> return $ normAppList (Builtin p (LossBuiltinTypeClassOp $ CompareRatTensorPointwiseTCOp op)) (prependHoles 3 pArgs)
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
            StackTensor -> case getExpr accessSpine args of
              -- Only rational stacks carry gradients, and the arity replaces the element type.
              Just (StackTensorArgs elementType d ds xs)
                | isRatType elementType ->
                    return $
                      normAppList (Builtin p (LossBuiltinFunction StackRatTensorWithGradients)) $
                        implicit (arityOf xs) : implicit d : implicit ds : fmap explicit xs
              _ -> sameFunction f
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
          -- Only elements with a `Real` carry gradients, and each is typed separately, so the
          -- element type becomes a family over the gradient.
          VectorLiteral -> case getExpr accessSpine args of
            Just (VectorLitArgs elementType d xs)
              | Just family <- gradientFamily p elementType ->
                  normAppList (Builtin p (LossBuiltinConstructor VectorLiteralWithGradients)) $
                    implicit family : implicitIrrelevant d : fmap explicit xs
            _ -> sameConstructor c
          _ -> sameConstructor c
        BuiltinType t -> case t of
          RatType -> return $ normAppList (Builtin p $ StandardBuiltinType t) [explicitIrrelevant (mkRatTypeArg p)]
          BoolType -> return $ mkBoolTypeExpr p
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

        arityOf elements = Builtin p (StandardBuiltinConstructor (NatLiteral (length elements)))

        -- `Real` has already been given its gradient argument by the time the stack is reached.
        isRatType = \case
          Builtin _ (StandardBuiltinType RatType) -> True
          App (Builtin _ (StandardBuiltinType RatType)) _ -> True
          _ -> False

        -- Apply a cast
        prependHoles n xs = replicate n (implicit $ Hole p "_") <> xs
        castWith f original = normAppList (Builtin p $ LossBuiltinTypeClassOp f) [explicit original]
        convertTo n f = return $ normAppList (Builtin p f) (prependHoles n args)

-- | The family `\g -> t[g]` giving every `Real` in `t` the gradient `g`, if `t` has one. The binder
-- is irrelevant, so removing irrelevant code after typing turns the family back into `t`.
gradientFamily :: Provenance -> Expr (LossBuiltin mode) -> Maybe (Expr (LossBuiltin mode))
gradientFamily p t = do
  let (body, Any hasReal) = runWriter $ go 0 (liftDBIndices 1 t)
  if hasReal then Just (Lam p binder body) else Nothing
  where
    binder = Binder (BinderDisplayForm (OnlyName "g" p) True) Explicit Irrelevant (Builtin p (LossBuiltinType GradientType))

    go :: Int -> Expr (LossBuiltin mode) -> Writer Any (Expr (LossBuiltin mode))
    go depth expr = case expr of
      App fun@(Builtin _ (StandardBuiltinType RatType)) (gradient :| []) -> do
        tell (Any True)
        return $ App fun [BoundVar p (Ix depth) <$ gradient]
      App fun args -> App <$> go depth fun <*> traverse (traverse (go depth)) args
      Pi p' b res -> Pi p' <$> traverse (go depth) b <*> go (depth + 1) res
      Lam p' b body -> Lam p' <$> traverse (go depth) b <*> go (depth + 1) body
      Let p' bound b body -> Let p' <$> go depth bound <*> traverse (go depth) b <*> go (depth + 1) body
      Record p' ident fields -> Record p' ident <$> traverseRecordFields (go depth) fields
      RecordProj p' recordType value field -> RecordProj p' <$> go depth recordType <*> go depth value <*> pure field
      Universe {} -> return expr
      FreeVar {} -> return expr
      BoundVar {} -> return expr
      Hole {} -> return expr
      Meta {} -> return expr
      Builtin {} -> return expr

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
