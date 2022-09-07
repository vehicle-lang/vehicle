module Vehicle.Compile.Type.Bidirectional
  ( TCM
  , checkExpr
  , inferExpr
  , runTCM
  ) where

import Prelude hiding (pi)
import Control.Monad (when)
import Control.Monad.Except (MonadError(..))
import Control.Monad.Reader (MonadReader(..), ReaderT (runReaderT))
import Control.Monad.State (MonadState, StateT, evalStateT)
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Monoid (Endo(..), appEndo)
import Data.Text (pack)

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Language.DSL
import Vehicle.Language.Print
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.WeakHeadNormalForm
import Vehicle.Compile.Type.VariableContext
import Vehicle.Compile.Type.Constraint

--------------------------------------------------------------------------------
-- Bidirectional type-checking

-- Recurses through the expression, switching between check and infer modes.
-- Inserts meta-variables for missing implicit and instance arguments and
-- gathers the constraints over those meta-variables.

--------------------------------------------------------------------------------
-- The type-checking monad

-- | The type-checking monad
type TCM m =
  ( MonadCompile                   m
  , MonadState  MetaCtx            m
  , MonadReader TypingVariableCtx  m
  )

runTCM :: MonadCompile m => ReaderT TypingVariableCtx (StateT MetaCtx m) a -> m a
runTCM e = evalStateT (runReaderT e emptyVariableCtx) emptyMetaCtx

--------------------------------------------------------------------------------
-- Debug functions

showCheckEntry :: MonadLogger m => CheckedType -> UncheckedExpr -> m ()
showCheckEntry t e = do
  logDebug MaxDetail ("check-entry" <+> prettyVerbose e <+> "<-" <+> prettyVerbose t)
  incrCallDepth

showCheckExit :: MonadLogger m => CheckedExpr -> m ()
showCheckExit e = do
  decrCallDepth
  logDebug MaxDetail ("check-exit " <+> prettyVerbose e)

showInferEntry :: MonadLogger m => UncheckedExpr -> m ()
showInferEntry e = do
  logDebug MaxDetail ("infer-entry" <+> prettyVerbose e)
  incrCallDepth

showInferExit :: MonadLogger m => (CheckedExpr, CheckedType) -> m ()
showInferExit (e, t) = do
  decrCallDepth
  logDebug MaxDetail ("infer-exit " <+> prettyVerbose e <+> "->" <+> prettyVerbose t)

-------------------------------------------------------------------------------
-- Utility functions

removeBinderName :: CheckedBinder -> CheckedBinder
removeBinderName (Binder ann v r _n t) = Binder ann v r Nothing t

unify :: TCM m => Provenance -> CheckedExpr -> CheckedExpr -> m ()
unify p e1 e2 = do
  ctx <- getVariableCtx
  -- TODO calculate the most general unifier
  addUnificationConstraint TypeGroup p ctx e1 e2

--------------------------------------------------------------------------------
-- Checking

checkExpr :: TCM m
          => CheckedType   -- Type we're checking against
          -> UncheckedExpr -- Expression being type-checked
          -> m CheckedExpr -- Updated expression
checkExpr expectedType expr = do
  showCheckEntry expectedType expr
  res <- case (expectedType, expr) of
    -- If the type is a meta, then we're forced to switch to infer.
    (Meta ann _, _) -> viaInfer ann expectedType expr

    (Pi _ piBinder resultType, Lam ann lamBinder body)
      | visibilityOf piBinder == visibilityOf lamBinder -> do
        checkedLamBinderType <- checkExpr (TypeUniverse (inserted ann) 0) (typeOf lamBinder)

        -- Unify the result with the type of the pi binder.
        unify (provenanceOf ann) (typeOf piBinder) checkedLamBinderType

        -- Add bound variable to context
        checkedBody <- addToBoundCtx (nameOf lamBinder, checkedLamBinderType, Nothing) $ do
          -- Check if the type of the expression matches the expected result type.
          checkExpr resultType body

        let checkedLamBinder = replaceBinderType checkedLamBinderType lamBinder
        return $ Lam ann checkedLamBinder checkedBody

    (Pi _ piBinder resultType, e)
      | isImplicit piBinder || isInstance piBinder -> do
      -- Then eta-expand
      let ann = inserted $ provenanceOf piBinder
      let binderName = nameOf piBinder
      let binderType = typeOf piBinder

      -- Add the pi-bound variable to the context
      checkedExpr <- addToBoundCtx (binderName, binderType, Nothing) $
        -- Check if the type of the expression matches the expected result type.
        checkExpr resultType (liftFreeDBIndices 1 e)

      -- Create a new binder mirroring the Pi binder expected
      let lamBinder = Binder ann (visibilityOf piBinder) (relevanceOf piBinder) binderName binderType

      -- Prepend a new lambda to the expression with the implicit binder
      return $ Lam ann lamBinder checkedExpr

    (_, Lam ann binder _) -> do
      ctx <- getBoundCtx
      let expected = fromDSL ann $ pi (visibilityOf binder) (relevanceOf binder) (tHole "a") (const (tHole "b"))
      throwError $ TypeMismatch (provenanceOf ann) (boundContextOf ctx) expectedType expected

    (_, Hole p _name) -> do
      -- Replace the hole with meta-variable.
      -- NOTE, different uses of the same hole name will be interpreted as
      -- different meta-variables.
      freshExprMeta p expectedType =<< getBoundCtx

    (_, Universe  ann _)    -> viaInfer ann expectedType expr
    (_, Meta     ann _)     -> viaInfer ann expectedType expr
    (_, App      ann _ _)   -> viaInfer ann expectedType expr
    (_, Pi       ann _ _)   -> viaInfer ann expectedType expr
    (_, Builtin  ann _)     -> viaInfer ann expectedType expr
    (_, Var      ann _)     -> viaInfer ann expectedType expr
    (_, Let      ann _ _ _) -> viaInfer ann expectedType expr
    (_, Literal  ann _)     -> viaInfer ann expectedType expr
    (_, LVec     ann _)     -> viaInfer ann expectedType expr
    (_, Ann      ann _ _)   -> viaInfer ann expectedType expr

  showCheckExit res
  return res

viaInfer :: TCM m => Provenance -> CheckedType -> UncheckedExpr -> m CheckedExpr
viaInfer ann expectedType e = do
  -- Switch to inference mode
  (checkedExpr, actualType) <- inferExpr e
  -- Insert any needed implicit or instance arguments
  (appliedCheckedExpr, resultType) <- inferApp ann checkedExpr actualType []
  -- Assert the expected and the actual types are equal
  unify ann expectedType resultType
  return appliedCheckedExpr

--------------------------------------------------------------------------------
-- Inference

-- | Takes in an unchecked expression and attempts to infer it's type.
-- Returns the expression annotated with its type as well as the type itself.
inferExpr :: TCM m
          => UncheckedExpr
          -> m (CheckedExpr, CheckedType)
inferExpr e = do
  showInferEntry e
  res <- case e of
    Universe ann u -> case u of
      TypeUniv l   -> return (e , TypeUniverse (inserted ann) (l + 1))
      _            -> compilerDeveloperError $
        "Should not be trying to infer the type of" <+> pretty u

    Meta _ m -> do
      metaType <- getMetaType m
      return (e, metaType)

    Hole ann _name -> do
      -- Replace the hole with meta-variable.
      -- NOTE, different uses of the same hole name will be interpreted
      -- as different meta-variables.
      metaType <- freshExprMeta (provenanceOf ann) (TypeUniverse ann 0) =<< getBoundCtx
      metaExpr <- freshExprMeta (provenanceOf ann) metaType =<< getBoundCtx
      unify ann metaType (TypeUniverse ann 0)
      return (metaExpr, metaType)

    Ann ann expr exprType -> do
      (checkedExprType, exprTypeType) <- inferExpr exprType
      unify (provenanceOf ann) exprTypeType (TypeUniverse (inserted ann) 0)
      checkedExpr <- checkExpr checkedExprType expr
      return (Ann ann checkedExpr checkedExprType , checkedExprType)

    Pi ann binder resultType -> do
      (checkedBinderType, typeOfBinderType) <- inferExpr (typeOf binder)

      (checkedResultType, typeOfResultType) <-
        addToBoundCtx (nameOf binder, checkedBinderType, Nothing) $ inferExpr resultType

      let maxResultType = typeOfBinderType `tMax` typeOfResultType
      let checkedBinder = replaceBinderType checkedBinderType binder
      return (Pi ann checkedBinder checkedResultType , maxResultType)

    -- Literals are slightly tricky to type-check, as by default they
    -- probably are standalone, i.e. not wrapped in an `App`, in which
    -- case we need to insert an `App` around them. However, if the
    -- user has provided an implicit argument to them or we are type
    -- checking a second time, then the `App` will already be present.
    -- One approach might be to pass a boolean flag through `infer`
    -- which signals whether the parent node is an `App`, however
    -- for now it's simplier to split into the following two cases:
    App ann (Literal ann' l) args -> do
      let (checkedLit, checkedLitType) = inferLiteral ann' l
      inferApp ann checkedLit checkedLitType (NonEmpty.toList args)

    Literal ann l -> do
      let (checkedLit, checkedLitType) = inferLiteral ann l
      inferApp ann checkedLit checkedLitType []


    App ann fun args -> do
      (checkedFun, checkedFunType) <- inferExpr fun
      inferApp ann checkedFun checkedFunType (NonEmpty.toList args)

    Var ann (Bound i) -> do
      -- Lookup the type of the variable in the context.
      ctx <- getBoundCtx
      case ctx !!? i of
        Just (_, checkedType, _) -> do
          let liftedCheckedType = liftFreeDBIndices (i+1) checkedType
          return (Var ann (Bound i), liftedCheckedType)
        Nothing      -> compilerDeveloperError $
          "DBIndex" <+> pretty i <+> "out of bounds when looking" <+>
          "up variable in context" <+> prettyVerbose (boundContextOf ctx) <+> "at" <+> pretty (provenanceOf ann)

    Var ann (Free ident) -> do
      -- Lookup the type of the declaration variable in the context.
      checkedType <- getDeclType ann ident
      return (Var ann (Free ident), checkedType)

    Let ann boundExpr binder body -> do
      -- Check the type of the bound expression against the provided type
      (typeOfBoundExpr, typeOfBoundExprType) <- inferExpr (typeOf binder)
      unify ann typeOfBoundExprType (TypeUniverse (inserted ann) 0)
      checkedBoundExpr <- checkExpr typeOfBoundExpr boundExpr

      let checkedBinder = replaceBinderType typeOfBoundExpr binder

      (checkedBody, typeOfBody) <-
        addToBoundCtx (nameOf binder, typeOfBoundExpr, Just checkedBoundExpr) $ inferExpr body

      -- It's possible for the type of the body to depend on the let bound variable,
      -- e.g. `let y = Nat in (2 : y)` so in order to avoid the DeBruijn index escaping
      -- it's context we need to substitute the bound expression into the type.
      normTypeOfBody <- if isMeta typeOfBody
        then return typeOfBody
        else do
          let normTypeOfBody = checkedBoundExpr `substInto` typeOfBody
          when (normTypeOfBody /= typeOfBody) $
            logDebug MaxDetail $ "normalising" <+> prettyVerbose typeOfBody <+> "to" <+> prettyVerbose normTypeOfBody
          return normTypeOfBody

      return (Let ann checkedBoundExpr checkedBinder checkedBody , normTypeOfBody)

    Lam ann binder body -> do
      -- Infer the type of the bound variable from the binder
      (typeOfBinder, typeOfBinderType) <- inferExpr (typeOf binder)

      let insertedAnn = inserted ann
      unify ann typeOfBinderType (TypeUniverse insertedAnn 0)
      let checkedBinder = replaceBinderType typeOfBinder binder

      -- Update the context with the bound variable
      (checkedBody , typeOfBody) <-
        addToBoundCtx (nameOf binder, typeOfBinder, Nothing) $ inferExpr body

      let t' = Pi insertedAnn (removeBinderName checkedBinder) typeOfBody
      return (Lam ann checkedBinder checkedBody , t')

    Builtin p op -> do
      return (Builtin p op, typeOfBuiltin p op)

    LVec ann elems -> do
      let p = provenanceOf ann

      -- Infer the type for each element in the list
      elemTypePairs <- traverse inferExpr elems
      -- Insert any implicit arguments for each element in the list to try and
      -- standardise the types
      elemTypePairs' <- traverse (uncurry $ insertNonExplicitArgs p) elemTypePairs
      let (checkedElems, typesOfElems) = unzip elemTypePairs'

      -- Create the new type.
      -- Roughly [x1, ..., xn] has type
      --  forall {tElem} {{TypesEqual tElem [t1, ..., tn]}} . Vector tElem n
      let liftedTypesOfElems = liftFreeDBIndices 3 <$> typesOfElems
      let typesOfElemsSeq = mkList p (TypeUniverse p 0) liftedTypesOfElems
      let tc = AlmostEqualConstraint
      let elemsTC tElem = BuiltinTypeClass p tc (ExplicitArg p <$> [tElem, typesOfElemsSeq])
      let typeOfContainer =
            Pi p (ImplicitBinder p Nothing (TypeUniverse p 0)) $
              Pi p (IrrelevantInstanceBinder p Nothing (elemsTC (Var p (Bound 0)))) $
                VectorType p (Var p (Bound 1)) (NatLiteral p (length elems))

      -- Return the result
      return (LVec ann checkedElems, typeOfContainer)

      -- TODO re-enable once we have the universe solver up and running.
      {-
      (checkedTypeClass, typeClassType) <- inferExpr typeClass
      unify ann typeClassType (TypeUniverse (inserted ann) 0)
      return (PrimDict ann checkedTypeClass, checkedTypeClass)
      -}

  showInferExit res
  return res

inferLiteral :: Provenance -> Literal -> (CheckedExpr, CheckedType)
inferLiteral p l = (Literal p l, typeOfLiteral p l)

-- | Takes the expected type of a function and the user-provided arguments
-- and traverses through checking each argument type against the type of the
-- matching pi binder and inserting any required implicit/instance arguments.
-- Returns the type of the function when applied to the full list of arguments
-- (including inserted arguments) and that list of arguments.
inferArgs :: TCM m
          => Provenance     -- Provenance of the function
          -> CheckedType    -- Type of the function
          -> [UncheckedArg] -- User-provided arguments of the function
          -> m (CheckedType, [CheckedArg])
inferArgs p (Pi _ binder resultType) (arg : args)
  | visibilityOf binder == visibilityOf arg = do
    let binderType = typeOf binder

    checkedArgExpr <- if isInstance binder
      -- If the argument is an instance argument then add it to the set
      -- of constraints. This is needed, even when the instance is already
      -- present, as solving it may produce information needed to solve
      -- auxiliary metas elsewhere.
      then createInstanceMetaAndAddConstraint p binder
      -- Check the type of the argument.
      else checkExpr binderType (argExpr arg)

    -- Substitute argument in `resultType`
    let updatedResultType = checkedArgExpr `substInto` resultType
    let newArg = replaceArgExpr checkedArgExpr arg

    -- Recurse into the list of args
    (typeAfterApplication, checkedArgs) <- inferArgs p updatedResultType args

    -- Return the appropriately annotated type with its inferred kind.
    return (typeAfterApplication, newArg : checkedArgs)

  | visibilityOf binder == Explicit = do
    -- Then we're expecting an explicit arg but have a non-explicit arg
    -- so panic
    ctx <- getBoundCtx
    throwError $ MissingExplicitArg (boundContextOf ctx) arg (typeOf binder)

-- This case handles either
-- `visibilityOf binder /= Explicit` and (`visibilityOf binder /= visibilityOf arg` or args == [])
inferArgs p (Pi _ binder resultType) args
  | visibilityOf binder /= Explicit = do
    logDebug MaxDetail ("insert-arg" <+> pretty (visibilityOf binder) <+> prettyVerbose (typeOf binder))
    let ann = inserted $ provenanceOf binder
    let binderType = typeOf binder

    (updateArgs, updatedResultType) <- do
        -- Check if the required argument is an instance argument
        metaExpr <- if isInstance binder
          then createInstanceMetaAndAddConstraint p binder
          else freshExprMeta p binderType =<< getBoundCtx

        -- Substitute meta-variable in tRes
        let updatedResultType = metaExpr `substInto` resultType

        -- Create a new argument
        let newArg = Arg ann (visibilityOf binder) (relevanceOf binder) metaExpr

        return ((newArg:), updatedResultType)

    -- Recurse into the list of args
    (typeAfterApplication, checkedArgs) <- inferArgs p updatedResultType args

    -- Return the appropriately annotated type with its inferred kind.
    return (typeAfterApplication, updateArgs checkedArgs)

inferArgs _p functionType [] = return (functionType, [])

inferArgs p functionType args = do
  ctx <- getBoundCtx
  let ann = inserted p
  let mkRes = [Endo $ \tRes -> pi (visibilityOf arg) (relevanceOf arg) (tHole ("arg" <> pack (show i))) (const tRes)
              | (i, arg) <- zip [0::Int ..] args]
  let expectedType = fromDSL ann (appEndo (mconcat mkRes) (tHole "res"))
  throwError $ TypeMismatch p (boundContextOf ctx) functionType expectedType

-- |Takes a function and its arguments, inserts any needed implicits
-- or instance arguments and then returns the function applied to the full
-- list of arguments as well as the result type.
inferApp :: TCM m
         => Provenance
         -> CheckedExpr
         -> CheckedType
         -> [UncheckedArg]
         -> m (CheckedExpr, CheckedType)
inferApp ann fun funType args = do
  (appliedFunType, checkedArgs) <- inferArgs (provenanceOf fun) funType args
  varCtx <- getVariableCtx
  normAppliedFunType <- whnfExprWithMetas varCtx appliedFunType
  return (normAppList ann fun checkedArgs, normAppliedFunType)

insertNonExplicitArgs :: TCM m
                      => Provenance
                      -> CheckedExpr
                      -> CheckedType
                      -> m (CheckedExpr, CheckedType)
insertNonExplicitArgs ann checkedExpr actualType = inferApp ann checkedExpr actualType []

createInstanceMetaAndAddConstraint :: TCM m => Provenance -> CheckedBinder -> m CheckedExpr
createInstanceMetaAndAddConstraint p binder = do
  let binderType = typeOf binder
  m <- freshTypeClassPlacementMeta p binderType
  ctx <- getVariableCtx
  addTypeClassConstraint ctx m binderType
  return $ Meta p m

--------------------------------------------------------------------------------
-- Typing of literals and builtins

-- | Return the type of the provided literal,
typeOfLiteral :: Provenance -> Literal -> CheckedType
typeOfLiteral ann l = fromDSL ann $ case l of
  LUnit      -> tUnit
  LBool _    -> tAnnBool constant unquantified
  LIndex n _ -> tIndex (natLit n)
  LNat{}     -> tNat
  LInt{}     -> tInt
  LRat{}     -> tAnnRat constant

typeOfTypeClassOp :: TypeClassOp -> DSLExpr
typeOfTypeClassOp b = case b of
  NotTC     -> typeOfTCOp1 hasNot
  ImpliesTC -> typeOfTCOp2 hasImplies
  AndTC     -> typeOfTCOp2 hasAnd
  OrTC      -> typeOfTCOp2 hasOr

  NegTC -> typeOfTCOp1 hasNeg
  AddTC -> typeOfTCOp2 hasAdd
  SubTC -> typeOfTCOp2 hasSub
  MulTC -> typeOfTCOp2 hasMul
  DivTC -> typeOfTCOp2 hasDiv

  EqualsTC op -> typeOfTCOp2 $ hasEq op
  OrderTC  op -> typeOfTCOp2 $ hasOrd op

  FromNatTC n -> forall type0 $ \t -> hasNatLits n t ~~~> typeOfFromNat n t
  FromRatTC   -> forall type0 $ \t -> hasRatLits t   ~~~> typeOfFromRat t
  FromVecTC n ->
    forall type0 $ \t ->
      forall type0 $ \e ->
        hasVecLits n e t  ~~~> tVector e (natLit n) ~> t

  MapTC  -> developerError "Unsure about type of MapTC"
  FoldTC -> typeOfFold

  QuantifierTC   q -> typeOfQuantifier   q
  QuantifierInTC q -> typeOfQuantifierIn q

-- | Return the type of the provided builtin.
typeOfBuiltin :: Provenance -> Builtin -> CheckedType
typeOfBuiltin ann b = fromDSL ann $ case b of
  -- Auxillary types
  Polarity{}    -> tPol
  Linearity{}   -> tLin

  -- Type classes
  TypeClass   tc -> typeOfTypeClass tc
  TypeClassOp tc -> typeOfTypeClassOp tc

  -- Types
  Unit   -> type0
  Nat    -> type0
  Int    -> type0
  Rat    -> tLin .~~> type0
  Bool   -> tLin .~~> tPol .~~> type0
  List   -> type0 ~> type0
  Vector -> type0 ~> tNat ~> type0
  Tensor -> type0 ~> tList tNat ~> type0
  Index  -> tNat ~> type0

  -- Boolean operations
  Not ->
    forallIrrelevant tLin $ \l ->
      forallIrrelevant tPol $ \p1 ->
        forallIrrelevant tPol $ \p2 ->
          negPolarity p1 p2 .~~~> tAnnBool l p1 ~> tAnnBool l p2

  Implies -> typeOfBoolOp2 maxLinearity impliesPolarity
  And     -> typeOfBoolOp2 maxLinearity maxPolarity
  Or      -> typeOfBoolOp2 maxLinearity maxPolarity
  If      -> typeOfIf

  -- Arithmetic operations
  Neg dom -> case dom of
    NegInt -> tInt ~> tInt
    NegRat -> tRat ~> tRat

  Add dom -> case dom of
    AddNat -> tNat ~> tNat ~> tNat
    AddInt -> tInt ~> tInt ~> tInt
    AddRat -> forallLinearityTriples $ \l1 l2 l3 ->
                maxLinearity l1 l2 l3 .~~~>
                tAnnRat l1 ~> tAnnRat l2 ~> tAnnRat l3

  Sub dom -> case dom of
    SubInt -> tInt ~> tInt ~> tInt
    SubRat -> forallLinearityTriples $ \l1 l2 l3 ->
                maxLinearity l1 l2 l3 .~~~>
                tAnnRat l1 ~> tAnnRat l2 ~> tAnnRat l3

  Mul dom -> case dom of
    MulNat -> tNat ~> tNat ~> tNat
    MulInt -> tInt ~> tInt ~> tInt
    MulRat -> forallLinearityTriples $ \l1 l2 l3 ->
                mulLinearity l1 l2 l3 .~~~>
                tAnnRat l1 ~> tAnnRat l2 ~> tAnnRat l3

  Div dom -> case dom of
    DivRat -> forallLinearityTriples $ \l1 l2 l3 ->
                mulLinearity l1 l2 l3 .~~~>
                tAnnRat l1 ~> tAnnRat l2 ~> tAnnRat l3

  -- Comparisons
  Equals dom op -> typeOfEquals dom op
  Order  dom op -> typeOfOrder dom op

  -- Conversion functions
  FromNat n dom -> case dom of
    FromNatToIndex -> forall tNat $ \s -> typeOfFromNat n (tIndex s)
    FromNatToNat   -> typeOfFromNat n tNat
    FromNatToInt   -> typeOfFromNat n tInt
    FromNatToRat   -> typeOfFromNat n (tAnnRat constant)

  FromRat dom -> case dom of
    FromRatToRat -> typeOfFromRat (tAnnRat constant)

  FromVec n dom -> case dom of
    FromVecToList -> forall type0 $ \t -> tVector t (natLit n) ~> tList t
    FromVecToVec  -> forall type0 $ \t -> tVector t (natLit n) ~> tVector t (natLit n)

  -- Container functions
  Map dom -> case dom of
    MapList   -> typeOfMap tList
    MapVector -> forall tNat $ \n -> typeOfMap (`tVector` n)

  Fold dom -> case dom of
    FoldList   -> forall type0 $ \tElem ->
                    forall type0 $ \tRes ->
                      (tElem ~> tRes ~> tRes) ~> tRes ~> tList tElem ~> tRes
    FoldVector -> forall type0 $ \tElem ->
                    forall type0 $ \tRes ->
                      forall tNat $ \dim ->
                        (tElem ~> tRes ~> tRes) ~> tRes ~> tVector tElem dim ~> tRes

  Nil     -> typeOfNil
  Cons    -> typeOfCons

  At      -> typeOfAt

  Foreach -> typeOfForeach

typeOfTypeClass :: TypeClass -> DSLExpr
typeOfTypeClass tc = case tc of
  HasEq{}            -> type0 ~> type0 ~> type0 ~> type0
  HasOrd{}           -> type0 ~> type0 ~> type0 ~> type0
  HasNot             -> type0 ~> type0 ~> type0
  HasAnd             -> type0 ~> type0 ~> type0 ~> type0
  HasOr              -> type0 ~> type0 ~> type0 ~> type0
  HasImplies         -> type0 ~> type0 ~> type0 ~> type0
  HasQuantifier{}    -> type0 ~> type0 ~> type0 ~> type0
  HasAdd             -> type0 ~> type0 ~> type0 ~> type0
  HasSub             -> type0 ~> type0 ~> type0 ~> type0
  HasMul             -> type0 ~> type0 ~> type0 ~> type0
  HasDiv             -> type0 ~> type0 ~> type0 ~> type0
  HasNeg             -> type0 ~> type0 ~> type0
  HasFold            -> type0 ~> type0 ~> type0
  HasQuantifierIn{}  -> type0 ~> type0 ~> type0

  HasNatLits{} -> type0 ~> type0
  HasRatLits   -> type0 ~> type0
  HasVecLits{} -> type0 ~> type0 ~> type0

  MaxLinearity -> tLin ~> tLin ~> tLin ~> type0
  MulLinearity -> tLin ~> tLin ~> tLin ~> type0

  NegPolarity{}     -> tPol ~> tPol ~> type0
  AddPolarity{}     -> tPol ~> tPol ~> type0
  MaxPolarity       -> tPol ~> tPol ~> tPol ~> type0
  EqPolarity{}      -> tPol ~> tPol ~> tPol ~> type0
  ImpliesPolarity{} -> tPol ~> tPol ~> tPol ~> type0

  AlmostEqualConstraint{} -> forall type0 $ \t -> t ~> tList t ~> type0
  NatInDomainConstraint{} -> forall type0 $ \t -> t ~> type0

typeOfBoolOp2 :: (DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr)
              -> (DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr)
              -> DSLExpr
typeOfBoolOp2 linearityConstraint polarityConstraint =
  forallLinearityTriples $ \l1 l2 l3 ->
    forallPolarityTriples $ \p1 p2 p3 ->
      linearityConstraint l1 l2 l3 .~~~>
      polarityConstraint  p1 p2 p3 .~~~>
      tAnnBool l1 p1 ~> tAnnBool l2 p2 ~> tAnnBool l3 p3

typeOfIf :: DSLExpr
typeOfIf =
  forall type0 $ \t ->
    forallIrrelevant tLin $ \lin ->
      tAnnBool lin unquantified ~> t ~> t ~> t

typeOfEquals :: EqualityDomain -> EqualityOp -> DSLExpr
typeOfEquals domain _op = case domain of
  EqIndex{} ->
    forall tNat $ \n1 ->
      forall tNat $ \n2 ->
        tIndex n1 ~> tIndex n2 ~> tAnnBool constant unquantified

  EqNat{} ->
    tNat ~> tNat ~> tAnnBool constant unquantified

  EqInt{} ->
    tInt ~> tInt ~> tAnnBool constant unquantified

  EqRat{} ->
    forallLinearityTriples $ \l1 l2 l3 ->
      maxLinearity l1 l2 l3 .~~~>
      tAnnRat l1 ~> tAnnRat l2 ~> tAnnBool l3 unquantified

typeOfOrder :: OrderDomain -> OrderOp -> DSLExpr
typeOfOrder domain _op = case domain of
  OrderIndex{} ->
    forall tNat $ \n1 ->
      forall tNat $ \n2 ->
        tIndex n1 ~> tIndex n2 ~> tAnnBool constant unquantified

  OrderNat{} ->
    tNat ~> tNat ~> tAnnBool constant unquantified

  OrderInt{} ->
    tInt ~> tInt ~> tAnnBool constant unquantified

  OrderRat{} ->
    forallLinearityTriples $ \l1 l2 l3 ->
      maxLinearity l1 l2 l3 .~~~> tAnnRat l1 ~> tAnnRat l2 ~> tAnnBool l3 unquantified

typeOfTCOp1 :: (DSLExpr -> DSLExpr -> DSLExpr) -> DSLExpr
typeOfTCOp1 constraint =
  forall type0 $ \t1 ->
    forall type0 $ \t2 ->
      constraint t1 t2 ~~~> t1 ~> t2

typeOfTCOp2 :: (DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr) -> DSLExpr
typeOfTCOp2 constraint =
  forall type0 $ \t1 ->
    forall type0 $ \t2 ->
      forall type0 $ \t3 ->
        constraint t1 t2 t3 ~~~> t1 ~> t2 ~> t3

typeOfForeach :: DSLExpr
typeOfForeach =
  forall type0 $ \tRes ->
    forall tNat $ \d ->
      (tIndex d ~> tRes) ~> tVector tRes d

typeOfNil :: DSLExpr
typeOfNil =
  forall type0 $ \tElem ->
    tList tElem

typeOfCons :: DSLExpr
typeOfCons =
  forall type0 $ \tElem ->
    tElem ~> tList tElem ~> tList tElem

typeOfAt :: DSLExpr
typeOfAt =
  forall type0 $ \tElem ->
    forall tNat $ \tDim ->
      tVector tElem tDim ~> tIndex tDim ~> tElem

typeOfMap :: (DSLExpr -> DSLExpr) -> DSLExpr
typeOfMap tCont =
  forall type0 $ \tFrom ->
    forall type0 $ \tTo ->
      (tFrom ~> tTo) ~> tCont tFrom ~> tCont tTo

typeOfFold :: DSLExpr
typeOfFold =
  forall type0 $ \tElem ->
    forall type0 $ \tCont ->
      forall type0 $ \tRes ->
        hasFold tElem tCont ~~~> (tElem ~> tRes ~> tRes) ~> tRes ~> tCont ~> tRes

typeOfQuantifier :: Quantifier -> DSLExpr
typeOfQuantifier q =
  forall type0 $ \tDomain ->
    forall type0 $ \tBody ->
      forall type0 $ \tRes ->
        hasQuantifier q tDomain tBody tRes ~~~> (tDomain ~> tBody) ~> tRes

typeOfQuantifierIn :: Quantifier -> DSLExpr
typeOfQuantifierIn q =
  forall type0 $ \tElem ->
    forall type0 $ \tCont ->
      forall type0 $ \tRes ->
        hasQuantifierIn q tElem tCont tRes ~~~> (tElem ~> tRes) ~> tCont ~> tRes

typeOfFromNat :: Int -> DSLExpr -> DSLExpr
typeOfFromNat n t = natInDomainConstraint n t .~~~> tNat ~> t

typeOfFromRat :: DSLExpr -> DSLExpr
typeOfFromRat t = tAnnRat constant ~> t