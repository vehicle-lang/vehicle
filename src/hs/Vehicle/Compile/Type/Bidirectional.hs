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

--------------------------------------------------------------------------------
-- Bidirectional type-checking

-- Recurses through the expression, switching between check and infer modes.
-- Inserts meta-variables for missing implicit and instance arguments and
-- gathers the constraints over those meta-variables.

--------------------------------------------------------------------------------
-- The type-checking monad

-- | The type-checking monad
type TCM m =
  ( MonadLogger              m
  , MonadError  CompileError m
  , MonadState  MetaCtx      m
  , MonadReader VariableCtx  m
  )

runTCM :: MonadCompile m => ReaderT VariableCtx (StateT MetaCtx m) a -> m a
runTCM e = evalStateT (runReaderT e emptyVariableCtx) emptyMetaCtx

--------------------------------------------------------------------------------
-- Debug functions

showCheckEntry :: MonadLogger m => CheckedExpr -> UncheckedExpr -> m ()
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

showInferExit :: MonadLogger m => (CheckedExpr, CheckedExpr) -> m ()
showInferExit (e, t) = do
  decrCallDepth
  logDebug MaxDetail ("infer-exit " <+> prettyVerbose e <+> "->" <+> prettyVerbose t)

-------------------------------------------------------------------------------
-- Utility functions

removeBinderName :: CheckedBinder -> CheckedBinder
removeBinderName (Binder ann v _n t) = Binder ann v Nothing t

unify :: TCM m => Provenance -> CheckedExpr -> CheckedExpr -> m ()
unify p e1 e2 = do
  ctx <- getVariableCtx
  -- TODO calculate the most general unifier
  addUnificationConstraint p ctx e1 e2

--------------------------------------------------------------------------------
-- Checking

checkExpr :: TCM m
          => CheckedExpr   -- Type we're checking against
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
        checkedBody <- addToBoundCtx (nameOf lamBinder) checkedLamBinderType Nothing $ do
          -- Check if the type of the expression matches the expected result type.
          checkExpr resultType body

        let checkedLamBinder = replaceBinderType checkedLamBinderType lamBinder
        return $ Lam ann checkedLamBinder checkedBody

    (Pi _ piBinder resultType, e)
      | visibilityOf piBinder == Implicit || visibilityOf piBinder == Instance -> do
      -- Then eta-expand
      let ann = inserted $ provenanceOf piBinder
      let binderName = nameOf piBinder
      let binderType = typeOf piBinder
      let binderVis  = visibilityOf piBinder

      -- Add the pi-bound variable to the context
      checkedExpr <- addToBoundCtx binderName binderType Nothing $
        -- Check if the type of the expression matches the expected result type.
        checkExpr resultType (liftFreeDBIndices 1 e)

      -- Create a new binder mirroring the Pi binder expected
      let lamBinder = Binder ann binderVis binderName binderType

      -- Prepend a new lambda to the expression with the implicit binder
      return $ Lam ann lamBinder checkedExpr

    (_, Lam ann binder _) -> do
      ctx <- getBoundCtx
      let expected = fromDSL ann $ pi (visibilityOf binder) (tHole "a") (const (tHole "b"))
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
    (_, LSeq     ann _)     -> viaInfer ann expectedType expr
    (_, Ann      ann _ _)   -> viaInfer ann expectedType expr
    (_, PrimDict ann _)     -> viaInfer ann expectedType expr

  showCheckExit res
  return res

viaInfer :: TCM m => Provenance -> CheckedExpr -> UncheckedExpr -> m CheckedExpr
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
          -> m (CheckedExpr, CheckedExpr)
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
        addToBoundCtx (nameOf binder) checkedBinderType Nothing $ inferExpr resultType

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
        addToBoundCtx (nameOf binder) typeOfBoundExpr (Just checkedBoundExpr) $ inferExpr body

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
        addToBoundCtx (nameOf binder) typeOfBinder Nothing $ inferExpr body

      let t' = Pi insertedAnn (removeBinderName checkedBinder) typeOfBody
      return (Lam ann checkedBinder checkedBody , t')

    Builtin p op -> do
      return (Builtin p op, typeOfBuiltin p op)

    LSeq ann elems -> do
      let p = provenanceOf ann

      -- Infer the type for each element in the list
      elemTypePairs <- traverse inferExpr elems
      -- Insert any implicit arguments for each element in the list to try and
      -- standardise the types
      elemTypePairs' <- traverse (uncurry $ insertNonExplicitArgs p) elemTypePairs
      let (checkedElems, typesOfElems) = unzip elemTypePairs'

      -- Create the new type.
      -- Roughly [x1, ..., xn] has type
      --  forall {tElem} {tCont} {{HasConLits tElem tCont}} {{TypesEqual tElem [t1, ..., tn]}} . tCont
      let conLitsTC = HasConLitsOfSizeExpr p (length typesOfElems)

      let liftedTypesOfElems = liftFreeDBIndices 3 <$> typesOfElems
      let typesOfElemsSeq = mkList p (TypeUniverse p 0) liftedTypesOfElems

      let tc = TypesEqualModAuxiliaryAnnotations
      let elemsTC tElem = BuiltinTypeClass p tc (ExplicitArg p <$> [tElem, typesOfElemsSeq])
      let typeOfContainer =
            Pi p (ImplicitBinder p Nothing (TypeUniverse p 0)) $
              Pi p (ImplicitBinder p Nothing (TypeUniverse p 0)) $
                Pi p (InstanceBinder p Nothing (conLitsTC (Var p (Bound 1)) (Var p (Bound 0)))) $
                  Pi p (InstanceBinder p Nothing (elemsTC (Var p (Bound 2)))) $
                    Var p (Bound 2)

      -- Return the result
      return (LSeq ann checkedElems, typeOfContainer)

    PrimDict ann typeClass -> do
      return (PrimDict ann typeClass, typeClass)

      -- TODO re-enable once we have the universe solver up and running.
      {-
      (checkedTypeClass, typeClassType) <- inferExpr typeClass
      unify ann typeClassType (TypeUniverse (inserted ann) 0)
      return (PrimDict ann checkedTypeClass, checkedTypeClass)
      -}

  showInferExit res
  return res

inferLiteral :: Provenance -> Literal -> (CheckedExpr, CheckedExpr)
inferLiteral p l = (Literal p l, typeOfLiteral p l)

-- | Takes the expected type of a function and the user-provided arguments
-- and traverses through checking each argument type against the type of the
-- matching pi binder and inserting any required implicit/instance arguments.
-- Returns the type of the function when applied to the full list of arguments
-- (including inserted arguments) and that list of arguments.
inferArgs :: TCM m
          => Provenance     -- Provenance of the function
          -> CheckedExpr    -- Type of the function
          -> [UncheckedArg] -- User-provided arguments of the function
          -> m (CheckedExpr, [CheckedArg])
inferArgs p (Pi _ binder resultType) (arg : args)
  | visibilityOf binder == visibilityOf arg = do
    let binderType = typeOf binder

    -- Check the type of the argument.
    checkedArgExpr <- checkExpr binderType (argExpr arg)

    -- If the argument is an instance argument then add it to the set
    -- of constraints. This is needed, even when the instance is already
    -- present, as solving it may produce information needed to solve
    -- auxiliary metas elsewhere.
    when (visibilityOf binder == Instance) $ do
      varCtx <- getVariableCtx
      m <- freshTypeClassPlacementMeta p binderType
      addTypeClassConstraint varCtx m binderType

    -- Generate the new checked arg
    let checkedArg = replaceArgExpr checkedArgExpr arg

    -- Substitute argument in `resultType`
    let updatedResultType = checkedArgExpr `substInto` resultType

    -- Recurse into the list of args
    (typeAfterApplication, checkedArgs) <- inferArgs p updatedResultType args

    -- Return the appropriately annotated type with its inferred kind.
    return (typeAfterApplication, checkedArg : checkedArgs)

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
    let binderVis = visibilityOf binder
    let binderType = typeOf binder

    (updateArgs, updatedResultType) <- do

        -- Check if the required argument is a type-class
        metaExpr <- if binderVis == Instance then do
          -- Generate a new meta-variable for the argument
          meta <- freshTypeClassPlacementMeta p binderType
          ctx <- getVariableCtx
          addTypeClassConstraint ctx meta binderType
          return $ Meta ann meta
        else do
          boundCtx <- getBoundCtx
          freshExprMeta p binderType boundCtx

        let metaArg = Arg ann binderVis metaExpr
        -- Substitute meta-variable in tRes
        let updatedResultType = metaExpr `substInto` resultType

        return ((metaArg :), updatedResultType)

    -- Recurse into the list of args
    (typeAfterApplication, checkedArgs) <- inferArgs p updatedResultType args

    -- Return the appropriately annotated type with its inferred kind.
    return (typeAfterApplication, updateArgs checkedArgs)

inferArgs _p functionType [] = return (functionType, [])

inferArgs p functionType args = do
  ctx <- getBoundCtx
  let ann = inserted p
  let mkRes = [Endo $ \tRes -> pi (visibilityOf arg) (tHole ("arg" <> pack (show i))) (const tRes)
              | (i, arg) <- zip [0::Int ..] args]
  let expectedType = fromDSL ann (appEndo (mconcat mkRes) (tHole "res"))
  throwError $ TypeMismatch p (boundContextOf ctx) functionType expectedType

-- |Takes a function and its arguments, inserts any needed implicits
-- or instance arguments and then returns the function applied to the full
-- list of arguments as well as the result type.
inferApp :: TCM m
         => Provenance
         -> CheckedExpr
         -> CheckedExpr
         -> [UncheckedArg]
         -> m (CheckedExpr, CheckedExpr)
inferApp ann fun funType args = do
  (appliedFunType, checkedArgs) <- inferArgs (provenanceOf fun) funType args
  varCtx <- getVariableCtx
  normAppliedFunType <- whnfExprWithMetas varCtx appliedFunType
  return (normAppList ann fun checkedArgs, normAppliedFunType)

insertNonExplicitArgs :: TCM m
                      => Provenance
                      -> CheckedExpr
                      -> CheckedExpr
                      -> m (CheckedExpr, CheckedExpr)
insertNonExplicitArgs ann checkedExpr actualType = inferApp ann checkedExpr actualType []

--------------------------------------------------------------------------------
-- Typing of literals and builtins

-- | Return the type of the provided literal,
typeOfLiteral :: Provenance -> Literal -> CheckedExpr
typeOfLiteral ann l = fromDSL ann $ case l of
  LUnit   -> tUnit
  LNat  n -> forall type0 $ \t -> hasNatLitsUpTo n t ~~~> t
  LInt  _ -> forall type0 $ \t -> hasIntLits t ~~~> t
  LRat  _ -> forall type0 $ \t -> hasRatLits t ~~~> t
  LBool _ -> tAnnBool constant unquantified

-- | Return the type of the provided builtin.
typeOfBuiltin :: Provenance -> Builtin -> CheckedExpr
typeOfBuiltin ann b = fromDSL ann $ case b of
  Polarity{}    -> tPol
  Linearity{}   -> tLin

  TypeClass tc -> typeOfTypeClass tc

  Unit -> type0
  Nat  -> type0
  Int  -> type0
  -- Rat gets an extra linearity argument during type-checking.
  Rat -> tLin ~~> type0
  -- Bool gets extra linearity and polarity arguments during type-checking.
  Bool -> tLin ~~> tPol ~~> type0

  List   -> type0 ~> type0
  Tensor -> type0 ~> tList tNat ~> type0
  Index  -> tNat ~> type0

  If              -> typeOfIf

  Equality eq     -> typeOfOp2 $ hasEq eq
  Order    ord    -> typeOfOp2 $ hasOrd ord
  Not             -> typeOfOp1 hasNot
  Implies         -> typeOfOp2 hasImplies
  And             -> typeOfOp2 hasAnd
  Or              -> typeOfOp2 hasOr
  Neg             -> typeOfOp1 hasNeg
  Add             -> typeOfOp2 hasAdd
  Sub             -> typeOfOp2 hasSub
  Mul             -> typeOfOp2 hasMul
  Div             -> typeOfOp2 hasDiv

  Cons -> typeOfCons
  At   -> typeOfAtOp
  Map  -> typeOfMapOp
  Fold -> typeOfFoldOp

  Quant q   -> typeOfQuantifier q
  QuantIn q -> typeOfQuantifierIn q

  Foreach   -> typeOfForeach
  ForeachIn -> typeOfForeachIn

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

  HasNatLitsUpTo{}   -> type0 ~> type0
  HasIntLits         -> type0 ~> type0
  HasRatLits         -> type0 ~> type0
  HasConLitsOfSize{} -> type0 ~> type0 ~> type0

  MaxLinearity -> tLin ~> tLin ~> tLin ~> type0
  MulLinearity -> tLin ~> tLin ~> tLin ~> type0

  NegPolarity{}     -> tPol ~> tPol ~> type0
  AddPolarity{}     -> tPol ~> tPol ~> type0
  MaxPolarity       -> tPol ~> tPol ~> tPol ~> type0
  EqPolarity{}      -> tPol ~> tPol ~> tPol ~> type0
  ImpliesPolarity{} -> tPol ~> tPol ~> tPol ~> type0

  TypesEqualModAuxiliaryAnnotations{} ->
    forall type0 $ \t -> t ~> tList t ~> type0

typeOfIf :: DSLExpr
typeOfIf =
  forall type0 $ \t ->
    forall tLin $ \lin ->
      tAnnBool lin unquantified ~> t ~> t ~> t

typeOfOp1 :: (DSLExpr -> DSLExpr -> DSLExpr) -> DSLExpr
typeOfOp1 constraint =
  forall type0 $ \t1 ->
    forall type0 $ \t2 ->
      constraint t1 t2 ~~~> t1 ~> t2

typeOfOp2 :: (DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr) -> DSLExpr
typeOfOp2 constraint =
  forall type0 $ \t1 ->
    forall type0 $ \t2 ->
      forall type0 $ \t3 ->
        constraint t1 t2 t3 ~~~> t1 ~> t2 ~> t3

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

typeOfForeach :: DSLExpr
typeOfForeach =
  forall tNat $ \d ->
    forall type0 $ \tRes ->
      (tIndex d ~> tRes) ~> tTensor tRes (cons tNat d (nil tNat (tList tNat)))

typeOfForeachIn :: DSLExpr
typeOfForeachIn =
  forall tNat $ \d ->
    forall type0 $ \tElem ->
      forall tLin $ \lin ->
        forall tPol $ \pol ->
          -- This is a hack. Need to think about the multi-dimensional case
          -- more carefully.
          let ds = lseq tNat (tList tNat) [d] in
          (tElem ~> tAnnBool lin pol) ~> tTensor tElem ds ~> tTensor (tAnnBool lin pol) ds

typeOfCons :: DSLExpr
typeOfCons =
  forall type0 $ \tElem ->
    tElem ~> tList tElem ~> tList tElem

typeOfAtOp :: DSLExpr
typeOfAtOp =
  forall type0 $ \tElem ->
    forall tNat $ \tDim ->
      forall (tList tNat) $ \tDims ->
        tTensor tElem (cons tNat tDim tDims) ~> tIndex tDim ~> tTensor tElem tDims

-- TODO generalise these to tensors etc. (remember to do mkMap' in utils as well)
typeOfMapOp :: DSLExpr
typeOfMapOp =
  forall type0 $ \tFrom ->
    forall type0 $ \tTo ->
      (tFrom ~> tTo) ~> tList tFrom ~> tList tTo

typeOfFoldOp :: DSLExpr
typeOfFoldOp =
  forall type0 $ \tElem ->
    forall type0 $ \tCont ->
      forall type0 $ \tRes ->
        hasFold tElem tCont ~~~> (tElem ~> tRes ~> tRes) ~> tRes ~> tCont ~> tRes