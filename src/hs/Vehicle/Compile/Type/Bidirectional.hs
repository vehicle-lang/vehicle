
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
import Data.Foldable (foldrM)
import Data.Map qualified as Map
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Monoid (Endo(..), appEndo)
import Data.Text (pack)

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Language.DSL
import Vehicle.Language.Print
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.WeakHeadNormalForm

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

unify :: TCM m => Provenance -> CheckedExpr -> CheckedExpr -> m CheckedExpr
unify p e1 e2 = do
  ctx <- getVariableCtx
  addUnificationConstraint p ctx e1 e2
  -- TODO calculate the most general unifier
  return e1

freshMeta :: TCM m => Provenance -> CheckedExpr -> m (Meta, CheckedExpr)
freshMeta p metaType = freshMetaWith p metaType =<< getBoundCtx

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
        checkedLamBinderType <- checkExpr (Type (inserted ann) 0) (typeOf lamBinder)

        -- Unify the result with the type of the pi binder.
        _ <- unify (provenanceOf ann) (typeOf piBinder) checkedLamBinderType

        -- Add bound variable to context
        checkedBody <- addToBoundCtx (nameOf lamBinder) checkedLamBinderType Nothing $ do
          -- Check if the type of the expression matches the expected result type.
          checkExpr resultType body

        let checkedLamBinder = replaceBinderType checkedLamBinderType lamBinder
        return $ Lam ann checkedLamBinder checkedBody

    (Pi _ binder resultType, e) -> do
      let ann = inserted $ provenanceOf binder

      -- Add the binder to the context
      checkedExpr <- addToBoundCtx (nameOf binder) (typeOf binder) Nothing $
        -- Check if the type of the expression matches the expected result type.
        checkExpr resultType (liftFreeDBIndices 1 e)

      -- Create a new binder mirroring the implicit Pi binder expected
      let lamBinder = Binder ann Implicit (nameOf binder) (typeOf binder)

      -- Prepend a new lambda to the expression with the implicit binder
      return $ Lam ann lamBinder checkedExpr

    (_, Lam ann binder _) -> do
      ctx <- getBoundCtx
      let expected = fromDSL ann $ pi (visibilityOf binder) (tHole "a") (const (tHole "b"))
      throwError $ TypeMismatch (provenanceOf ann) ctx expectedType expected

    (_, Hole ann _name) -> do
      -- Replace the hole with meta-variable. Throws away the expected type.
      -- Can we use it somehow?
      -- NOTE, different uses of the same hole name will be interpreted as
      -- different meta-variables.
      (_, meta) <- freshMeta (provenanceOf ann) expectedType
      return meta

    (_, Type     ann _)     -> viaInfer ann expectedType expr
    (_, Meta     ann _)     -> viaInfer ann expectedType expr
    (_, App      ann _ _)   -> viaInfer ann expectedType expr
    (_, Pi       ann _ _)   -> viaInfer ann expectedType expr
    (_, Builtin  ann _)     -> viaInfer ann expectedType expr
    (_, Var      ann _)     -> viaInfer ann expectedType expr
    (_, Let      ann _ _ _) -> viaInfer ann expectedType expr
    (_, Literal  ann _)     -> viaInfer ann expectedType expr
    (_, LSeq     ann _ _)   -> viaInfer ann expectedType expr
    (_, Ann      ann _ _)   -> viaInfer ann expectedType expr
    (_, PrimDict ann _)     -> viaInfer ann expectedType expr

  showCheckExit res
  return res

viaInfer :: TCM m => CheckedAnn -> CheckedExpr -> UncheckedExpr -> m CheckedExpr
viaInfer ann expectedType e = do
  -- Switch to inference mode
  (checkedExpr, actualType) <- inferExpr e
  -- Insert any needed implicit or instance arguments
  (appliedCheckedExpr, resultType) <- inferApp ann checkedExpr actualType []
  -- Assert the expected and the actual types are equal
  _t <- unify (provenanceOf ann) expectedType resultType
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
    Type ann l ->
      return (e , Type (inserted ann) (l + 1))

    Meta _ m -> compilerDeveloperError $
      "Trying to infer the type of a meta-variable" <+> pretty m

    Hole ann _name -> do
      -- Replace the hole with meta-variable.
      -- NOTE, different uses of the same hole name will be interpreted as different meta-variables.
      (_, typeMeta) <- freshMeta (provenanceOf ann) (Type ann 0)
      (_, exprMeta) <- freshMeta (provenanceOf ann) typeMeta
      return (exprMeta, typeMeta)

    Ann ann expr exprType -> do
      (checkedExprType, exprTypeType) <- inferExpr exprType
      _ <- unify (provenanceOf ann) exprTypeType (Type (inserted ann) 0)
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
          "up variable in context" <+> prettyVerbose (ctxNames ctx) <+> "at" <+> pretty (provenanceOf ann)

    Var ann (Free ident) -> do
      -- Lookup the type of the declaration variable in the context.
      ctx <- getDeclCtx
      case Map.lookup ident ctx of
        Just (checkedType, _) -> return (Var ann (Free ident), checkedType)
        -- This should have been caught during scope checking
        Nothing -> compilerDeveloperError $
          "Declaration'" <+> pretty ident <+> "'not found when" <+>
          "looking up variable in context" <+> pretty (Map.keys ctx) <+> "at" <+> pretty (provenanceOf ann)

    Let ann boundExpr binder body -> do
      -- Check the type of the bound expression against the provided type
      (typeOfBoundExpr, typeOfBoundExprType) <- inferExpr (typeOf binder)
      _ <- unify ann typeOfBoundExprType (Type (inserted ann) 0)
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
      _ <- unify ann typeOfBinderType (Type insertedAnn 0)
      let checkedBinder = replaceBinderType typeOfBinder binder

      -- Update the context with the bound variable
      (checkedBody , typeOfBody) <-
        addToBoundCtx (nameOf binder) typeOfBinder Nothing $ inferExpr body

      let t' = Pi insertedAnn (removeBinderName checkedBinder) typeOfBody
      return (Lam ann checkedBinder checkedBody , t')

    Builtin p op -> do
      return (Builtin p op, typeOfBuiltin p op)

    LSeq ann dict elems -> do
      let p = provenanceOf ann
      ctx <- getVariableCtx

      -- Infer the type for each element in the list
      (checkedElems, typesOfElems) <- unzip <$> traverse inferExpr elems

      -- Generate a fresh meta variable for the type of elements in the list, e.g. Int
      (_, typeOfElems) <- freshMeta p (Type ann 0)
      -- Unify the types of all the elements in the sequence
      _ <- foldrM (unify p) typeOfElems typesOfElems

      -- Generate a meta-variable for the applied container type, e.g. List Int
      (_, typeOfContainer) <- freshMeta p (Type ann 0)
      let typeOfDict = HasConLitsOfSizeExpr ann (length elems) typeOfElems typeOfContainer

      -- Check the type of the dict
      checkedDict <- if not (isHole dict)
        then checkExpr typeOfDict dict
        else do
          (meta, checkedDict) <- freshMeta p typeOfDict
          addTypeClassConstraint ctx meta typeOfDict
          return checkedDict

      -- Return the result
      return (LSeq ann checkedDict checkedElems, typeOfContainer)

    PrimDict ann typeClass -> do
      (checkedTypeClass, typeClassType) <- inferExpr typeClass
      _ <- unify (provenanceOf ann) typeClassType (Type (inserted ann) 0)
      return (PrimDict ann checkedTypeClass, checkedTypeClass)

  showInferExit res
  return res

inferLiteral :: UncheckedAnn -> Literal -> (CheckedExpr, CheckedExpr)
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
    -- Check the type of the argument.
    checkedArgExpr <- checkExpr (typeOf binder) (argExpr arg)

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
    throwError $ MissingExplicitArg ctx arg (typeOf binder)

-- This case handles either
-- `visibilityOf binder /= Explicit` and (`visibilityOf binder /= visibilityOf arg` or args == [])
inferArgs p (Pi _ binder resultType) args
  | visibilityOf binder /= Explicit = do
    logDebug MaxDetail ("insert-arg" <+> pretty (visibilityOf binder) <+> prettyVerbose (typeOf binder))
    let ann = inserted $ provenanceOf binder
    let binderVis = visibilityOf binder
    let binderType = typeOf binder

    (updateArgs, updatedResultType) <-
      if binderVis == Instance && isAuxiliaryTypeClass binderType then do
        -- Auxiliary type classes have no associated computational content,
        -- so no need to generate a new meta so that the solution that be
        -- inserted after solving the constraints.
        ctx <- getVariableCtx
        addAuxiliaryConstraint ctx binderType
        return (id, resultType)

      else do
        -- Generate a new meta-variable for the argument
        (meta, metaExpr) <- freshMeta p binderType
        let metaArg = Arg ann binderVis metaExpr

        -- Check if the required argument is a type-class
        when (binderVis == Instance) $ do
          ctx <- getVariableCtx
          addTypeClassConstraint ctx meta binderType

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
  throwError $ TypeMismatch p ctx functionType expectedType

-- |Takes a function and its arguments, inserts any needed implicits
-- or instance arguments and then returns the function applied to the full
-- list of arguments as well as the result type.
inferApp :: TCM m
         => CheckedAnn
         -> CheckedExpr
         -> CheckedExpr
         -> [UncheckedArg]
         -> m (CheckedExpr, CheckedExpr)
inferApp ann fun funType args = do
  (appliedFunType, checkedArgs) <- inferArgs (provenanceOf fun) funType args
  varCtx <- getVariableCtx
  normAppliedFunType <- whnfWithMetas varCtx appliedFunType
  return (normAppList ann fun checkedArgs, normAppliedFunType)

--------------------------------------------------------------------------------
-- Typing of literals and builtins

-- | Return the type of the provided literal,
typeOfLiteral :: CheckedAnn -> Literal -> CheckedExpr
typeOfLiteral ann l = fromDSL ann $ case l of
  LNat  n -> forall type0 $ \t -> hasNatLitsUpTo n t ~~~> t
  LInt  _ -> forall type0 $ \t -> hasIntLits t ~~~> t
  LRat  _ -> forall type0 $ \t -> hasRatLits t ~~~> t
  LBool _ -> tBool unquantified

-- | Return the type of the provided builtin.
typeOfBuiltin :: CheckedAnn -> Builtin -> CheckedExpr
typeOfBuiltin ann b = fromDSL ann $ case b of
  Auxiliary  -> type0
  Polarity{} -> tAux

  PolarityTypeClass HasNot          -> tAux ~> tAux ~> tAux
  PolarityTypeClass HasAndOr        -> tAux ~> tAux ~> tAux ~> tAux
  PolarityTypeClass HasImpl         -> tAux ~> tAux ~> tAux ~> tAux
  PolarityTypeClass HasQuantifier{} -> type0 ~> tAux ~> tAux ~> tAux

  TypeClass HasEq              -> type0 ~> type0
  TypeClass HasOrd             -> type0 ~> type0
  TypeClass HasAdd             -> type0 ~> type0
  TypeClass HasSub             -> type0 ~> type0
  TypeClass HasMul             -> type0 ~> type0
  TypeClass HasDiv             -> type0 ~> type0
  TypeClass HasNeg             -> type0 ~> type0
  TypeClass HasConOps          -> type0 ~> type0 ~> type0
  TypeClass HasNatLitsUpTo{}   -> type0 ~> type0
  TypeClass HasIntLits         -> type0 ~> type0
  TypeClass HasRatLits         -> type0 ~> type0
  TypeClass HasConLitsOfSize{} -> type0 ~> type0 ~> type0

  -- The Bool type takes an implicit polarity argument
  Bool                      -> tAux ~~> type0

  NumericType   _           -> type0
  ContainerType List        -> type0 ~> type0
  ContainerType Tensor      -> type0 ~> tList tNat ~> type0
  Index                     -> tNat ~> type0

  If              -> typeOfIf
  Not             -> typeOfNot
  BooleanOp2 Impl -> typeOfImpl
  BooleanOp2 And  -> typeOfAndOr
  BooleanOp2 Or   -> typeOfAndOr

  NumericOp2 Add -> typeOfNumOp2 hasAdd
  NumericOp2 Sub -> typeOfNumOp2 hasSub
  NumericOp2 Mul -> typeOfNumOp2 hasMul
  NumericOp2 Div -> typeOfNumOp2 hasDiv
  Neg            -> typeOfNumOp1 hasNeg

  Equality _ -> typeOfEqualityOp
  Order    _ -> typeOfComparisonOp

  Cons -> typeOfCons
  At   -> typeOfAtOp
  Map  -> typeOfMapOp
  Fold -> typeOfFoldOp

  Quant q   -> typeOfQuantifier q
  QuantIn _ -> typeOfQuantifierIn

  Foreach   -> typeOfForeach
  ForeachIn -> typeOfForeachIn

typeOfIf :: DSLExpr
typeOfIf =
  forall type0 $ \t ->
    tBool unquantified ~> t ~> t ~> t

typeOfEqualityOp :: DSLExpr
typeOfEqualityOp =
  forall type0 $ \t ->
    hasEq t ~~~> t ~> t ~> tBool unquantified

typeOfComparisonOp :: DSLExpr
typeOfComparisonOp =
  forall type0 $ \t ->
    hasOrd t ~~~> t ~> t ~> tBool unquantified

typeOfNot :: DSLExpr
typeOfNot =
  forall tAux $ \pol1 ->
    forall tAux $ \pol2 ->
      hasNot pol1 pol2 ~~~> tBool pol1 ~> tBool pol2

typeOfAndOr :: DSLExpr
typeOfAndOr =
  forall tAux $ \pol1 ->
    forall tAux $ \pol2 ->
      forall tAux $ \pol3 ->
        hasAndOr pol1 pol2 pol3 ~~~> tBool pol1 ~> tBool pol2 ~> tBool pol3

typeOfImpl :: DSLExpr
typeOfImpl =
  forall tAux $ \pol1 ->
    forall tAux $ \pol2 ->
      forall tAux $ \pol3 ->
        hasAndOr pol1 pol2 pol3 ~~~> tBool pol1 ~> tBool pol2 ~> tBool pol3

typeOfNumOp2 :: (DSLExpr -> DSLExpr) -> DSLExpr
typeOfNumOp2 numConstraint =
  forall type0 $ \t ->
    numConstraint t ~~~> t ~> t ~> t

typeOfNumOp1 :: (DSLExpr -> DSLExpr) -> DSLExpr
typeOfNumOp1 numConstraint =
  forall type0 $ \t ->
    numConstraint t ~~~> t ~> t

typeOfQuantifier :: Quantifier -> DSLExpr
typeOfQuantifier q =
  forall type0 $ \t ->
    forall tAux $ \pol1 ->
      forall tAux $ \pol2 ->
        hasQuantifier q t pol1 pol2 ~~~> (t ~> tBool pol1) ~> tBool pol2

typeOfForeach :: DSLExpr
typeOfForeach =
  forall tNat $ \d ->
    forall (tList tNat) $ \ds ->
      forall tAux $ \pol ->
        (tIndex d ~> tTensor (tBool pol) ds) ~> tTensor (tBool pol) (cons tNat d ds)

typeOfQuantifierIn :: DSLExpr
typeOfQuantifierIn =
  forall type0 $ \tElem ->
    forall type0 $ \tCont ->
      forall tAux $ \pol ->
        hasConOps tElem tCont ~~~> (tElem ~> tBool pol) ~> tCont ~> tBool pol

typeOfForeachIn :: DSLExpr
typeOfForeachIn =
  forall tNat $ \d ->
    forall type0 $ \tElem ->
      forall tAux $ \pol ->
        -- This is a hack. Need to think about the multi-dimensional case
        -- more carefully.
        let ds = lseq tNat (tList tNat) [d] in
        (tElem ~> tBool pol) ~> tTensor tElem ds ~> tTensor (tBool pol) ds

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
        hasConOps tElem tCont ~~~> (tElem ~> tRes ~> tRes) ~> tRes ~> tCont ~> tRes