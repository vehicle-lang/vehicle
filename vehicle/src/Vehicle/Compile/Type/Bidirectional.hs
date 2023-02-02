module Vehicle.Compile.Type.Bidirectional
  ( checkExpr,
    inferExpr,
  )
where

import Control.Monad.Reader (MonadReader (..))
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Subsystem
import Vehicle.Compile.Type.VariableContext (TypingBoundCtx)
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised (GluedExpr (..))
import Prelude hiding (pi)

--------------------------------------------------------------------------------
-- Bidirectional type-checking

-- Recurses through the expression, switching between check and infer modes.
-- Inserts meta-variables for missing implicit and instance arguments and
-- gathers the constraints over those meta-variables.

--------------------------------------------------------------------------------
-- Debug functions

showCheckEntry :: MonadBidirectional builtin m => DBType builtin -> UncheckedExpr builtin -> m ()
showCheckEntry t e = do
  logDebug MaxDetail ("check-entry" <+> prettyVerbose e <+> ":" <+> prettyVerbose t)
  incrCallDepth

showCheckExit :: MonadBidirectional builtin m => DBExpr builtin -> m ()
showCheckExit e = do
  decrCallDepth
  logDebug MaxDetail ("check-exit " <+> prettyVerbose e)

showInferEntry :: MonadBidirectional builtin m => UncheckedExpr builtin -> m ()
showInferEntry e = do
  logDebug MaxDetail ("infer-entry" <+> prettyVerbose e)
  incrCallDepth

showInferExit :: MonadBidirectional builtin m => (DBExpr builtin, CheckedType builtin) -> m ()
showInferExit (e, t) = do
  decrCallDepth
  logDebug MaxDetail ("infer-exit " <+> prettyVerbose e <+> ":" <+> prettyVerbose t)

-------------------------------------------------------------------------------
-- Utility functions

-- | Type checking monad with additional bound context for the bidirectional
-- type-checking pass.
type MonadBidirectional builtin m =
  ( TCM builtin m,
    TypableBuiltin builtin,
    MonadReader (TypingBoundCtx builtin) m
  )

checkExprTypesEqual ::
  MonadBidirectional builtin m =>
  Provenance ->
  DBExpr builtin ->
  DBType builtin ->
  DBType builtin ->
  m ()
checkExprTypesEqual p expr expectedType actualType = do
  ctx <- ask
  let origin = CheckingExprType expr expectedType actualType
  createFreshUnificationConstraint p ctx origin expectedType actualType

checkBinderTypesEqual ::
  MonadBidirectional builtin m =>
  Provenance ->
  Maybe Name ->
  DBType builtin ->
  DBType builtin ->
  m ()
checkBinderTypesEqual p binderName expectedType actualType = do
  ctx <- ask
  let origin = CheckingBinderType binderName expectedType actualType
  createFreshUnificationConstraint p ctx origin expectedType actualType

--------------------------------------------------------------------------------
-- Checking

checkExpr ::
  MonadBidirectional builtin m =>
  DBType builtin -> -- Type we're checking against
  DBExpr builtin -> -- Expression being type-checked
  m (DBExpr builtin) -- Updated expression
checkExpr expectedType expr = do
  showCheckEntry expectedType expr
  res <- case (expectedType, expr) of
    -- In the case where we have a matching pi binder and lam binder use the pi-binder to
    -- aid inference of lambda binder.
    (Pi _ piBinder resultType, Lam p lamBinder body)
      | visibilityOf piBinder == visibilityOf lamBinder -> do
          let binderName = nameOf lamBinder
          -- Check that the type of the lambda binder is a type.
          checkedLamBinderType <- checkExpr (TypeUniverse p 0) (typeOf lamBinder)

          -- Check that the lambda and pi binders have the same type.
          checkBinderTypesEqual p binderName (typeOf piBinder) checkedLamBinderType

          -- Add bound variable to context
          checkedBody <- addToBoundCtx (binderName, typeOf piBinder, Nothing) $ do
            -- Check if the type of the expression matches the expected result type.
            checkExpr resultType body

          let checkedLamBinder = replaceBinderType checkedLamBinderType lamBinder
          return $ Lam p checkedLamBinder checkedBody

    -- In the case where we have an implicit or instance pi binder then insert a new
    -- lambda expression.
    (Pi _ piBinder resultType, e)
      | isImplicit piBinder || isInstance piBinder -> do
          -- Then eta-expand
          let p = provenanceOf piBinder
          let binderName = nameOf piBinder
          let binderType = typeOf piBinder

          -- Add the pi-bound variable to the context
          checkedExpr <-
            addToBoundCtx (binderName, binderType, Nothing) $
              -- Check if the type of the expression matches the expected result type.
              checkExpr resultType (liftDBIndices 1 e)

          -- Create a new binder mirroring the Pi binder expected
          lamBinderName <- getBinderNameOrFreshName (nameOf piBinder) binderType
          let lamBinderForm = BinderDisplayForm (OnlyName lamBinderName) False
          let lamBinder = Binder p lamBinderForm (visibilityOf piBinder) (relevanceOf piBinder) () binderType

          -- Prepend a new lambda to the expression with the implicit binder
          return $ Lam p lamBinder checkedExpr
    (_, Hole p _name) -> do
      -- Replace the hole with meta-variable.
      -- NOTE, different uses of the same hole name will be interpreted as
      -- different meta-variables.
      boundCtx <- getBoundCtx
      unnormalised <$> freshExprMeta p expectedType boundCtx

    -- Otherwise switch to inference mode
    (_, _) -> viaInfer expectedType expr

  showCheckExit res
  return res

viaInfer :: MonadBidirectional builtin m => DBType builtin -> UncheckedExpr builtin -> m (CheckedExpr builtin)
viaInfer expectedType expr = do
  let p = provenanceOf expr
  -- Switch to inference mode
  (checkedExpr, actualType) <- inferExpr expr
  -- Insert any needed implicit or instance arguments
  (appliedCheckedExpr, resultType) <- inferApp p checkedExpr actualType []
  -- Check the expected and the actual types are equal
  checkExprTypesEqual p expr expectedType resultType
  return appliedCheckedExpr

--------------------------------------------------------------------------------
-- Inference

-- | Takes in an unchecked expression and attempts to infer it's type.
-- Returns the expression annotated with its type as well as the type itself.
inferExpr ::
  MonadBidirectional builtin m =>
  UncheckedExpr builtin ->
  m (CheckedExpr builtin, CheckedType builtin)
inferExpr e = do
  showInferEntry e
  res <- case e of
    Universe p u -> case u of
      TypeUniv l -> return (e, TypeUniverse p (l + 1))
      _ ->
        compilerDeveloperError $
          "Should not be trying to infer the type of" <+> pretty u
    Meta _ m -> do
      metaType <- getMetaType m
      return (e, metaType)
    Hole p _name -> do
      -- Replace the hole with meta-variable.
      -- NOTE, different uses of the same hole name will be interpreted
      -- as different meta-variables.
      boundCtx <- getBoundCtx
      metaType <- unnormalised <$> freshExprMeta p (TypeUniverse p 0) boundCtx
      metaExpr <- unnormalised <$> freshExprMeta p metaType boundCtx
      checkExprTypesEqual p metaExpr metaType (TypeUniverse p 0)
      return (metaExpr, metaType)
    Ann p expr exprType -> do
      -- Check the annotation is a type.
      (checkedExprType, exprTypeType) <- inferExpr exprType
      checkExprTypesEqual p exprType (TypeUniverse p 0) exprTypeType

      checkedExpr <- checkExpr checkedExprType expr
      return (Ann p checkedExpr checkedExprType, checkedExprType)
    Pi p binder resultType -> do
      (checkedBinderType, typeOfBinderType) <- inferExpr (typeOf binder)

      (checkedResultType, typeOfResultType) <-
        addToBoundCtx (nameOf binder, checkedBinderType, Nothing) $ inferExpr resultType

      maxResultType <- typeOfBinderType `tMax` typeOfResultType
      let checkedBinder = replaceBinderType checkedBinderType binder
      return (Pi p checkedBinder checkedResultType, maxResultType)

    -- Literals are slightly tricky to type-check, as by default they
    -- probably are standalone, i.e. not wrapped in an `App`, in which
    -- case we need to insert an `App` around them. However, if the
    -- user has provided an implicit argument to them or we are type
    -- checking a second time, then the `App` will already be present.
    -- One approach might be to pass a boolean flag through `infer`
    -- which signals whether the parent node is an `App`, however
    -- for now it's simplier to split into the following two cases:
    App p (Literal p' l) args -> do
      let (checkedLit, checkedLitType) = inferLiteral p' l
      inferApp p checkedLit checkedLitType (NonEmpty.toList args)
    Literal p l -> do
      let (checkedLit, checkedLitType) = inferLiteral p l
      inferApp p checkedLit checkedLitType []
    App p fun args -> do
      (checkedFun, checkedFunType) <- inferExpr fun
      inferApp p checkedFun checkedFunType (NonEmpty.toList args)
    Var p (Bound i) -> do
      ctx <- getBoundCtx
      case lookupVar ctx i of
        Just (_, checkedType, _) -> do
          let liftedCheckedType = liftDBIndices (DBLevel $ unIndex i + 1) checkedType
          return (Var p (Bound i), liftedCheckedType)
        Nothing ->
          compilerDeveloperError $
            "DBIndex"
              <+> pretty i
              <+> "out of bounds when looking"
              <+> "up variable in context"
              <+> prettyVerbose (boundContextOf ctx)
              <+> "at"
              <+> pretty p
    Var p (Free ident) -> do
      originalType <- getDeclType p ident
      return (Var p (Free ident), originalType)
    Let p boundExpr binder body -> do
      -- Check that the type of the bound variable is a type
      (typeOfBoundExpr, typeOfBoundExprType) <- inferExpr (typeOf binder)
      checkExprTypesEqual p typeOfBoundExpr (TypeUniverse p 0) typeOfBoundExprType
      let checkedBinder = replaceBinderType typeOfBoundExpr binder

      -- Check the type of the body, with the bound variable added to the context.
      (checkedBody, typeOfBody) <-
        addToBoundCtx (nameOf binder, typeOfBoundExpr, Nothing) $ inferExpr body

      -- Pretend the let expression is really a lambda application and use
      -- the application machinary to infer the result type and the type of the bound expression.
      (resultType, boundArgs) <-
        inferArgs
          (Lam p checkedBinder body, [ExplicitArg p boundExpr])
          (Pi p checkedBinder typeOfBody)
          [ExplicitArg p boundExpr]

      -- Extract the type of the bound expression
      checkedBoundExpr <- case boundArgs of
        [arg] -> return (argExpr arg)
        _ ->
          compilerDeveloperError $
            "inference of type of let expression returned more than one argument:"
              <+> prettyVerbose boundArgs

      return (Let p checkedBoundExpr checkedBinder checkedBody, resultType)
    Lam p binder body -> do
      -- Infer the type of the bound variable from the binder
      (typeOfBinder, typeOfBinderType) <- inferExpr (typeOf binder)

      checkExprTypesEqual p typeOfBinder (TypeUniverse p 0) typeOfBinderType
      let checkedBinder = replaceBinderType typeOfBinder binder

      -- Update the context with the bound variable
      (checkedBody, typeOfBody) <-
        addToBoundCtx (nameOf binder, typeOfBinder, Nothing) $ inferExpr body

      let t' = Pi p checkedBinder typeOfBody
      return (Lam p checkedBinder checkedBody, t')
    Builtin p op -> do
      return (Builtin p op, typeBuiltin p op)
    LVec p elems -> do
      -- Infer the type for each element in the list
      elemTypePairs <- traverse inferExpr elems
      -- Insert any implicit arguments for each element in the list to try and
      -- standardise the types
      (checkedElems, typesOfElems) <- unzip <$> traverse (uncurry $ insertNonExplicitArgs p) elemTypePairs
      -- Create the new type.
      let vecType = typeVectorLiteral p typesOfElems
      return (LVec p checkedElems, vecType)

  showInferExit res
  return res

inferLiteral :: TypableBuiltin builtin => Provenance -> Literal -> (CheckedExpr builtin, CheckedType builtin)
inferLiteral p l = (Literal p l, typeLiteral p l)

-- | Takes a function and its arguments, inserts any needed implicits
-- or instance arguments and then returns the function applied to the full
-- list of arguments as well as the result type.
inferApp ::
  MonadBidirectional builtin m =>
  Provenance ->
  CheckedExpr builtin ->
  CheckedType builtin ->
  [UncheckedArg builtin] ->
  m (CheckedExpr builtin, CheckedType builtin)
inferApp p fun funType args = do
  (appliedFunType, checkedArgs) <- inferArgs (fun, args) funType args
  return (normAppList p fun checkedArgs, appliedFunType)

-- | Takes the expected type of a function and the user-provided arguments
-- and traverses through checking each argument type against the type of the
-- matching pi binder and inserting any required implicit/instance arguments.
-- Returns the type of the function when applied to the full list of arguments
-- (including inserted arguments) and that list of arguments.
inferArgs ::
  MonadBidirectional builtin m =>
  (CheckedExpr builtin, [UncheckedArg builtin]) -> -- The original function and its arguments
  CheckedType builtin -> -- Type of the function
  [UncheckedArg builtin] -> -- User-provided arguments of the function
  m (CheckedType builtin, [CheckedArg builtin])
inferArgs original@(fun, args') piT@(Pi _ binder resultType) args
  | isExplicit binder && null args = return (piT, [])
  | otherwise = do
      let p = provenanceOf fun

      -- Determine whether we have an arg that matches the binder
      (matchedUncheckedArg, remainingUncheckedArgs) <- case args of
        [] -> return (Nothing, args)
        (arg : remainingArgs)
          | visibilityMatches binder arg -> return (Just arg, remainingArgs)
          | isExplicit binder -> do
              boundCtx <- getBoundCtx
              handleTypingError (MissingExplicitArgument boundCtx binder arg)
          | otherwise -> return (Nothing, args)

      -- Calculate what the new checked arg should be, create a fresh meta
      -- if no arg was matched above
      checkedArg <- case matchedUncheckedArg of
        Just arg -> traverse (checkExpr (typeOf binder)) arg
        Nothing -> do
          boundCtx <- getBoundCtx
          newArg <- instantiateArgForNonExplicitBinder boundCtx p (fun, args') binder
          return $ fmap unnormalised newArg

      -- Substitute the checked arg through the result of the Pi type.
      let substResultType = argExpr checkedArg `substDBInto` resultType

      -- Recurse if necessary to check the remaining unchecked args
      let needToRecurse = not (null remainingUncheckedArgs) || visibilityOf binder /= Explicit
      (typeAfterApplication, checkedArgs) <-
        if needToRecurse
          then inferArgs original substResultType remainingUncheckedArgs
          else return (substResultType, [])

      -- Return the result
      return (typeAfterApplication, checkedArg : checkedArgs)
inferArgs (fun, originalArgs) nonPiType args
  | null args = return (nonPiType, [])
  | otherwise = do
      ctx <- getBoundCtx
      handleTypingError (FunctionTypeMismatch ctx fun originalArgs nonPiType args)

insertNonExplicitArgs ::
  MonadBidirectional builtin m =>
  Provenance ->
  CheckedExpr builtin ->
  CheckedType builtin ->
  m (CheckedExpr builtin, CheckedType builtin)
insertNonExplicitArgs p checkedExpr actualType = inferApp p checkedExpr actualType []

universeLevel :: MonadBidirectional builtin m => CheckedExpr builtin -> m UniverseLevel
universeLevel = \case
  Universe _ (TypeUniv l) -> return l
  -- These next cases are probably going to bite us, apologies.
  Meta {} -> return 0
  App _ Meta {} _ -> return 0
  Pi _ _ r -> universeLevel r
  t ->
    compilerDeveloperError $
      "Expected argument of type Type. Found" <+> prettyVerbose t <> "."

tMax :: MonadBidirectional builtin m => CheckedExpr builtin -> CheckedExpr builtin -> m (CheckedExpr builtin)
tMax t1 t2 = do
  l1 <- universeLevel t1
  l2 <- universeLevel t2
  return $ if l1 > l2 then t1 else t2
