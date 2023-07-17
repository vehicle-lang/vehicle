module Vehicle.Compile.Type.Bidirectional
  ( checkExpr,
    inferExpr,
    MonadBidirectionalInternal,
    checkExprTypesEqual,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), asks)
import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Maybe (fromMaybe)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised
import Prelude hiding (pi)

--------------------------------------------------------------------------------
-- Bidirectional type-checking

-- Recurses through the expression, switching between check and infer modes.
-- Inserts meta-variables for missing implicit and instance arguments and
-- gathers the constraints over those meta-variables.

type MonadBidirectionalInternal builtin m =
  ( MonadTypeChecker builtin m,
    MonadReader (TypingBoundCtx builtin, Relevance) m
  )

-- | Type checking monad with additional bound context for the bidirectional
-- type-checking pass.
type MonadBidirectional builtin m =
  ( MonadBidirectionalInternal builtin m,
    TypableBuiltin builtin
  )

--------------------------------------------------------------------------------
-- Checking

checkExpr ::
  (MonadBidirectional builtin m) =>
  Type Ix builtin -> -- Type we're checking against
  Expr Ix builtin -> -- Expression being type-checked
  m (Expr Ix builtin) -- Checked expression
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
          checkedBody <- extendBoundCtx piBinder $ do
            -- Check if the type of the expression matches the expected result type.
            checkExpr resultType body

          let checkedLamBinder = replaceBinderType checkedLamBinderType lamBinder
          let finalLamBinder = setBinderRelevance checkedLamBinder (relevanceOf piBinder)
          return $ Lam p finalLamBinder checkedBody

    -- In the case where we have an implicit or instance pi binder then insert a new
    -- lambda expression.
    (Pi _ piBinder resultType, e)
      | isImplicit piBinder || isInstance piBinder -> do
          -- Then eta-expand
          let p = provenanceOf piBinder
          let binderType = typeOf piBinder

          -- Add the pi-bound variable to the context
          checkedExpr <-
            extendBoundCtx piBinder $
              -- Check if the type of the expression matches the expected result type.
              checkExpr resultType (liftDBIndices 1 e)

          -- Create a new binder mirroring the Pi binder expected
          lamBinderName <- getBinderNameOrFreshName (nameOf piBinder) binderType
          let lamBinderForm = BinderDisplayForm (OnlyName lamBinderName) False
          let lamBinder = Binder p lamBinderForm (visibilityOf piBinder) (relevanceOf piBinder) binderType

          -- Prepend a new lambda to the expression with the implicit binder
          return $ Lam p lamBinder checkedExpr
    (_, Hole p _name) -> do
      -- Replace the hole with meta-variable.
      -- NOTE, different uses of the same hole name will be interpreted as
      -- different meta-variables.
      boundCtx <- getBoundCtx
      unnormalised <$> freshMetaExpr p expectedType boundCtx

    -- Otherwise switch to inference mode
    (_, _) -> viaInfer expectedType expr

  showCheckExit res
  return res

viaInfer :: (MonadBidirectional builtin m) => Type Ix builtin -> Expr Ix builtin -> m (Expr Ix builtin)
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
  (MonadBidirectional builtin m) =>
  Expr Ix builtin ->
  m (Expr Ix builtin, Type Ix builtin)
inferExpr e = do
  showInferEntry e
  res <- case e of
    -- TODO fix once we have a universe solver up and running.
    Universe p (UniverseLevel l) -> return (e, TypeUniverse p l)
    Meta _ m -> do
      metaType <- getMetaType m
      return (e, metaType)
    Hole p _name -> do
      -- Replace the hole with meta-variable.
      -- NOTE, different uses of the same hole name will be interpreted
      -- as different meta-variables.
      boundCtx <- getBoundCtx
      metaType <- unnormalised <$> freshMetaExpr p (TypeUniverse p 0) boundCtx
      metaExpr <- unnormalised <$> freshMetaExpr p metaType boundCtx
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
      let checkedBinder = replaceBinderType checkedBinderType binder

      (checkedResultType, typeOfResultType) <-
        extendBoundCtx checkedBinder $ inferExpr resultType

      maxResultType <- typeOfBinderType `tMax` typeOfResultType
      return (Pi p checkedBinder checkedResultType, maxResultType)
    App p fun args -> do
      (checkedFun, checkedFunType) <- inferExpr fun
      inferApp p checkedFun checkedFunType (NonEmpty.toList args)
    BoundVar p i -> do
      ctx <- getBoundCtx
      case lookupIx ctx i of
        Just binder -> do
          currentRelevance <- getCurrentRelevance
          if currentRelevance == Relevant && relevanceOf binder == Irrelevant
            then do
              let varName = fromMaybe "<unknown>" $ nameOf binder
              throwError $ RelevantUseOfIrrelevantVariable p varName
            else do
              let liftedCheckedType = liftDBIndices (Lv $ unIx i + 1) (typeOf binder)
              return (BoundVar p i, liftedCheckedType)
        Nothing ->
          compilerDeveloperError $
            "Ix"
              <+> pretty i
              <+> "out of bounds when looking"
              <+> "up variable in context"
              <+> prettyVerbose (boundContextOf ctx)
              <+> "at"
              <+> pretty p
    FreeVar p ident -> do
      originalType <- getDeclType p ident
      return (FreeVar p ident, originalType)
    Let p boundExpr binder body -> do
      -- Check that the type of the bound variable is a type
      (typeOfBoundExpr, typeOfBoundExprType) <- inferExpr (typeOf binder)
      checkExprTypesEqual p typeOfBoundExpr (TypeUniverse p 0) typeOfBoundExprType
      let checkedBinder = replaceBinderType typeOfBoundExpr binder

      -- Check the type of the body, with the bound variable added to the context.
      (checkedBody, typeOfBody) <-
        extendBoundCtx checkedBinder $ inferExpr body

      -- Pretend the let expression is really a lambda application and use
      -- the application machinary to infer the result type and the type of the bound expression.
      (resultType, boundArgs) <-
        inferArgs
          (Lam p checkedBinder body, [RelevantExplicitArg p boundExpr])
          (Pi p checkedBinder typeOfBody)
          [RelevantExplicitArg p boundExpr]

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
      (checkedBody, typeOfBody) <- extendBoundCtx checkedBinder $ inferExpr body

      let t' = Pi p checkedBinder typeOfBody
      return (Lam p checkedBinder checkedBody, t')
    Builtin p op -> do
      return (Builtin p op, typeBuiltin p op)

  showInferExit res
  return res

-- | Takes a function and its arguments, inserts any needed implicits
-- or instance arguments and then returns the function applied to the full
-- list of arguments as well as the result type.
inferApp ::
  (MonadBidirectional builtin m) =>
  Provenance ->
  Expr Ix builtin ->
  Type Ix builtin ->
  [Arg Ix builtin] ->
  m (Expr Ix builtin, Type Ix builtin)
inferApp p fun funType args = do
  (appliedFunType, checkedArgs) <- inferArgs (fun, args) funType args
  return (normAppList p fun checkedArgs, appliedFunType)

-- | Takes the expected type of a function and the user-provided arguments
-- and traverses through checking each argument type against the type of the
-- matching pi binder and inserting any required implicit/instance arguments.
-- Returns the type of the function when applied to the full list of arguments
-- (including inserted arguments) and that list of arguments.
inferArgs ::
  (MonadBidirectional builtin m) =>
  (Expr Ix builtin, [Arg Ix builtin]) -> -- The original function and its arguments
  Type Ix builtin -> -- Type of the function
  [Arg Ix builtin] -> -- User-provided arguments of the function
  m (Type Ix builtin, [Arg Ix builtin])
inferArgs original@(fun, args') piT@(Pi _ binder resultType) args
  | isExplicit binder && null args = return (piT, [])
  | otherwise = do
      let p = provenanceOf fun
      let visibility = visibilityOf binder

      -- Determine whether we have an arg that matches the binder
      (matchedUncheckedArg, remainingUncheckedArgs) <- case args of
        [] -> return (Nothing, args)
        (arg : remainingArgs)
          | visibilityOf arg == visibility -> return (Just arg, remainingArgs)
          | isExplicit binder -> do
              boundCtx <- getBoundCtx
              handleTypingError (MissingExplicitArgument boundCtx binder arg)
          | otherwise -> return (Nothing, args)

      -- Calculate what the new checked arg should be, create a fresh meta
      -- if no arg was matched above
      checkedArg <- case matchedUncheckedArg of
        Just arg -> do
          let relevance = relevanceOf binder
          checkedArgExpr <-
            setRelevance relevance $
              checkExpr (typeOf binder) (argExpr arg)
          return $ Arg p visibility relevance checkedArgExpr
        Nothing -> do
          boundCtx <- getBoundCtx
          newArg <- instantiateArgForNonExplicitBinder boundCtx p (fun, args') binder
          return $ fmap unnormalised newArg

      -- Substitute the checked arg through the result of the Pi type.
      let substResultType = argExpr checkedArg `substDBInto` resultType

      -- Recurse if necessary to check the remaining unchecked args
      let needToRecurse = not (null remainingUncheckedArgs) || visibility /= Explicit
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

-------------------------------------------------------------------------------
-- Utility functions

universeLevel :: (MonadBidirectional builtin m) => Expr Ix builtin -> m Int
universeLevel = \case
  TypeUniverse _ l -> return l
  -- These next cases are probably going to bite us, apologies.
  Meta {} -> return 0
  App _ Meta {} _ -> return 0
  Pi _ _ r -> universeLevel r
  t ->
    compilerDeveloperError $
      "Expected argument of type Type. Found" <+> prettyVerbose t <> "."

tMax :: (MonadBidirectional builtin m) => Expr Ix builtin -> Expr Ix builtin -> m (Expr Ix builtin)
tMax t1 t2 = do
  l1 <- universeLevel t1
  l2 <- universeLevel t2
  return $ if l1 > l2 then t1 else t2

checkExprTypesEqual ::
  (MonadBidirectionalInternal builtin m) =>
  Provenance ->
  Expr Ix builtin ->
  Type Ix builtin ->
  Type Ix builtin ->
  m ()
checkExprTypesEqual p expr expectedType actualType = do
  ctx <- getBoundCtx
  let origin = CheckingExprType expr expectedType actualType
  createFreshUnificationConstraint p ctx origin expectedType actualType

checkBinderTypesEqual ::
  (MonadBidirectional builtin m) =>
  Provenance ->
  Maybe Name ->
  Type Ix builtin ->
  Type Ix builtin ->
  m ()
checkBinderTypesEqual p binderName expectedType actualType = do
  ctx <- getBoundCtx
  let origin = CheckingBinderType binderName expectedType actualType
  createFreshUnificationConstraint p ctx origin expectedType actualType

getBoundCtx :: (MonadBidirectionalInternal builtin m) => m (TypingBoundCtx builtin)
getBoundCtx = asks fst

getCurrentRelevance :: (MonadBidirectionalInternal builtin m) => m Relevance
getCurrentRelevance = asks snd

extendBoundCtx ::
  (MonadBidirectionalInternal builtin m) =>
  Binder Ix builtin ->
  m c ->
  m c
extendBoundCtx binder = local (first (binder :))

setRelevance ::
  (MonadBidirectionalInternal builtin m) =>
  Relevance ->
  m c ->
  m c
setRelevance relevance = local (second (relevance <>))

--------------------------------------------------------------------------------
-- Debug functions

showCheckEntry :: (MonadBidirectional builtin m) => Type Ix builtin -> Expr Ix builtin -> m ()
showCheckEntry t e = do
  logDebug MaxDetail ("check-entry" <+> prettyVerbose e <+> ":" <+> prettyVerbose t)
  incrCallDepth

showCheckExit :: (MonadBidirectional builtin m) => Expr Ix builtin -> m ()
showCheckExit e = do
  decrCallDepth
  logDebug MaxDetail ("check-exit " <+> prettyVerbose e)

showInferEntry :: (MonadBidirectional builtin m) => Expr Ix builtin -> m ()
showInferEntry e = do
  logDebug MaxDetail ("infer-entry" <+> prettyVerbose e)
  incrCallDepth

showInferExit :: (MonadBidirectional builtin m) => (Expr Ix builtin, Type Ix builtin) -> m ()
showInferExit (e, t) = do
  decrCallDepth
  logDebug MaxDetail ("infer-exit " <+> prettyVerbose e <+> ":" <+> prettyVerbose t)
