module Vehicle.Compile.Type.Bidirectional
  ( checkExprType,
    inferExprType,
    solveArgInsertionProblem,
    createFreshUnificationConstraint,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.Data (Proxy (..))
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Maybe (fromMaybe)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (normaliseInEnv)
import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Constraint.UnificationSolver (solveUnificationConstraint)
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Force (forceHead)
import Vehicle.Compile.Type.Meta (MetaSet)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Monad.Class (createFreshConstraintCtx)
import Vehicle.Compile.Type.System (HasTypeSystem (..), TCM)
import Vehicle.Data.Builtin.Interface.Type (TypableBuiltin (..))
import Vehicle.Data.Code.Value
import Vehicle.Data.Universe (UniverseLevel (..))
import Vehicle.Data.Variable.Bound.Context
import Vehicle.Data.Variable.Free.Context.Class
import Prelude hiding (pi)

--------------------------------------------------------------------------------
-- Bidirectional type-checking

-- Recurses through the expression, switching between check and infer modes.
-- Inserts meta-variables for missing implicit and instance arguments and
-- gathers the constraints over those meta-variables.

-- | Type checking monad with additional bound context for the bidirectional
-- type-checking pass.
type MonadBidirectional builtin m =
  ( TCM builtin m,
    MonadBoundContext (Type builtin) m,
    MonadReader Relevance m
  )

runMonadBidirectional ::
  forall m builtin a.
  (MonadTypeChecker builtin m) =>
  BoundCtx (Type builtin) ->
  Relevance ->
  BoundContextT (Type builtin) (ReaderT Relevance m) a ->
  m a
runMonadBidirectional ctx relevance x =
  runReaderT (runBoundContextT ctx x) relevance

--------------------------------------------------------------------------------
-- Checking

-- | Checks that the given expression is of the provided type while
-- generating the necessary constraints along the way, returning a well-typed
-- version of the expression with the necessary implicit and instance arguments
-- inserted.
checkExprType ::
  (TCM builtin m) =>
  BoundCtx (Type builtin) ->
  Relevance ->
  Type builtin ->
  Expr builtin ->
  m (Expr builtin)
checkExprType boundCtx relevance expectedType expr = do
  runMonadBidirectional boundCtx relevance $ checkExpr expectedType expr

-- | Checks that the given expression is of the provided type while
-- generating the necessary constraints along the way, returning a well-typed
-- version of the expression with the necessary implicit and instance arguments
-- inserted.
checkExpr ::
  forall builtin m.
  (MonadBidirectional builtin m) =>
  Type builtin ->
  Expr builtin ->
  m (Expr builtin)
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

          let checkedLamBinder = replaceBinderType (typeOf piBinder) lamBinder
          let finalLamBinder = setBinderRelevance checkedLamBinder (relevanceOf piBinder)
          -- Add bound variable to context and check if the type of the expression
          -- matches the expected result type.
          checkedBody <- addBinderToContext finalLamBinder $ checkExpr resultType body
          return $ Lam p finalLamBinder checkedBody

    -- In the case where we have an implicit or instance pi binder then insert a new
    -- lambda expression.
    (Pi _ piBinder resultType, e)
      | isImplicit piBinder || isInstance piBinder -> do
          logDebug MaxDetail $ "inserting-binder" <+> prettyVerbose piBinder

          -- Create a suitable binder
          let p = provenanceOf piBinder
          let binderType = typeOf piBinder
          lamBinderName <- getBinderNameOrFreshName (nameOf piBinder) binderType
          let lamBinderForm = BinderDisplayForm (OnlyName lamBinderName) False
          let lamBinder = Binder p lamBinderForm (visibilityOf piBinder) (relevanceOf piBinder) binderType

          -- Re-check the expression
          checkedExpr <- addBinderToContext lamBinder $ checkExpr resultType (liftDBIndices 1 e)

          return $ Lam p lamBinder checkedExpr
    -- checkExpr expectedType (Lam p lamBinder e)

    -- Otherwise switch to inference mode
    (_, _) -> viaInfer expectedType expr

  showCheckExit res
  return res

viaInfer ::
  (MonadBidirectional builtin m) =>
  Type builtin ->
  Expr builtin ->
  m (Expr builtin)
viaInfer expectedType expr = do
  let p = provenanceOf expr
  -- Switch to inference mode
  (checkedExpr, actualType) <- inferExpr expr
  -- Insert any needed implicit or instance arguments
  (appliedCheckedExpr, resultType) <- inferApp checkedExpr actualType []
  -- Check the expected and the actual types are equal
  checkExprTypesEqual p expr expectedType resultType
  return appliedCheckedExpr

--------------------------------------------------------------------------------
-- Inference

inferExprType ::
  (TCM builtin m) =>
  BoundCtx (Type builtin) ->
  Relevance ->
  Expr builtin ->
  m (Expr builtin, Type builtin)
inferExprType boundCtx relevance expr = do
  runMonadBidirectional boundCtx relevance $ inferExpr expr

-- | Takes in an unchecked expression and attempts to infer it's type.
-- Returns the expression annotated with its type as well as the type itself.
inferExpr ::
  forall builtin m.
  (MonadBidirectional builtin m) =>
  Expr builtin ->
  m (Expr builtin, Type builtin)
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
      boundCtx <- getBoundCtx (Proxy @(Type builtin))
      metaType <- freshMetaExpr p (TypeUniverse p 0) boundCtx
      metaExpr <- freshMetaExpr p metaType boundCtx
      return (metaExpr, metaType)
    Pi p binder resultType -> do
      checkedBinderType <- checkExpr (TypeUniverse p 0) (typeOf binder)
      let checkedBinder = replaceBinderType checkedBinderType binder

      checkedResultType <-
        addBinderToContext checkedBinder $
          checkExpr (TypeUniverse p 0) resultType

      return (Pi p checkedBinder checkedResultType, TypeUniverse p 0)
    App fun args -> do
      (checkedFun, checkedFunType) <- inferExpr fun
      inferApp checkedFun checkedFunType (NonEmpty.toList args)
    BoundVar p i -> do
      ctx <- getBoundCtx (Proxy @(Type builtin))
      let binder = lookupIxInBoundCtx i ctx
      currentRelevance <- getCurrentRelevance (Proxy @builtin)
      if currentRelevance == Relevant && relevanceOf binder == Irrelevant
        then do
          let varName = fromMaybe "<unknown>" $ nameOf binder
          throwError $ TypingError $ RelevantUseOfIrrelevantVariable $ RelevantUseOfIrrelevantVariableError (Proxy @builtin) p varName
        else do
          let liftedCheckedType = liftDBIndices (Lv $ unIx i + 1) (typeOf binder)
          return (BoundVar p i, liftedCheckedType)
    FreeVar p ident -> do
      originalType <- getDeclType (Proxy @builtin) ident
      return (FreeVar p ident, originalType)
    Let p boundExpr binder body -> do
      -- Check that the type of the bound variable is a type
      (typeOfBoundExpr, typeOfBoundExprType) <- inferExpr (typeOf binder)
      checkExprTypesEqual p typeOfBoundExpr (TypeUniverse p 0) typeOfBoundExprType
      let checkedBinder = replaceBinderType typeOfBoundExpr binder

      -- Check that the expression being bound is correct.
      checkedBoundExpr <- checkExpr typeOfBoundExpr boundExpr

      -- Check the type of the body, with the bound variable added to the context.
      (checkedBody, typeOfBody) <- addBinderToContext checkedBinder $ inferExpr body

      -- Substitute through the type of the bound expression to preserve well-typedness
      let finalType = typeOfBoundExpr `substDBInto` typeOfBody

      return (Let p checkedBoundExpr checkedBinder checkedBody, finalType)
    Lam p binder body -> do
      -- Infer the type of the bound variable from the binder
      (typeOfBinder, typeOfBinderType) <- inferExpr (typeOf binder)

      checkExprTypesEqual p typeOfBinder (TypeUniverse p 0) typeOfBinderType
      let checkedBinder = replaceBinderType typeOfBinder binder

      -- Update the context with the bound variable
      (checkedBody, typeOfBody) <- addBinderToContext checkedBinder $ inferExpr body
      return (Lam p checkedBinder checkedBody, Pi p checkedBinder typeOfBody)
    Builtin p op -> do
      typ <- typeBuiltin p op
      return (Builtin p op, typ)
    Record p recordIdent fields -> do
      declaredFields <- getDeclaredRecordFields (Proxy @builtin) recordIdent
      checkedFields <- traverse (checkRecordField declaredFields) fields
      let checkedType = FreeVar p recordIdent
      return (Record p recordIdent checkedFields, checkedType)
    RecordAcc p record (recordIdent, field) -> do
      let recordType = FreeVar (provenanceOf field) recordIdent
      checkedRecord <- checkExpr recordType record
      declaredFields <- getDeclaredRecordFields (Proxy @builtin) recordIdent
      let fieldType = lookupRecordField declaredFields field
      return (RecordAcc p checkedRecord (recordIdent, field), fieldType)

  showInferExit res
  return res

-- | Takes a function and its arguments, inserts any needed implicits
-- or instance arguments and then returns the function applied to the full
-- list of arguments as well as the result type.
inferApp ::
  forall builtin m.
  (MonadBidirectional builtin m) =>
  Expr builtin ->
  Type builtin ->
  [Arg builtin] ->
  m (Expr builtin, Type builtin)
inferApp fun funType args = do
  relevance <- getCurrentRelevance (Proxy @builtin)
  ctx <- getBoundCtx (Proxy @(Type builtin))
  let insertionProblem =
        ArgInsertionProblem
          { originalFun = fun,
            originalArgs = args,
            originalType = funType,
            checkedArgs = mempty,
            currentExpectedType = funType,
            uncheckedArgs = args,
            contextRelevance = relevance
          }
  result <- solveArgInsertionProblem ctx insertionProblem
  case result of
    Left (problem, blockingMetas) -> createFreshApplicationConstraint ctx problem blockingMetas
    Right r -> return r

-------------------------------------------------------------------------------
-- Utility functions

checkExprTypesEqual ::
  forall builtin m.
  (MonadBidirectional builtin m) =>
  Provenance ->
  Expr builtin ->
  Type builtin ->
  Type builtin ->
  m ()
checkExprTypesEqual p expr expectedType actualType = do
  ctx <- getBoundCtx (Proxy @(Type builtin))
  let origin =
        CheckingExprType $
          CheckingExpr
            { checkedExpr = Right expr,
              checkedExprExpectedType = expectedType,
              checkedExprActualType = actualType
            }
  createFreshUnificationConstraint p ctx origin expectedType actualType

checkBinderTypesEqual ::
  forall builtin m.
  (MonadBidirectional builtin m) =>
  Provenance ->
  Maybe Name ->
  Type builtin ->
  Type builtin ->
  m ()
checkBinderTypesEqual p binderName expectedType actualType = do
  ctx <- getBoundCtx (Proxy @(Type builtin))
  let origin =
        CheckingExprType $
          CheckingExpr
            { checkedExpr = Left binderName,
              checkedExprExpectedType = expectedType,
              checkedExprActualType = actualType
            }
  createFreshUnificationConstraint p ctx origin expectedType actualType

checkRecordField ::
  (MonadBidirectional builtin m) =>
  RecordFields (Type builtin) ->
  RecordField (Expr builtin) ->
  m (RecordField (Expr builtin))
checkRecordField declaredFields (field, value) = do
  let fieldType = lookupRecordField declaredFields field
  checkedValue <- checkExpr fieldType value
  return (field, checkedValue)

-- | Adds an entirely new unification constraint (as opposed to one
-- derived from another constraint).
createFreshUnificationConstraint ::
  forall builtin m.
  (MonadTypeChecker builtin m) =>
  Provenance ->
  BoundCtx (Type builtin) ->
  UnificationConstraintOrigin builtin ->
  Type builtin ->
  Type builtin ->
  m ()
createFreshUnificationConstraint p ctx origin expectedType actualType = do
  let env = boundContextToEnv ctx
  normExpectedType <- normaliseInEnv (toNamedBoundCtx ctx) env expectedType
  normActualType <- normaliseInEnv (toNamedBoundCtx ctx) env actualType
  context <- createFreshConstraintCtx p ctx
  let unification = Unify origin normExpectedType normActualType
  solveUnificationConstraint (WithContext unification context)

getCurrentRelevance :: (MonadBidirectional builtin m) => Proxy builtin -> m Relevance
getCurrentRelevance _ = ask

-------------------------------------------------------------------------------
-- Arg insertion problem

type ArgInsertionProblemSolution builtin =
  Either (ArgInsertionProblem builtin, MetaSet) (Expr builtin, Type builtin)

-- | Deals with insertion of missing implicits and instance arguments
solveArgInsertionProblem ::
  (TCM builtin m) =>
  BoundCtx (Type builtin) ->
  ArgInsertionProblem builtin ->
  m (ArgInsertionProblemSolution builtin)
solveArgInsertionProblem ctx problem@ArgInsertionProblem {..} = do
  -- First see if the unnormalised type is correct. Don't pre-emptively normalise as we want to keep as much
  -- type information as we can.
  case currentExpectedType of
    -- If a standard Pi type then proceed to check against it (need to do this first before we check if args
    -- are null, as it may be a non-explicit binder for which we do need to insert arguments even if the user
    -- hasn't provided any)
    Pi _ binder resultType -> checkArgsAgainstPiType ctx problem binder resultType
    -- Otherwise if there are no unchecked arguments we have nothing to do.
    _
      | null uncheckedArgs -> argInsertionProblemSolved problem
      | otherwise -> do
          -- Force the current expected type to normalise
          (forcedExpectedType, blockingMetas) <- forceApplicationHeadType ctx currentExpectedType
          logDebug MaxDetail ("normalising type to" <+> prettyExternal (WithContext forcedExpectedType (toNamedBoundCtx ctx)))
          case forcedExpectedType of
            -- If the forced expression is a `Pi` then well we've lost the user's types but we can proceed
            Pi _ binder resultType -> checkArgsAgainstPiType ctx problem binder resultType
            -- Otherwise if we are blocked on metas then we can postpone the problem until these metas are solved
            _
              | not (MetaSet.null blockingMetas) -> do
                  let newProblem = ArgInsertionProblem {currentExpectedType = forcedExpectedType, ..}
                  return $ Left (newProblem, blockingMetas)
              -- Otherwise we're truely stuck and we error.
              | otherwise -> do
                  let boundCtx = toNamedBoundCtx ctx
                  throwError $ TypingError $ FunctionTypeMismatch $ FunctionTypeMismatchError boundCtx originalFun currentExpectedType uncheckedArgs

forceApplicationHeadType ::
  (MonadTypeChecker builtin m) =>
  BoundCtx (Type builtin) ->
  Type builtin ->
  m (Type builtin, MetaSet)
forceApplicationHeadType ctx typ = do
  normType <- normaliseInEnv (toNamedBoundCtx ctx) (boundContextToEnv ctx) typ
  (forcedType, blockingMetas) <- forceHead (toNamedBoundCtx ctx) normType
  return (quote (provenanceOf typ) (boundCtxLv ctx) forcedType, blockingMetas)

checkArgsAgainstPiType ::
  (TCM builtin m) =>
  BoundCtx (Type builtin) ->
  ArgInsertionProblem builtin ->
  Binder builtin ->
  Type builtin ->
  m (ArgInsertionProblemSolution builtin)
checkArgsAgainstPiType ctx problem@ArgInsertionProblem {..} binder resultType
  | isExplicit binder && null uncheckedArgs = argInsertionProblemSolved problem
  | otherwise = do
      let nameCtx = toNamedBoundCtx ctx

      let checkedExprDoc = prettyExternal (WithContext (solutionSoFar problem) nameCtx)
      let uncheckedArgsDoc = prettyExternal (WithContext uncheckedArgs nameCtx)
      logDebug MaxDetail $ "checking-args-enter" <+> checkedExprDoc <+> "@" <+> uncheckedArgsDoc
      incrCallDepth
      logDebug MaxDetail $ "expected-type:" <+> prettyExternal (WithContext currentExpectedType nameCtx)

      -- Determine whether we have an arg that matches the binder
      let visibility = visibilityOf binder
      (matchedUncheckedArg, remainingUncheckedArgs) <- case uncheckedArgs of
        [] -> return (Nothing, uncheckedArgs)
        (arg : remainingArgs)
          | visibilityOf arg == visibility -> return (Just arg, remainingArgs)
          | isExplicit binder -> throwError $ TypingError $ MissingExplicitArg $ MissingExplicitArgError (toNamedBoundCtx ctx) binder arg
          | otherwise -> return (Nothing, uncheckedArgs)

      -- Calculate what the new checked arg should be, create a fresh meta
      -- if no arg was matched above
      let p = provenanceOf originalFun
      checkedArg <- case matchedUncheckedArg of
        Just arg -> do
          logDebug MaxDetail $ "matching-arg-found" <+> prettyVerbose arg
          let relevance = relevanceOf binder
          let ctxRelevance = if contextRelevance == Irrelevant then Irrelevant else relevance
          checkedArgExpr <- checkExprType ctx ctxRelevance (typeOf binder) (argExpr arg)
          return $ Arg p (visibilityOf arg) relevance checkedArgExpr
        Nothing -> do
          logDebug MaxDetail "no-matching-arg-found"
          let original = (originalFun, originalArgs, originalType)
          instantiateArgForNonExplicitBinder ctx p original binder

      let newCheckedArgs = checkedArg : checkedArgs
      let newExpectedType = argExpr checkedArg `substDBInto` resultType
      let newProblem =
            problem
              { checkedArgs = newCheckedArgs,
                currentExpectedType = newExpectedType,
                uncheckedArgs = remainingUncheckedArgs
              }

      logDebug MaxDetail $ "new-expected-type:" <+> prettyExternal (WithContext newExpectedType nameCtx)
      decrCallDepth
      let newCheckedExprDoc = prettyExternal (WithContext (solutionSoFar newProblem) nameCtx)
      let newUncheckedArgsDoc = prettyExternal (WithContext remainingUncheckedArgs nameCtx)
      logDebug MaxDetail $ "checking-args-exit" <+> newCheckedExprDoc <+> "@" <+> newUncheckedArgsDoc

      -- Recurse to check the remaining unchecked args
      solveArgInsertionProblem ctx newProblem

argInsertionProblemSolved ::
  (MonadTypeChecker builtin m) =>
  ArgInsertionProblem builtin ->
  m (ArgInsertionProblemSolution builtin)
argInsertionProblemSolved problem@ArgInsertionProblem {..} =
  return $ Right (solutionSoFar problem, currentExpectedType)

instantiateArgForNonExplicitBinder ::
  (TCM builtin m) =>
  BoundCtx (Type builtin) ->
  Provenance ->
  (Expr builtin, [Arg builtin], Type builtin) ->
  Binder builtin ->
  m (Arg builtin)
instantiateArgForNonExplicitBinder boundCtx p (fun, funArgs, funType) binder = do
  let binderType = typeOf binder
  checkedExpr <- case visibilityOf binder of
    Explicit {} -> compilerDeveloperError "Should not be instantiating Arg for explicit Binder"
    Implicit {} -> freshMetaExpr p binderType boundCtx
    Instance {} -> do
      let origin =
            InstanceArgOrigin $
              ArgOrigin
                { checkedInstanceOp = fun,
                  checkedInstanceOpArgs = funArgs,
                  checkedInstanceOpType = funType,
                  checkedInstanceType = binderType
                }
      createFreshInstanceConstraint (isAuxiliaryConstraint binderType) boundCtx (provenanceOf fun) origin (relevanceOf binder) binderType
  return $ Arg p (markInserted $ visibilityOf binder) (relevanceOf binder) checkedExpr

--------------------------------------------------------------------------------
-- Debug functions

showCheckEntry :: forall builtin m. (MonadBidirectional builtin m) => Type builtin -> Expr builtin -> m ()
showCheckEntry t e = do
  ctx <- getNamedBoundCtx (Proxy @(Type builtin))
  logDebug MaxDetail $ "check-entry" <+> prettyExternal (WithContext e ctx) <+> ":" <+> prettyExternal (WithContext t ctx) -- <+> "::::" <+> pretty (length ctx)
  incrCallDepth

showCheckExit :: forall builtin m. (MonadBidirectional builtin m) => Expr builtin -> m ()
showCheckExit e = do
  decrCallDepth
  ctx <- getNamedBoundCtx (Proxy @(Type builtin))
  logDebug MaxDetail $ "check-exit " <+> prettyVerbose e -- (WithContext e ctx)
  logDebug MaxDetail $ "check-exit " <+> prettyExternal (WithContext e ctx)

showInferEntry :: forall builtin m. (MonadBidirectional builtin m) => Expr builtin -> m ()
showInferEntry e = do
  ctx <- getNamedBoundCtx (Proxy @(Type builtin))
  logDebug MaxDetail $ "infer-entry" <+> prettyExternal (WithContext e ctx)
  incrCallDepth

showInferExit :: forall builtin m. (MonadBidirectional builtin m) => (Expr builtin, Type builtin) -> m ()
showInferExit (e, t) = do
  decrCallDepth
  ctx <- getNamedBoundCtx (Proxy @(Type builtin))
  -- logDebug MaxDetail $ "infer-exit " <+> prettyVerbose e <+> ":" <+> prettyVerbose t <+> pretty (length ctx)
  logDebug MaxDetail $ "infer-exit " <+> prettyExternal (WithContext e ctx) <+> ":" <+> prettyExternal (WithContext t ctx)
