{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Normalise.Force
  ( MonadNorm,
    TypedEvalScheme (..),
    eval,
    forceApplication,
    forceThunk,
    forceFreeVar,
    forceInEmptyEnv,
    findInstanceArg,
    forceRecordAcc,
  )
where

import Control.Monad (when)
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.Writer.Strict (MonadWriter (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Data (Proxy (..))
import Data.List.NonEmpty as NonEmpty (toList)
import Data.Map.Ordered qualified as OMap
import Data.Void (absurd)
import GHC.Stack (HasCallStack)
import Vehicle.Compile.Normalise.Core
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendlyEmptyCtx)
import Vehicle.Compile.Type.Meta.Set
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Code.Interface (HasBuiltinConstructor (..), IsArgs (..))
import Vehicle.Data.Variable.Bound.Context.Name (prettyFriendlyInCtx)
import Vehicle.Data.Variable.Free.Context.Class (MonadFreeContext (..))

-----------------------------------------------------------------------------
-- Evaluation interface
-----------------------------------------------------------------------------

class (MetaLike meta) => TypedEvalScheme meta builtin m where
  forceForced ::
    GenericForcedValue meta builtin ->
    m (GenericForcedValue meta builtin)
  forceMeta ::
    meta ->
    GenericUnforcedSpine meta builtin ->
    m (GenericForcedValue meta builtin)
  forceBuiltinEval ::
    (IsArgs args) =>
    EvalBuiltinFn meta builtin args m ->
    builtin ->
    GenericUnforcedSpine meta builtin ->
    m (GenericForcedValue meta builtin)

-- | Evaluation of expressions after type-checking
instance (Monad m) => TypedEvalScheme NoMeta builtin m where
  -- If we have already forced the expression, simply return the result.
  forceForced = return

  -- There are no meta variables at this stage.
  forceMeta m _ = absurd m

  -- When we are evaluating after type-checking, we don't necessarily want to
  -- evaluate builtins, so we leave it up to the call-site.
  forceBuiltinEval _ b spine = return $ VBuiltin b spine

-- | Evaluation of expressions during type-checking
instance
  ( MonadNorm builtin m,
    MonadWriter MetaSet m,
    MonadReader (MetaID -> Maybe (GluedExprWithMetas builtin)) m
  ) =>
  TypedEvalScheme MetaID builtin m
  where
  -- If we come to a meta-variable that has been solved substitute the solution through
  forceMeta m spine = do
    lookupMetaSolution <- ask
    case lookupMetaSolution m of
      Just solution -> forceApplication (normalised solution) spine
      Nothing -> do
        tell $ MetaSet.singleton m
        return $ VMeta m spine

  -- A value that we have previously forced during type-checking
  -- may be able to be forced further if a meta-variable is newly solved.
  forceForced value = case value of
    VBuiltin b spine ->
      forceBuiltin b spine
    VMeta v spine ->
      forceMeta v spine
    VRecordAcc recordType record field spine ->
      forceRecordAcc recordType record field spine
    VUniverse {} -> return value
    VLam {} -> return value
    VPi {} -> return value
    VFreeVar {} -> return value
    VBoundVar {} -> return value
    VRecord {} -> return value

  forceBuiltinEval evalFn b spine =
    case getExpr accessSpine spine of
      Nothing -> return $ mkExpr accessBuiltinC (b, spine)
      Just args -> do
        evalResult <- evalFn args
        case evalResult of
          Evaluated result -> forceThunk result
          Unevaluable {} -> return $ mkExpr accessBuiltinC (b, spine)

-----------------------------------------------------------------------------
-- Evaluation algorithm
-----------------------------------------------------------------------------

-- Merge into `TypedEvalScheme`?
instance
  ( Show meta,
    MonadNorm builtin m,
    TypedEvalScheme meta builtin m
  ) =>
  NormalisableExpr (GenericForcedValue meta) (GenericThunk meta) builtin m
  where
  force = forceThunk
  forceApp = forceApplication

forceThunk ::
  (MonadNorm builtin m, TypedEvalScheme meta builtin m) =>
  GenericThunk meta builtin ->
  m (GenericForcedValue meta builtin)
forceThunk = \case
  Forced value -> forceForced value
  Unforced env expr -> eval env expr

eval ::
  forall meta builtin m.
  (MonadNorm builtin m, TypedEvalScheme meta builtin m) =>
  GenericBoundEnv meta builtin ->
  Expr builtin ->
  m (GenericForcedValue meta builtin)
eval env expr = do
  showEntry env expr
  result <- case expr of
    Hole {} -> resolutionError currentPass "Hole"
    -- Always handlable
    BoundVar _ v -> forceBoundVar env v
    FreeVar _ v -> forceFreeVar v []
    Builtin _ b -> return $ VBuiltin b []
    Meta _ m -> forceMeta (fromMetaID m) []
    Let _ bound binder body ->
      forceLet env bound binder body
    RecordProj _ typ record field ->
      forceRecordAcc (Unforced env typ) (Unforced env record) field []
    App fun args ->
      forceApplication (Unforced env fun) (fmap (Unforced env) <$> NonEmpty.toList args)
    Universe _ u ->
      return $ VUniverse u
    Lam _ binder body ->
      return $ VLam (fmap (Unforced env) binder) (Closure env body)
    Pi _ binder body ->
      return $ VPi (fmap (Unforced env) binder) (Closure env body)
    Record _p recordType fields ->
      return $ VRecord (Unforced env recordType) $ OMap.fromList $ fmap (second (Unforced env)) fields

  showExit result
  return result

forceApplication ::
  (MonadNorm builtin m, TypedEvalScheme meta builtin m) =>
  GenericThunk meta builtin ->
  GenericUnforcedSpine meta builtin ->
  m (GenericForcedValue meta builtin)
forceApplication fun [] = forceThunk fun
forceApplication fun args@(a : as) = do
  forcedFun <- forceThunk fun
  case forcedFun of
    VBuiltin b spine ->
      forceBuiltin b (spine <> args)
    VFreeVar v spine ->
      return $ VFreeVar v (spine <> args)
    VMeta v spine ->
      forceMeta v (spine <> args)
    VBoundVar v spine ->
      return $ VBoundVar v (spine <> args)
    VRecordAcc recordType record field spine ->
      return $ VRecordAcc recordType record field (spine <> args)
    VLam binder closure
      | not (visibilityMatches binder a) ->
          visibilityError fun a
      | otherwise -> do
          let body = extendClosure closure binder (argExpr a)
          when (isExplicit a) $
            logDebugM MaxDetail $ do
              fDoc <- prettyFriendlyInCtx forcedFun
              aDoc <- prettyFriendlyInCtx $ argExpr a
              bDoc <- prettyFriendlyInCtx body
              return $
                "applying " <+> squotes fDoc
                  <> line
                  <> "  to     " <+> squotes aDoc
                  <> line
                  <> "  getting" <+> squotes bDoc
          forceApplication body as
    VPi {} -> illTyped "VPi"
    VRecord {} -> illTyped "VRecord"
    VUniverse {} -> illTyped "VUniverse"
  where
    illTyped e = developerError $ "ill-typed function" <+> e

forceLet ::
  (TypedEvalScheme meta builtin m, MonadNorm builtin m) =>
  GenericBoundEnv meta builtin ->
  Expr builtin ->
  Binder builtin ->
  Expr builtin ->
  m (GenericForcedValue meta builtin)
forceLet env bound binder body = do
  let boundNormExpr = Unforced env bound
  let newBoundEnv = extendEnvWithDefined boundNormExpr binder env
  eval newBoundEnv body

forceRecordAcc ::
  (TypedEvalScheme meta builtin m, MonadNorm builtin m) =>
  GenericThunk meta builtin ->
  GenericThunk meta builtin ->
  FieldName ->
  GenericUnforcedSpine meta builtin ->
  m (GenericForcedValue meta builtin)
forceRecordAcc recordType record field spine = do
  record' <- forceThunk record
  case record' of
    VRecord _ fields -> do
      let fieldValue = lookupRecordFieldS fields field
      forceApplication fieldValue spine
    _ -> return $ VRecordAcc recordType (Forced record') field spine

forceBoundVar ::
  (TypedEvalScheme meta builtin m, MonadNorm builtin m) =>
  GenericBoundEnv meta builtin ->
  Ix ->
  m (GenericForcedValue meta builtin)
forceBoundVar env ix = forceThunk $ lookupIxInEnv env ix

forceFreeVar ::
  forall meta builtin m.
  (TypedEvalScheme meta builtin m, MonadNorm builtin m) =>
  Identifier ->
  GenericUnforcedSpine meta builtin ->
  m (GenericForcedValue meta builtin)
forceFreeVar ident args = do
  decl <- getDeclEntry (Proxy @builtin) ident
  case decl of
    DefFunction _ _ _ _ value -> do
      logDebug MaxDetail $ "substitute" <+> quotePretty (nameOf ident) <+> "for" <+> squotes (prettyFriendlyEmptyCtx value)
      forceApplication (Unforced emptyBoundEnv value) args
    _ -> return $ VFreeVar ident args

forceBuiltin ::
  forall meta builtin m.
  (TypedEvalScheme meta builtin m, MonadNorm builtin m) =>
  builtin ->
  GenericUnforcedSpine meta builtin ->
  m (GenericForcedValue meta builtin)
forceBuiltin b spine = do
  case evalScheme b of
    Eval evalFn -> forceBuiltinEval evalFn b spine
    None -> return $ VBuiltin b spine
    Derived ident -> forceFreeVar ident spine
    TypeClassOperation -> do
      logDebug MaxDetail $ pretty $ length spine
      (inst, remainingArgs) <- findInstanceArg b spine
      forceApplication inst remainingArgs

findInstanceArg :: (MonadLogger m, Show op) => op -> [GenericArg a] -> m (a, [GenericArg a])
findInstanceArg op = \case
  (InstanceArg _ inst : xs) -> return (inst, xs)
  (_ : xs) -> findInstanceArg op xs
  [] -> developerError $ "Malformed type class operation:" <+> pretty (show op)

-----------------------------------------------------------------------------
-- Specialised methods for when the normalised builtins is the same as the
-- unnormalised builtins and has the standard set of datatypes.

forceInEmptyEnv :: (MonadNorm builtin m) => Expr builtin -> m (ForcedValue builtin)
forceInEmptyEnv = eval emptyBoundEnv

-----------------------------------------------------------------------------
-- Other

currentPass :: Doc ()
currentPass = "normalisation by evaluation"

showEntry :: (MonadNorm builtin m) => GenericBoundEnv meta builtin -> Expr builtin -> m ()
showEntry _ _ = return ()

showExit :: (MonadLogger m) => GenericForcedValue meta builtin -> m ()
showExit _ = return ()

{-
showEntry :: (MonadNorm builtin m) => BoundEnv builtin -> Expr builtin -> m ()
showEntry _ctx boundEnv expr = do
  logDebug MaxDetail $ "nbe-entry" <+> prettyFriendly (WithContext expr (boundEnvToCtx boundEnv)) -- <+> "   (ctx =" <+> pretty ctx <> "," <+> "boundEnv =" <+> prettyFriendly (WithContext boundEnv ctx) <+> ")"
  -- logDebug MidDetail $ "nbe-entry" <+> prettyFriendly (WithContext expr (boundEnvToCtx boundEnv)) <+> "   { boundEnv =" <+> prettyFriendly boundEnv <+> "}"
  -- logDebug MidDetail $ "nbe-entry" <+> prettyVerbose expr <+> "   { boundEnv=" <+> prettyVerbose boundEnv <+> "}"
  incrCallDepth
  return ()

showExit :: (MonadNorm builtin m) => ForcedValue builtin -> m ()
showExit ctx result = do
  decrCallDepth
  -- logDebug MidDetail $ "nbe-exit" <+> prettyVerbose result
  logDebug MaxDetail $ "nbe-exit" <+> prettyFriendly (WithContext result ctx)
  return ()
-}
{-
showApp ::
  (MonadNorm builtin m) =>
  FunctionExpr builtin ->
  [UnforcedArg builtin] ->
  m ()
showApp _ _ = return ()
showAppExit :: (MonadLogger m) => m typedExpr -> m typedExpr
showAppExit = id

-}

visibilityError ::
  (HasCallStack, MonadNorm builtin m, MetaLike meta) =>
  GenericThunk meta builtin ->
  GenericArg (GenericThunk meta builtin) ->
  m b
visibilityError fun arg = do
  funDoc <- prettyFriendlyInCtx fun
  argsDoc <- prettyFriendlyInCtx (argExpr arg)
  let visDoc = pretty (visibilityOf arg)
  developerError $
    unexpectedExpr currentPass (visDoc <+> "arg" <+> squotes argsDoc) <+> "Does not match function's visibility:" <> line <> indent 2 funDoc
