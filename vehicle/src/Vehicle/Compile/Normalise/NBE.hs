module Vehicle.Compile.Normalise.NBE
  ( MonadNorm,
    FreeEnv,
    normalise,
    normaliseInEmptyFreeEnv,
    normaliseAppInEmptyFreeEnv,
    normaliseInFreeCtx,
    normaliseApp,
    evalBuiltin,
    normaliseClosure,
    normaliseClosureInCtx,
    evalDecl,
    eval,
    evalInEmptyEnv,
    evalApp,
    findInstanceArg,
  )
where

import Data.Data (Proxy (..))
import Data.List.NonEmpty as NonEmpty (toList)
import Data.Map.Ordered.Strict qualified as OMap
import GHC.Stack (HasCallStack)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Builtin.Interface.Normalise
  ( EvalScheme (..),
    NormalisableBuiltin (..),
  )
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Code.Interface (IsArgs (..))
import Vehicle.Data.Code.Value
import Vehicle.Data.Variable.Bound.Context.Generic
import Vehicle.Data.Variable.Bound.Context.Name.Class (MonadReadableNameContext (getNameContext))
import Vehicle.Data.Variable.Bound.Context.Name.Core
import Vehicle.Data.Variable.Free.Context.Class (MonadFreeContext (..))
import Vehicle.Data.Variable.Free.Context.Instance (runFreeContextT, runFreshFreeContextT)

-- NOTE: there is no evaluatation to NF in this file. To do it
-- efficiently you should just evaluate to WHNF and then recursively
-- evaluate as required.

-----------------------------------------------------------------------------
-- Specialised methods for when the normalised builtins is the same as the
-- unnormalised builtins and has the standard set of datatypes.

normalise ::
  forall builtin m.
  (MonadNorm builtin m, MonadBoundContext (Type builtin) m, MonadFreeContext builtin m) =>
  Expr builtin ->
  m (Value builtin)
normalise e = do
  boundCtx <- getBoundCtx (Proxy @(Type builtin))
  let boundEnv = boundContextToEnv boundCtx
  eval (toNamedBoundCtx boundCtx) boundEnv e

normaliseInFreeCtx ::
  (MonadNorm builtin m) =>
  FreeCtx builtin ->
  NamedBoundCtx ->
  BoundEnv builtin ->
  Expr builtin ->
  m (Value builtin)
normaliseInFreeCtx freeCtx ctx boundEnv expr = do
  runFreeContextT freeCtx $ eval ctx boundEnv expr

normaliseInEmptyFreeEnv ::
  forall builtin m.
  (MonadNorm builtin m) =>
  NamedBoundCtx ->
  BoundEnv builtin ->
  Expr builtin ->
  m (Value builtin)
normaliseInEmptyFreeEnv ctx env expr =
  runFreshFreeContextT (Proxy @builtin) $ eval ctx env expr

normaliseApp ::
  (MonadNorm builtin m, MonadFreeContext builtin m) =>
  NamedBoundCtx ->
  Value builtin ->
  Spine builtin ->
  m (Value builtin)
normaliseApp ctx fn spine = do
  evalApp ctx fn spine

normaliseAppInEmptyFreeEnv ::
  forall builtin m.
  (MonadNorm builtin m) =>
  NamedBoundCtx ->
  Value builtin ->
  Spine builtin ->
  m (Value builtin)
normaliseAppInEmptyFreeEnv ctx fn spine = do
  runFreshFreeContextT (Proxy @builtin) $ evalApp ctx fn spine

normaliseClosureInCtx ::
  (MonadNorm builtin m, MonadFreeContext builtin m) =>
  NamedBoundCtx ->
  VBinder builtin ->
  Closure builtin ->
  m (Value builtin)
normaliseClosureInCtx ctx binder (Closure env body) = do
  let newEnv = extendEnvWithBound (boundCtxLv ctx) binder env
  eval (nameOf binder : ctx) newEnv body

normaliseClosure ::
  (MonadNorm builtin m, MonadFreeContext builtin m, MonadReadableNameContext m) =>
  VBinder builtin ->
  Closure builtin ->
  m (Value builtin)
normaliseClosure binder closure = do
  ctx <- getNameContext
  normaliseClosureInCtx ctx binder closure

-----------------------------------------------------------------------------
-- Evaluation

type MonadNorm builtin m =
  ( MonadLogger m,
    NormalisableBuiltin builtin,
    PrintableBuiltin builtin
  )

evalDecl ::
  (MonadNorm builtin m, MonadFreeContext builtin m) =>
  Decl builtin ->
  m (VDecl builtin)
evalDecl d = case d of
  DefAbstract {} -> traverse evalInEmptyEnv d
  DefFunction {} -> traverse evalInEmptyEnv d
  DefRecord p ident sort telescope fields -> do
    (telescope', fields') <- evalRecordDef (telescope, fields)
    return $ DefRecord p ident sort telescope' fields'

evalInEmptyEnv ::
  (MonadNorm builtin m, MonadFreeContext builtin m) =>
  Expr builtin ->
  m (Value builtin)
evalInEmptyEnv = eval mempty emptyBoundEnv

evalRecordDef ::
  forall builtin m.
  (MonadNorm builtin m, MonadFreeContext builtin m) =>
  (Telescope builtin, RecordFields builtin) ->
  m (VTelescope builtin, GenericRecordFields (Value builtin))
evalRecordDef = go mempty emptyBoundEnv
  where
    go ::
      NamedBoundCtx ->
      BoundEnv builtin ->
      (Telescope builtin, RecordFields builtin) ->
      m (VTelescope builtin, GenericRecordFields (Value builtin))
    go ctx boundEnv (telescope, fields) = case telescope of
      binder : binders -> do
        binder' <- traverse (eval ctx boundEnv) binder
        let newEnv = extendEnvWithBound (boundCtxLv ctx) binder boundEnv
        let newCtx = nameOf binder : ctx
        (binders', fields') <- go newCtx newEnv (binders, fields)
        return (binder' : binders', fields')
      [] -> do
        fields' <- traverseRecordFields (eval ctx boundEnv) fields
        return ([], fields')

eval ::
  (MonadNorm builtin m, MonadFreeContext builtin m) =>
  NamedBoundCtx ->
  BoundEnv builtin ->
  Expr builtin ->
  m (Value builtin)
eval ctx boundEnv expr = do
  showEntry ctx boundEnv expr
  let recEval = eval ctx boundEnv
  result <- case expr of
    Hole {} -> resolutionError currentPass "Hole"
    Meta _ m -> return $ VMeta m []
    Universe _ u -> return $ VUniverse u
    BoundVar _ v -> return $ lookupIxInEnv boundEnv v
    FreeVar _ v -> lookupIdentValue v -- I think this could be the issue?
    -- lookupIdentValue eventually does a getDeclEntry which then does lookupInFreeCtx
    Builtin _ b -> return $ VBuiltin b []
    Lam _ binder body -> do
      binder' <- traverse recEval binder
      return $ VLam binder' (Closure boundEnv body)
    Pi _ binder body -> do
      binder' <- traverse recEval binder
      return $ VPi binder' (Closure boundEnv body)
    Let _ bound binder body -> do
      binder' <- traverse recEval binder
      boundNormExpr <- recEval bound
      let newBoundEnv = extendEnvWithDefined boundNormExpr binder' boundEnv
      eval ctx newBoundEnv body
    App fun args -> do
      fun' <- recEval fun
      args' <- traverse (traverse recEval) (NonEmpty.toList args)
      evalApp ctx fun' args'
    Record _p recordType fields -> do
      recordType' <- recEval recordType
      fields' <- traverseRecordFields recEval fields
      return $ VRecord recordType' $ OMap.fromList fields'
    RecordProj _p recordType record field -> do
      record' <- recEval record
      case record' of
        VRecord _ fields -> return $ lookupRecordFieldS fields field
        _ -> do
          recordType' <- recEval recordType
          return $ VRecordAcc recordType' record' field []

  showExit ctx result
  return result

evalApp ::
  (MonadNorm builtin m, MonadFreeContext builtin m) =>
  NamedBoundCtx ->
  Value builtin ->
  Spine builtin ->
  m (Value builtin)
evalApp _ctx fun [] = return fun
evalApp ctx fun args@(a : as) = do
  showApp ctx fun args
  result <- case fun of
    VMeta v spine -> return $ VMeta v (spine <> args)
    VBoundVar v spine -> return $ VBoundVar v (spine <> args)
    VFreeVar v spine -> return $ VFreeVar v (spine <> args)
    VRecordAcc recordType record field spine -> return $ VRecordAcc recordType record field (spine <> args)
    VBuiltin b spine -> evalBuiltin ctx b (spine <> args)
    VLam binder (Closure env body)
      | not (visibilityMatches binder a) ->
          visibilityError ctx fun a
      | otherwise -> do
          let newEnv = extendEnvWithDefined (argExpr a) binder env
          body' <- eval ctx newEnv body
          evalApp ctx body' as
    VUniverse {} -> unexpected "VUniverse"
    VPi {} -> unexpected "VPi"
    VRecord {} -> unexpected "VRecord"
  showAppExit ctx result
  return result
  where
    unexpected name = unexpectedExprError currentPass (name <+> prettyVerbose args)

evalBuiltin ::
  (MonadNorm builtin m, MonadFreeContext builtin m) =>
  NamedBoundCtx ->
  builtin ->
  Spine builtin ->
  m (Value builtin)
evalBuiltin ctx b spine
  | not (isTypeClassOp b) = case evalScheme b of
      Simple evalFn -> maybe (return $ VBuiltin b spine) evalFn (getExpr accessSpine spine)
      NonSimple evalFn -> maybe (return $ VBuiltin b spine) (evalFn ctx evalApp eval) (getExpr accessSpine spine)
      Derived ident -> do
        value <- lookupIdentValue ident
        evalApp ctx value spine
      None -> return $ VBuiltin b spine
  | otherwise = do
      (inst, remainingArgs) <- findInstanceArg b spine
      evalApp ctx inst remainingArgs

lookupIdentValue :: forall builtin m. (MonadFreeContext builtin m) => Identifier -> m (Value builtin)
lookupIdentValue ident = do
  decl <- getDeclEntry (Proxy @builtin) ident
  return $ case decl of
    DefFunction _ _ _ _ value -> value
    _ -> VFreeVar ident []

findInstanceArg :: (MonadLogger m, Show op) => op -> [GenericArg a] -> m (a, [GenericArg a])
findInstanceArg op = \case
  (InstanceArg _ inst : xs) -> return (inst, xs)
  (_ : xs) -> findInstanceArg op xs
  [] -> developerError $ "Malformed type class operation:" <+> pretty (show op)

-----------------------------------------------------------------------------
-- Other

currentPass :: Doc ()
currentPass = "normalisation by evaluation"

showEntry :: (MonadNorm builtin m) => NamedBoundCtx -> BoundEnv builtin -> Expr builtin -> m ()
showEntry _ _ _ = return ()

showExit :: (MonadNorm builtin m) => NamedBoundCtx -> Value builtin -> m ()
showExit _ _ = return ()

{-
showEntry :: (MonadNorm builtin m) => NamedBoundCtx -> BoundEnv builtin -> Expr builtin -> m ()
showEntry _ctx boundEnv expr = do
  logDebug MaxDetail $ "nbe-entry" <+> prettyFriendly (WithContext expr (boundEnvToCtx boundEnv)) -- <+> "   (ctx =" <+> pretty ctx <> "," <+> "boundEnv =" <+> prettyFriendly (WithContext boundEnv ctx) <+> ")"
  -- logDebug MidDetail $ "nbe-entry" <+> prettyFriendly (WithContext expr (boundEnvToCtx boundEnv)) <+> "   { boundEnv =" <+> prettyFriendly boundEnv <+> "}"
  -- logDebug MidDetail $ "nbe-entry" <+> prettyVerbose expr <+> "   { boundEnv=" <+> prettyVerbose boundEnv <+> "}"
  incrCallDepth
  return ()

showExit :: (MonadNorm builtin m) => NamedBoundCtx -> Value builtin -> m ()
showExit ctx result = do
  decrCallDepth
  -- logDebug MidDetail $ "nbe-exit" <+> prettyVerbose result
  logDebug MaxDetail $ "nbe-exit" <+> prettyFriendly (WithContext result ctx)
  return ()
-}

showApp :: (MonadNorm builtin m) => NamedBoundCtx -> Value builtin -> Spine builtin -> m ()
showApp _ _ _ = return ()

showAppExit :: (MonadNorm builtin m) => NamedBoundCtx -> Value builtin -> m ()
showAppExit _ _ = return ()

{-
showApp :: (MonadNorm builtin m) => NamedBoundCtx -> Value builtin -> Spine builtin -> m ()
showApp _ctx fun spine = do
  logDebug MaxDetail $ "nbe-app:" <+> prettyVerbose fun <+> "@" <+> prettyVerbose spine
  incrCallDepth
  return ()

showAppExit :: (MonadNorm builtin m) => NamedBoundCtx -> Value builtin -> m ()
showAppExit _ctx result = do
  decrCallDepth
  logDebug MaxDetail $ "nbe-app-exit:" <+> prettyVerbose result
  return ()
-}

visibilityError ::
  (HasCallStack, MonadNorm builtin m) =>
  NamedBoundCtx ->
  Value builtin ->
  VArg builtin ->
  m b
visibilityError ctx fun arg = do
  let funDoc = prettyFriendly (WithContext fun ctx)
  let argsDoc = prettyFriendly (WithContext (argExpr arg) ctx)
  let visDoc = pretty (visibilityOf arg)
  developerError $
    unexpectedExpr currentPass (visDoc <+> "arg" <+> squotes argsDoc) <+> "Does not match function's visibility:" <> line <> indent 2 funDoc
