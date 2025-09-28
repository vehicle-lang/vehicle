module Vehicle.Compile.Normalise.NBE
  ( MonadNorm,
    FreeEnv,
    NormalisableBuiltin,
    normalise,
    normaliseInEnv,
    normaliseInEmptyEnv,
    normaliseApp,
    normaliseBuiltin,
    normaliseClosure,
    eval,
    evalApp,
    findInstanceArg,
  )
where

import Data.Data (Proxy (..))
import Data.List.NonEmpty as NonEmpty (toList)
import Data.Map.Ordered.Strict qualified as OMap
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
import Vehicle.Data.Variable.Bound.Context.Class (MonadBoundContext (..))
import Vehicle.Data.Variable.Free.Context.Class (MonadFreeContext (..), getFreeEnv)

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
  normaliseInEnv (toNamedBoundCtx boundCtx) boundEnv e

normaliseInEnv ::
  (MonadNorm builtin m, MonadFreeContext builtin m) =>
  NamedBoundCtx ->
  BoundEnv builtin ->
  Expr builtin ->
  m (Value builtin)
normaliseInEnv ctx boundEnv expr = do
  freeEnv <- getFreeEnv
  eval freeEnv ctx boundEnv expr

normaliseInEmptyEnv ::
  (MonadNorm builtin m, MonadFreeContext builtin m) =>
  Expr builtin ->
  m (Value builtin)
normaliseInEmptyEnv = normaliseInEnv mempty emptyBoundEnv

normaliseApp ::
  (MonadNorm builtin m, MonadFreeContext builtin m) =>
  NamedBoundCtx ->
  Value builtin ->
  Spine builtin ->
  m (Value builtin)
normaliseApp ctx fn spine = do
  freeEnv <- getFreeEnv
  evalApp freeEnv ctx fn spine

normaliseBuiltin ::
  (MonadNorm builtin m, MonadFreeContext builtin m) =>
  NamedBoundCtx ->
  builtin ->
  Spine builtin ->
  m (Value builtin)
normaliseBuiltin ctx b spine = do
  freeEnv <- getFreeEnv
  evalBuiltin freeEnv ctx b spine

normaliseClosure ::
  (MonadNorm builtin m, MonadFreeContext builtin m) =>
  NamedBoundCtx ->
  VBinder builtin ->
  Closure builtin ->
  m (Value builtin)
normaliseClosure ctx binder (Closure env body) = do
  freeEnv <- getFreeEnv
  let newEnv = extendEnvWithBound (boundCtxLv ctx) binder env
  eval freeEnv ctx newEnv body

-----------------------------------------------------------------------------
-- Evaluation

type MonadNorm builtin m =
  ( MonadLogger m,
    NormalisableBuiltin builtin,
    PrintableBuiltin builtin
  )

eval ::
  (MonadNorm builtin m) =>
  FreeEnv builtin ->
  NamedBoundCtx ->
  BoundEnv builtin ->
  Expr builtin ->
  m (Value builtin)
eval freeEnv ctx boundEnv expr = do
  showEntry ctx boundEnv expr
  let recEval = eval freeEnv ctx boundEnv
  result <- case expr of
    Hole {} -> resolutionError currentPass "Hole"
    Meta _ m -> return $ VMeta m []
    Universe _ u -> return $ VUniverse u
    BoundVar _ v -> return $ lookupIxInEnv boundEnv v
    FreeVar _ v -> return $ lookupIdentValueInEnv freeEnv v
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
      eval freeEnv ctx newBoundEnv body
    App fun args -> do
      fun' <- recEval fun
      args' <- traverse (traverse recEval) (NonEmpty.toList args)
      evalApp freeEnv ctx fun' args'
    Record _p ident fields -> do
      fields' <- traverseRecordFields recEval fields
      return $ VRecord ident $ OMap.fromList fields'
    RecordAcc _p record fieldRef@(_i, field) -> do
      record' <- recEval record
      case record' of
        VRecord _ fields -> return $ lookupRecordFieldS fields field
        _ -> return $ VRecordAcc record' fieldRef

  showExit ctx result
  return result

evalApp ::
  (MonadNorm builtin m) =>
  FreeEnv builtin ->
  NamedBoundCtx ->
  Value builtin ->
  Spine builtin ->
  m (Value builtin)
evalApp _freeEnv _ctx fun [] = return fun
evalApp freeEnv ctx fun args@(a : as) = do
  showApp ctx fun args
  result <- case fun of
    VMeta v spine -> return $ VMeta v (spine <> args)
    VBoundVar v spine -> return $ VBoundVar v (spine <> args)
    VFreeVar v spine -> return $ VFreeVar v (spine <> args)
    VBuiltin b spine -> evalBuiltin freeEnv ctx b (spine <> args)
    VLam binder (Closure env body)
      | not (visibilityMatches binder a) ->
          visibilityError currentPass (prettyVerbose fun) (prettyVerbose args)
      | otherwise -> do
          let newEnv = extendEnvWithDefined (argExpr a) binder env
          body' <- eval freeEnv ctx newEnv body
          evalApp freeEnv ctx body' as
    VUniverse {} -> unexpected "VUniverse"
    VPi {} -> unexpected "VUniverse"
    VRecord {} -> unexpected "VUniverse"
    VRecordAcc {} -> unexpected "VUniverse"
  showAppExit ctx result
  return result
  where
    unexpected name = unexpectedExprError currentPass (name <+> prettyVerbose args)

evalBuiltin ::
  (MonadNorm builtin m) =>
  FreeEnv builtin ->
  NamedBoundCtx ->
  builtin ->
  Spine builtin ->
  m (Value builtin)
evalBuiltin freeEnv ctx b spine
  | not (isTypeClassOp b) = case evalScheme b of
      Simple evalFn -> maybe (return $ VBuiltin b spine) evalFn (getExpr accessSpine spine)
      NonSimple evalFn -> maybe (return $ VBuiltin b spine) (evalFn ctx (evalApp freeEnv) (eval freeEnv)) (getExpr accessSpine spine)
      Derived ident -> evalApp freeEnv ctx (lookupIdentValueInEnv freeEnv ident) spine
      None -> return $ VBuiltin b spine
  | otherwise = do
      (inst, remainingArgs) <- findInstanceArg b spine
      evalApp freeEnv ctx inst remainingArgs

findInstanceArg :: (MonadLogger m, Show op) => op -> [GenericArg a] -> m (a, [GenericArg a])
findInstanceArg op = \case
  (InstanceArg _ _ inst : xs) -> return (inst, xs)
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
showEntry ctx boundEnv expr = do
  logDebug MidDetail $ "nbe-entry" <+> prettyFriendly (WithContext expr (boundEnvToCtx boundEnv)) <+> "   (ctx =" <+> pretty ctx <> "," <+> "boundEnv =" <+> prettyFriendly (WithContext boundEnv ctx) <+> ")"
  -- logDebug MidDetail $ "nbe-entry" <+> prettyFriendly (WithContext expr (boundEnvToCtx boundEnv)) <+> "   { boundEnv =" <+> prettyFriendly boundEnv <+> "}"
  -- logDebug MidDetail $ "nbe-entry" <+> prettyVerbose expr -- <+> "   { boundEnv=" <+> prettyVerbose boundEnv <+> "}"
  incrCallDepth
  return ()

showExit :: (MonadNorm builtin m) => NamedBoundCtx -> Value builtin -> m ()
showExit ctx result = do
  decrCallDepth
  -- logDebug MidDetail $ "nbe-exit" <+> prettyVerbose result
  logDebug MidDetail $ "nbe-exit" <+> prettyFriendly (WithContext result ctx)
  return ()
-}
showApp :: (MonadNorm builtin m) => NamedBoundCtx -> Value builtin -> Spine builtin -> m ()
showApp _ _ _ = return ()

showAppExit :: (MonadNorm builtin m) => NamedBoundCtx -> Value builtin -> m ()
showAppExit _ _ = return ()

{-
showApp :: (MonadNorm builtin m) => NamedBoundCtx -> Value builtin -> Spine builtin -> m ()
showApp ctx fun spine = do
  logDebug MaxDetail $ "nbe-app:" <+> prettyVerbose fun <+> "@" <+> prettyVerbose spine
  incrCallDepth
  return ()

showAppExit :: (MonadNorm builtin m) => NamedBoundCtx -> Value builtin -> m ()
showAppExit ctx result = do
  decrCallDepth
  logDebug MaxDetail $ "nbe-app-exit:" <+> prettyVerbose result
  return ()
-}
