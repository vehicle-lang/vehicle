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
    evalClosure,
    traverseClosure,
    traverseClosureGeneric,
    findInstanceArg,
  )
where

import Data.Data (Proxy (..))
import Data.List.NonEmpty as NonEmpty (toList)
import Vehicle.Compile.Context.Bound.Class (MonadBoundContext (..))
import Vehicle.Compile.Context.Free.Class (MonadFreeContext (..), getFreeEnv)
import Vehicle.Compile.Context.Name (MonadNameContext, addNameToContext, getBinderContext)
import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Builtin.Interface.Normalise
  ( EvalScheme (..),
    NormalisableBuiltin (..),
    isBlocked,
  )
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Code.Interface (IsArgs (..))
import Vehicle.Data.Code.Value

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
  normaliseInEnv boundEnv e

normaliseInEnv ::
  (MonadNorm builtin m, MonadFreeContext builtin m) =>
  BoundEnv builtin ->
  Expr builtin ->
  m (Value builtin)
normaliseInEnv boundEnv expr = do
  freeEnv <- getFreeEnv
  eval freeEnv boundEnv expr

normaliseInEmptyEnv ::
  (MonadNorm builtin m, MonadFreeContext builtin m) =>
  Expr builtin ->
  m (Value builtin)
normaliseInEmptyEnv = normaliseInEnv mempty

normaliseApp ::
  (MonadNorm builtin m, MonadFreeContext builtin m) =>
  Value builtin ->
  Spine builtin ->
  m (Value builtin)
normaliseApp fn spine = do
  freeEnv <- getFreeEnv
  evalApp freeEnv fn spine

normaliseBuiltin ::
  (MonadNorm builtin m, MonadFreeContext builtin m) =>
  builtin ->
  Spine builtin ->
  m (Value builtin)
normaliseBuiltin b spine = do
  freeEnv <- getFreeEnv
  evalBuiltin freeEnv b spine

normaliseClosure ::
  (MonadNorm builtin m, MonadFreeContext builtin m) =>
  Lv ->
  VBinder builtin ->
  Closure builtin ->
  m (Value builtin)
normaliseClosure lv binder closure = do
  freeEnv <- getFreeEnv
  evalClosure freeEnv closure (binder, VBoundVar lv [])

-----------------------------------------------------------------------------
-- Evaluation of closures

evalClosure ::
  (MonadNorm builtin m) =>
  FreeEnv builtin ->
  Closure builtin ->
  (VBinder builtin, Value builtin) ->
  m (Value builtin)
evalClosure freeEnv (Closure env body) (binder, arg) = do
  let newEnv = extendEnvWithDefined arg binder env
  eval freeEnv newEnv body

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
  BoundEnv builtin ->
  Expr builtin ->
  m (Value builtin)
eval freeEnv boundEnv expr = do
  showEntry boundEnv expr
  result <- case expr of
    Hole {} -> resolutionError currentPass "Hole"
    Meta _ m -> return $ VMeta m []
    Universe _ u -> return $ VUniverse u
    BoundVar _ v -> return $ lookupIxValueInEnv boundEnv v
    FreeVar _ v -> return $ lookupIdentValueInEnv freeEnv v
    Builtin _ b -> return $ VBuiltin b []
    Lam _ binder body -> do
      binder' <- traverse (eval freeEnv boundEnv) binder
      return $ VLam binder' (Closure boundEnv body)
    Pi _ binder body -> do
      binder' <- traverse (eval freeEnv boundEnv) binder
      return $ VPi binder' (Closure boundEnv body)
    Let _ bound binder body -> do
      binder' <- traverse (eval freeEnv boundEnv) binder
      boundNormExpr <- eval freeEnv boundEnv bound
      let newBoundEnv = extendEnvWithDefined boundNormExpr binder' boundEnv
      eval freeEnv newBoundEnv body
    App fun args -> do
      fun' <- eval freeEnv boundEnv fun
      args' <- traverse (traverse (eval freeEnv boundEnv)) (NonEmpty.toList args)
      evalApp freeEnv fun' args'

  showExit boundEnv result
  return result

evalApp ::
  (MonadNorm builtin m) =>
  FreeEnv builtin ->
  Value builtin ->
  Spine builtin ->
  m (Value builtin)
evalApp _freeEnv fun [] = return fun
evalApp freeEnv fun args@(a : as) = do
  showApp fun args
  result <- case fun of
    VMeta v spine -> return $ VMeta v (spine <> args)
    VBoundVar v spine -> return $ VBoundVar v (spine <> args)
    VFreeVar v spine -> return $ VFreeVar v (spine <> args)
    VBuiltin b spine -> evalBuiltin freeEnv b (spine <> args)
    VLam binder closure
      | not (visibilityMatches binder a) ->
          visibilityError currentPass (prettyVerbose fun) (prettyVerbose args)
      | otherwise -> do
          body' <- evalClosure freeEnv closure (binder, argExpr a)
          evalApp freeEnv body' as
    VUniverse {} -> unexpectedExprError currentPass ("VUniverse" <+> prettyVerbose args)
    VPi {} -> unexpectedExprError currentPass ("VPi" <+> prettyVerbose args)

  showAppExit result
  return result

evalBuiltin ::
  (MonadNorm builtin m) =>
  FreeEnv builtin ->
  builtin ->
  Spine builtin ->
  m (Value builtin)
evalBuiltin freeEnv b spine
  | not (isTypeClassOp b) = case evalScheme b of
      Simple evalFn -> maybe (return $ VBuiltin b spine) evalFn (getExpr accessSpine spine)
      NonSimple evalFn -> maybe (return $ VBuiltin b spine) (evalFn (evalApp freeEnv)) (getExpr accessSpine spine)
      Derived ident -> do
        blocked <- isBlocked b spine
        if blocked
          then return $ VBuiltin b spine
          else evalApp freeEnv (lookupIdentValueInEnv freeEnv ident) spine
      None -> return $ VBuiltin b spine
  | otherwise = do
      (inst, remainingArgs) <- findInstanceArg b spine
      evalApp freeEnv inst remainingArgs

findInstanceArg :: (MonadLogger m, Show op) => op -> [GenericArg a] -> m (a, [GenericArg a])
findInstanceArg op = \case
  (InstanceArg _ _ inst : xs) -> return (inst, xs)
  (_ : xs) -> findInstanceArg op xs
  [] -> developerError $ "Malformed type class operation:" <+> pretty (show op)

lookupIxValueInEnv :: BoundEnv builtin -> Ix -> Value builtin
lookupIxValueInEnv boundEnv ix = do
  snd $ lookupIxInBoundCtx ix boundEnv

-----------------------------------------------------------------------------
-- Other

currentPass :: Doc ()
currentPass = "normalisation by evaluation"

showEntry :: (MonadNorm builtin m) => BoundEnv builtin -> Expr builtin -> m ()
showEntry _ _ = return ()

showExit :: (MonadNorm builtin m) => BoundEnv builtin -> Value builtin -> m ()
showExit _ _ = return ()

{-
showEntry :: (MonadNorm builtin m) => BoundEnv builtin -> Expr builtin -> m ()
showEntry boundEnv expr = do
  logDebug MidDetail $ "nbe-entry" <+> prettyFriendly (WithContext expr (boundEnvToCtx boundEnv))
  -- logDebug MidDetail $ "nbe-entry" <+> prettyVerbose expr -- <+> "   { boundEnv=" <+> prettyVerbose boundEnv <+> "}"
  incrCallDepth
  return ()

showExit :: (MonadNorm builtin m) => BoundEnv builtin -> Value builtin -> m ()
showExit boundEnv result = do
  decrCallDepth
  -- logDebug MidDetail $ "nbe-exit" <+> prettyVerbose result
  logDebug MidDetail $ "nbe-exit" <+> prettyFriendly (WithContext result (boundEnvToCtx boundEnv))
  return ()
-}

showApp :: (MonadNorm builtin m) => Value builtin -> Spine builtin -> m ()
showApp _ _ = return ()

showAppExit :: (MonadNorm builtin m) => Value builtin -> m ()
showAppExit _ = return ()

{-
showApp :: (MonadNorm builtin m) => Value builtin -> Spine builtin -> m ()
showApp fun spine = do
  logDebug MaxDetail $ "nbe-app:" <+> prettyVerbose fun <+> "@" <+> prettyVerbose spine
  incrCallDepth
  return ()

showAppExit :: (MonadNorm builtin m) => Value builtin -> m ()
showAppExit result = do
  decrCallDepth
  logDebug MaxDetail $ "nbe-app-exit:" <+> prettyVerbose result
  return ()
-}

traverseClosure ::
  forall builtin1 builtin2 m.
  (MonadLogger m, MonadNameContext m, NormalisableBuiltin builtin1, PrintableBuiltin builtin2) =>
  (Value builtin1 -> m (Value builtin2)) ->
  FreeEnv builtin1 ->
  VBinder builtin1 ->
  Closure builtin1 ->
  m (Closure builtin2)
traverseClosure traverseValue freeEnv binder closure =
  fst <$> traverseClosureGeneric traverseValue (,()) freeEnv binder closure

traverseClosureGeneric ::
  forall builtin1 builtin2 m a b.
  (MonadLogger m, MonadNameContext m, NormalisableBuiltin builtin1, PrintableBuiltin builtin2) =>
  (Value builtin1 -> m a) ->
  (a -> (Value builtin2, b)) ->
  FreeEnv builtin1 ->
  VBinder builtin1 ->
  Closure builtin1 ->
  m (Closure builtin2, b)
traverseClosureGeneric traverseValue splitResult freeCtx binder (Closure env body) = do
  ctx <- getBinderContext
  let lv = boundCtxLv ctx
  let newEnv = extendEnvWithBound lv binder env
  recResult <- addNameToContext binder $ traverseValue =<< eval freeCtx newEnv body
  let (normBody, remainder) = splitResult recResult
  let finalEnv = boundContextToEnv ctx
  let finalBody = quote mempty (lv + 1) normBody
  return (Closure finalEnv finalBody, remainder)
