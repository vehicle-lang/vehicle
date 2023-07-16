{-# HLINT ignore "Use <|>" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Compile.Normalise.NBE
  ( whnf,
    EvalOptions (..),
    defaultEvalOptions,
    eval,
    evalApp,
    extendEnv,
    extendEnvOverBinder,
    lookupFreeVar,
    forceHead,
    reeval,
    NormT,
    runNormT,
    runEmptyNormT,
    findInstanceArg,
  )
where

import Data.Data (Proxy (..))
import Data.List.NonEmpty as NonEmpty (toList)
import Data.Maybe (fromMaybe, isJust)
import Vehicle.Compile.Arity
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Builtin (Normalisable (..))
import Vehicle.Compile.Normalise.Monad
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta (MetaSet)
import Vehicle.Compile.Type.Meta.Map qualified as MetaMap
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised
import Vehicle.Libraries.StandardLibrary (StdLibFunction (..))

-----------------------------------------------------------------------------
-- Main method

whnf ::
  (MonadNorm builtin m) =>
  Env builtin ->
  Expr Ix builtin ->
  m (Value builtin)
whnf = eval

-----------------------------------------------------------------------------
-- Evaluation

eval :: (MonadNorm builtin m) => Env builtin -> Expr Ix builtin -> m (Value builtin)
eval env expr = do
  showEntry env expr
  result <- case expr of
    Hole {} -> resolutionError currentPass "Hole"
    Meta _ m -> return $ VMeta m []
    Universe _ u -> return $ VUniverse u
    Builtin _ b -> return $ VBuiltin b []
    Ann _ e _ -> eval env e
    Lam _ binder body -> do
      binder' <- evalBinder env binder
      return $ VLam binder' env body
    Pi _ binder body -> do
      binder' <- evalBinder env binder
      let newEnv = extendEnvOverBinder binder env
      body' <- eval newEnv body
      return $ VPi binder' body'
    BoundVar _ i -> do
      (_, value) <- lookupIxInBoundCtx currentPass i env
      return value
    FreeVar _ ident -> lookupFreeVar ident
    Let _ bound binder body -> do
      boundNormExpr <- eval env bound
      let newEnv = extendEnv binder boundNormExpr env
      eval newEnv body
    App _ fun args -> do
      fun' <- eval env fun
      args' <- traverse (traverse (eval env)) (NonEmpty.toList args)
      evalApp fun' args'

  showExit env result
  return result

lookupFreeVar :: forall builtin m. (MonadNorm builtin m) => Identifier -> m (Value builtin)
lookupFreeVar ident = do
  declSubst <- getDeclSubstitution
  let isFiniteQuantifier = ident == identifierOf StdForallIndex || ident == identifierOf StdExistsIndex
  evalFiniteQuants <- evalFiniteQuantifiers <$> getEvalOptions (Proxy @builtin)
  if isFiniteQuantifier && not evalFiniteQuants
    then return $ VFreeVar ident []
    else do
      TypingDeclCtxEntry {..} <- lookupInDeclCtx currentPass ident declSubst
      case declBody of
        Just expr | isInlinable declAnns -> return expr
        _ -> return $ VFreeVar ident []

evalBinder :: (MonadNorm builtin m) => Env builtin -> Binder Ix builtin -> m (VBinder builtin)
evalBinder env = traverse (eval env)

evalApp :: (MonadNorm builtin m) => Value builtin -> Spine builtin -> m (Value builtin)
evalApp fun [] = return fun
evalApp fun args@(a : as) = do
  showApp fun args
  result <- case fun of
    VMeta v spine -> return $ VMeta v (spine <> args)
    VBoundVar v spine -> return $ VBoundVar v (spine <> args)
    VFreeVar v spine -> evalFreeVarApp v (spine <> args)
    VLam binder env body
      | not (visibilityMatches binder a) -> do
          compilerDeveloperError $
            "Visibility mismatch during normalisation:"
              <> line
              <> indent
                2
                ( "fun:" <+> prettyVerbose fun
                    <> line
                    <> "args:" <+> prettyVerbose args
                )
      | otherwise -> do
          let newEnv = extendEnv binder (argExpr a) env
          body' <- eval newEnv body
          case as of
            [] -> return body'
            (b : bs) -> evalApp body' (b : bs)
    VBuiltin b spine
      | not (isTypeClassOp b) -> do
          evalBuiltin evalApp b (spine <> args)
      | otherwise -> do
          (inst, remainingArgs) <- findInstanceArg b args
          evalApp inst remainingArgs
    VUniverse {} -> unexpectedExprError currentPass ("VUniverse" <+> prettyVerbose args)
    VPi {} -> unexpectedExprError currentPass ("VPi" <+> prettyVerbose args)

  showAppExit result
  return result

-- | This evaluates a free variable applied to an application.
evalFreeVarApp ::
  (MonadNorm builtin m) =>
  Identifier ->
  Spine builtin ->
  m (Value builtin)
evalFreeVarApp ident spine = do
  declSubst <- getDeclSubstitution
  TypingDeclCtxEntry {..} <- lookupInDeclCtx currentPass ident declSubst
  case declBody of
    -- If free variable was annotated with a `@noinline` annotation but all
    -- it's explicit arguments are actually values then we should actually
    -- substitute it through and evaluate.
    Just expr | not (isInlinable declAnns) && length spine == arityFromVType (normalised declType) -> do
      let allExplicitArgsAreValues = all (isFullyReduced . argExpr) $ filter isExplicit spine
      if allExplicitArgsAreValues
        then evalApp expr spine
        else return $ VFreeVar ident spine
    _ -> return $ VFreeVar ident spine

isFullyReduced :: (Normalisable builtin) => Value builtin -> Bool
isFullyReduced = \case
  VUniverse {} -> True
  VLam {} -> True
  VPi {} -> True
  VMeta {} -> False
  VFreeVar {} -> False
  VBoundVar {} -> False
  VBuiltin b _ -> isValue b

-----------------------------------------------------------------------------
-- Reevaluation

reeval ::
  (MonadNorm builtin m) =>
  Env builtin ->
  Value builtin ->
  m (Value builtin)
reeval env expr = do
  showNormEntry env expr
  result <- case expr of
    VUniverse {} -> return expr
    VLam binder lamEnv body -> do
      lamEnv' <- traverse (\(a, b) -> (a,) <$> reeval env b) lamEnv
      return $ VLam binder lamEnv' body
    VPi {} -> return expr
    VMeta m spine -> VMeta m <$> reevalSpine env spine
    VFreeVar v spine -> do
      value <- lookupFreeVar v
      spine' <- reevalSpine env spine
      evalApp value spine'
    VBoundVar v spine -> do
      (_, value) <- lookupLvInBoundCtx currentPass v env
      spine' <- reevalSpine env spine
      evalApp value spine'
    VBuiltin b spine ->
      evalBuiltin evalApp b =<< reevalSpine env spine
  showNormExit env result
  return result

reevalSpine :: (MonadNorm builtin m) => Env builtin -> Spine builtin -> m (Spine builtin)
reevalSpine env = traverse (traverse (reeval env))

-----------------------------------------------------------------------------
-- Meta-variable forcing

-- | Recursively forces the evaluation of any meta-variables at the head
-- of the expresson.
forceHead :: (MonadNorm builtin m) => ConstraintContext builtin -> Value builtin -> m (Value builtin, MetaSet)
forceHead ctx expr = do
  (maybeForcedExpr, blockingMetas) <- forceExpr expr
  forcedExpr <- case maybeForcedExpr of
    Nothing -> return expr
    Just forcedExpr -> do
      let dbCtx = boundContextOf ctx
      logDebug MaxDetail $ "forced" <+> prettyFriendly (WithContext expr dbCtx) <+> "to" <+> prettyFriendly (WithContext forcedExpr dbCtx)
      return forcedExpr
  return (forcedExpr, blockingMetas)

-- | Recursively forces the evaluation of any meta-variables that are blocking
-- evaluation.
forceExpr :: forall builtin m. (MonadNorm builtin m) => Value builtin -> m (Maybe (Value builtin), MetaSet)
forceExpr = go
  where
    go :: Value builtin -> m (Maybe (Value builtin), MetaSet)
    go = \case
      VMeta m spine -> goMeta m spine
      VBuiltin b spine -> forceBuiltin evalApp forceArg b spine
      _ -> return (Nothing, mempty)

    goMeta :: MetaID -> Spine builtin -> m (Maybe (Value builtin), MetaSet)
    goMeta m spine = do
      metaSubst <- getMetaSubstitution
      case MetaMap.lookup m metaSubst of
        Just solution -> do
          normMetaExpr <- evalApp (normalised solution) spine
          (maybeForcedExpr, blockingMetas) <- go normMetaExpr
          let forcedExpr = maybe (Just normMetaExpr) Just maybeForcedExpr
          return (forcedExpr, blockingMetas)
        Nothing -> return (Nothing, MetaSet.singleton m)

forceArg :: (MonadNorm builtin m) => VArg builtin -> m (VArg builtin, (Bool, MetaSet))
forceArg arg = do
  (maybeResult, blockingMetas) <- unpairArg <$> traverse forceExpr arg
  let result = fmap (fromMaybe (argExpr arg)) maybeResult
  let reduced = isJust $ argExpr maybeResult
  return (result, (reduced, blockingMetas))

-----------------------------------------------------------------------------
-- Other

currentPass :: Doc ()
currentPass = "normalisation by evaluation"

showEntry :: (MonadNorm builtin m) => Env builtin -> Expr Ix builtin -> m ()
showEntry _env _expr = do
  -- logDebug MidDetail $ "nbe-entry" <+> prettyVerbose expr <+> "   { env=" <+> prettyVerbose env <+> "}"
  -- logDebug MidDetail $ "nbe-entry" <+> prettyFriendly (WithContext expr (fmap fst env)) <+> "   { env=" <+> hang 0 (prettyVerbose env) <+> "}"
  -- incrCallDepth
  return ()

showExit :: (MonadNorm builtin m) => Env builtin -> Value builtin -> m ()
showExit _env _result = do
  -- decrCallDepth
  -- logDebug MidDetail $ "nbe-exit" <+> prettyVerbose result
  -- logDebug MidDetail $ "nbe-exit" <+> prettyFriendly (WithContext result (fmap fst env))
  return ()

showNormEntry :: (MonadNorm builtin m) => Env builtin -> Value builtin -> m ()
showNormEntry _env _expr = do
  -- logDebug MidDetail $ "reeval-entry" <+> prettyVerbose expr -- <+> "   { env=" <+> prettyVerbose env <+> "}"
  -- logDebug MidDetail $ "reeval-entry" <+> prettyFriendly (WithContext expr (fmap fst env)) -- <+> "   { env=" <+> hang 0 (prettyVerbose env) <+> "}"
  -- incrCallDepth
  return ()

showNormExit :: (MonadNorm builtin m) => Env builtin -> Value builtin -> m ()
showNormExit _env _result = do
  -- decrCallDepth
  -- logDebug MidDetail $ "reeval-exit" <+> prettyVerbose result
  -- logDebug MidDetail $ "reeval-exit" <+> prettyFriendly (WithContext result (fmap fst env))
  return ()

showApp :: (MonadNorm builtin m) => Value builtin -> Spine builtin -> m ()
showApp _fun _spine = do
  -- logDebug MaxDetail $ "nbe-app:" <+> prettyVerbose fun <+> "@" <+> prettyVerbose spine
  -- incrCallDepth
  return ()

showAppExit :: (MonadNorm builtin m) => Value builtin -> m ()
showAppExit _result = do
  -- decrCallDepth
  -- logDebug MaxDetail $ "nbe-app-exit:" <+> prettyVerbose result
  return ()

findInstanceArg :: (MonadCompile m) => (Show op) => op -> [GenericArg a] -> m (a, [GenericArg a])
findInstanceArg op = \case
  (InstanceArg _ _ inst : xs) -> return (inst, xs)
  (_ : xs) -> findInstanceArg op xs
  [] -> compilerDeveloperError $ "Malformed type class operation:" <+> pretty (show op)
