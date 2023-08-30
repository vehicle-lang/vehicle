{-# HLINT ignore "Use <|>" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Compile.Normalise.NBE
  ( NBEOptions (..),
    defaultNBEOptions,
    eval,
    evalApp,
    extendEnv,
    extendEnvOverBinder,
    lookupIdentValueInEnv,
    reeval,
    findInstanceArg,
  )
where

import Data.Data (Proxy (..))
import Data.List.NonEmpty as NonEmpty (toList)
import Data.Maybe (isNothing)
import Vehicle.Compile.Arity
import Vehicle.Compile.Context.Free.Class (MonadFreeContext, getDecl)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Builtin (evalBuiltin)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Data.BuiltinInterface
import Vehicle.Data.NormalisedExpr
import Vehicle.Libraries.StandardLibrary.Definitions (StdLibFunction (..))

-----------------------------------------------------------------------------
-- Options during NBE

newtype NBEOptions = NBEOptions
  { evalFiniteQuantifiers :: Bool
  }

defaultNBEOptions :: NBEOptions
defaultNBEOptions =
  NBEOptions
    { evalFiniteQuantifiers = True
    }

-----------------------------------------------------------------------------
-- Evaluation

type MonadNorm builtin m = MonadFreeContext builtin m

eval ::
  (MonadNorm builtin m) =>
  NBEOptions ->
  Env builtin ->
  Expr Ix builtin ->
  m (Value builtin)
eval opts env expr = do
  showEntry env expr
  result <- case expr of
    Hole {} -> resolutionError currentPass "Hole"
    Meta _ m -> return $ VMeta m []
    Universe _ u -> return $ VUniverse u
    Builtin _ b -> return $ VBuiltin b []
    Lam _ binder body -> do
      binder' <- evalBinder opts env binder
      return $ VLam binder' env body
    Pi _ binder body -> do
      binder' <- evalBinder opts env binder
      let newEnv = extendEnvOverBinder binder' env
      body' <- eval opts newEnv body
      return $ VPi binder' body'
    BoundVar _ i ->
      lookupIxValueInEnv i env
    FreeVar _ ident ->
      lookupIdentValueInEnv opts ident
    Let _ bound binder body -> do
      binder' <- evalBinder opts env binder
      boundNormExpr <- eval opts env bound
      let newEnv = extendEnv binder' boundNormExpr env
      eval opts newEnv body
    App _ fun args -> do
      fun' <- eval opts env fun
      args' <- traverse (traverse (eval opts env)) (NonEmpty.toList args)
      evalApp opts fun' args'

  showExit env result
  return result

evalBinder ::
  (MonadNorm builtin m) =>
  NBEOptions ->
  Env builtin ->
  Binder Ix builtin ->
  m (VBinder builtin)
evalBinder opts env = traverse (eval opts env)

evalApp ::
  (MonadNorm builtin m) =>
  NBEOptions ->
  Value builtin ->
  Spine builtin ->
  m (Value builtin)
evalApp _opts fun [] = return fun
evalApp opts fun args@(a : as) = do
  showApp fun args
  result <- case fun of
    VMeta v spine -> return $ VMeta v (spine <> args)
    VBoundVar v spine -> return $ VBoundVar v (spine <> args)
    VFreeVar v spine -> evalFreeVarApp opts v (spine <> args)
    VLam binder env body
      | not (visibilityMatches binder a) -> do
          compilerDeveloperError $
            "Visibility mismatch during normalisation:"
              <> line
              <> indent
                2
                ( "fun:"
                    <+> prettyVerbose fun
                    <> line
                    <> "args:"
                      <+> prettyVerbose args
                )
      | otherwise -> do
          let newEnv = extendEnv binder (argExpr a) env
          body' <- eval opts newEnv body
          case as of
            [] -> return body'
            (b : bs) -> evalApp opts body' (b : bs)
    VBuiltin b spine
      | not (isTypeClassOp b) -> do
          evalBuiltin (evalApp opts) b (spine <> args)
      | otherwise -> do
          (inst, remainingArgs) <- findInstanceArg b args
          evalApp opts inst remainingArgs
    VUniverse {} -> unexpectedExprError currentPass ("VUniverse" <+> prettyVerbose args)
    VPi {} -> unexpectedExprError currentPass ("VPi" <+> prettyVerbose args)

  showAppExit result
  return result

-- | This evaluates a free variable applied to an application.
evalFreeVarApp ::
  forall builtin m.
  (MonadNorm builtin m) =>
  NBEOptions ->
  Identifier ->
  Spine builtin ->
  m (Value builtin)
evalFreeVarApp opts ident spine = do
  decl <- getDecl (Proxy @builtin) currentPass ident
  case bodyOf decl of
    -- If free variable was annotated with a `@noinline` annotation but all
    -- it's explicit arguments are actually values then we should actually
    -- substitute it through and evaluate.
    Just expr | not (isInlinable (annotationsOf decl)) && length spine == arityFromVType (normalised (typeOf decl)) -> do
      let allExplicitArgsAreValues = all (isFullyReduced . argExpr) $ filter isExplicit spine
      if allExplicitArgsAreValues
        then evalApp opts (normalised expr) spine
        else return $ VFreeVar ident spine
    _ -> return $ VFreeVar ident spine

isFullyReduced :: (HasStandardData builtin) => Value builtin -> Bool
isFullyReduced = \case
  VUniverse {} -> True
  VLam {} -> True
  VPi {} -> True
  VMeta {} -> False
  VFreeVar {} -> False
  VBoundVar {} -> False
  VBuiltin b _ -> isNothing $ getBuiltinFunction b

lookupIdentValueInEnv ::
  forall builtin m.
  (MonadNorm builtin m) =>
  NBEOptions ->
  Identifier ->
  m (Value builtin)
lookupIdentValueInEnv opts ident = do
  let isFiniteQuantifier = ident == identifierOf StdForallIndex || ident == identifierOf StdExistsIndex
  if isFiniteQuantifier && not (evalFiniteQuantifiers opts)
    then return $ VFreeVar ident []
    else do
      decl <- getDecl (Proxy @builtin) currentPass ident
      case bodyOf decl of
        Just expr | isInlinable (annotationsOf decl) -> return $ normalised expr
        _ -> return $ VFreeVar ident []

lookupIxValueInEnv ::
  (MonadNorm builtin m) =>
  Ix ->
  Env builtin ->
  m (Value builtin)
lookupIxValueInEnv l env =
  binderValue <$> lookupIxInBoundCtx currentPass l env

-----------------------------------------------------------------------------
-- Reevaluation

reeval ::
  (MonadNorm builtin m) =>
  NBEOptions ->
  Env builtin ->
  Value builtin ->
  m (Value builtin)
reeval opts env expr = do
  showNormEntry env expr
  result <- case expr of
    VUniverse {} -> return expr
    VLam binder lamEnv body -> do
      lamEnv' <- traverse (traverse (reeval opts env)) lamEnv
      return $ VLam binder lamEnv' body
    VPi {} -> return expr
    VMeta m spine -> VMeta m <$> reevalSpine opts env spine
    VFreeVar v spine -> do
      value <- lookupIdentValueInEnv opts v
      spine' <- reevalSpine opts env spine
      evalApp opts value spine'
    VBoundVar v spine -> do
      value <- lookupLvValueInEnv v env
      spine' <- reevalSpine opts env spine
      evalApp opts value spine'
    VBuiltin b spine ->
      evalBuiltin (evalApp opts) b =<< reevalSpine opts env spine
  showNormExit env result
  return result

reevalSpine ::
  (MonadNorm builtin m) =>
  NBEOptions ->
  Env builtin ->
  Spine builtin ->
  m (Spine builtin)
reevalSpine opts env = traverse (traverse (reeval opts env))

lookupLvValueInEnv ::
  (MonadNorm builtin m) =>
  Lv ->
  Env builtin ->
  m (Value builtin)
lookupLvValueInEnv l env =
  binderValue <$> lookupLvInBoundCtx currentPass l env

-----------------------------------------------------------------------------
-- Other

currentPass :: Doc ()
currentPass = "normalisation by evaluation"

showEntry :: (MonadNorm builtin m) => Env builtin -> Expr Ix builtin -> m ()
showEntry _env _expr = do
  -- logDebug MidDetail $ "nbe-entry" <+> prettyVerbose expr -- <+> "   { env=" <+> prettyVerbose env <+> "}"
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

findInstanceArg :: (MonadCompile m, Show op) => op -> [GenericArg a] -> m (a, [GenericArg a])
findInstanceArg op = \case
  (InstanceArg _ _ inst : xs) -> return (inst, xs)
  (_ : xs) -> findInstanceArg op xs
  [] -> compilerDeveloperError $ "Malformed type class operation:" <+> pretty (show op)
