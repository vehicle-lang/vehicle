{-# HLINT ignore "Use <|>" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Compile.Normalise.NBE
  ( MonadNorm,
    normalise,
    normaliseInEnv,
    normaliseInEmptyEnv,
    normaliseApp,
    eval,
    findInstanceArg,
    evalBuiltin,
  )
where

import Data.Data (Proxy (..))
import Data.List.NonEmpty as NonEmpty (toList)
import Vehicle.Compile.Context.Bound.Class (MonadBoundContext (..))
import Vehicle.Compile.Context.Free.Class (MonadFreeContext, getDecl)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Builtin qualified as Builtin (evalBuiltin)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Data.BuiltinInterface
import Vehicle.Data.NormalisedExpr

-- NOTE: there is no evaluatation to NF in this file. To do it
-- efficiently you should just evaluate to WHNF and then recursively
-- evaluate as required.

normalise ::
  forall builtin m.
  (MonadBoundContext builtin m, MonadFreeContext builtin m) =>
  Expr Ix builtin ->
  m (WHNFValue builtin)
normalise e = do
  boundCtx <- getBoundCtx (Proxy @builtin)
  let env = boundContextToEnv boundCtx
  normaliseInEnv env e

normaliseInEnv ::
  (MonadFreeContext builtin m) =>
  WHNFBoundEnv builtin ->
  Expr Ix builtin ->
  m (WHNFValue builtin)
normaliseInEnv = eval

normaliseInEmptyEnv ::
  (MonadFreeContext builtin m) =>
  Expr Ix builtin ->
  m (WHNFValue builtin)
normaliseInEmptyEnv = normaliseInEnv mempty

normaliseApp ::
  (MonadFreeContext builtin m) =>
  WHNFValue builtin ->
  WHNFSpine builtin ->
  m (WHNFValue builtin)
normaliseApp = evalApp

-----------------------------------------------------------------------------
-- Evaluation

type MonadNorm builtin m =
  ( PrintableBuiltin builtin,
    HasStandardData builtin,
    MonadFreeContext builtin m
  )

eval ::
  (MonadNorm builtin m) =>
  WHNFBoundEnv builtin ->
  Expr Ix builtin ->
  m (WHNFValue builtin)
eval env expr = do
  showEntry env expr
  result <- case expr of
    Hole {} -> resolutionError currentPass "Hole"
    Meta _ m -> return $ VMeta m []
    Universe _ u -> return $ VUniverse u
    Builtin _ b -> return $ VBuiltin b []
    BoundVar _ v -> lookupIxValueInEnv env v
    FreeVar _ v -> lookupIdentValueInEnv v
    Lam _ binder body -> do
      binder' <- evalBinder env binder
      return $ VLam binder' (WHNFBody env body)
    Pi _ binder body -> do
      binder' <- evalBinder env binder
      let newEnv = extendEnvWithBound (Lv $ length env) binder' env
      body' <- eval newEnv body
      return $ VPi binder' body'
    Let _ bound binder body -> do
      binder' <- evalBinder env binder
      boundNormExpr <- eval env bound
      let newEnv = extendEnvWithDefined boundNormExpr binder' env
      eval newEnv body
    App fun args -> do
      fun' <- eval env fun
      args' <- traverse (traverse (eval env)) (NonEmpty.toList args)
      evalApp fun' args'

  showExit env result
  return result

evalBinder ::
  (MonadNorm builtin m) =>
  WHNFBoundEnv builtin ->
  Binder Ix builtin ->
  m (WHNFBinder builtin)
evalBinder env = traverse (eval env)

evalApp ::
  (MonadNorm builtin m) =>
  WHNFValue builtin ->
  WHNFSpine builtin ->
  m (WHNFValue builtin)
evalApp fun [] = return fun
evalApp fun args@(a : as) = do
  showApp fun args
  result <- case fun of
    VMeta v spine -> return $ VMeta v (spine <> args)
    VBoundVar v spine -> return $ VBoundVar v (spine <> args)
    VFreeVar v spine -> return $ VFreeVar v (spine <> args)
    VLam binder (WHNFBody env body)
      | not (visibilityMatches binder a) ->
          visibilityError currentPass (prettyVerbose fun) (prettyVerbose args)
      | otherwise -> do
          let newEnv = extendEnvWithDefined (argExpr a) binder env
          body' <- eval newEnv body
          case as of
            [] -> return body'
            (b : bs) -> evalApp body' (b : bs)
    VBuiltin b spine
      | not (isTypeClassOp b) -> do
          evalBuiltin b (spine <> args)
      | otherwise -> do
          (inst, remainingArgs) <- findInstanceArg b args
          evalApp inst remainingArgs
    VUniverse {} -> unexpectedExprError currentPass ("VUniverse" <+> prettyVerbose args)
    VPi {} -> unexpectedExprError currentPass ("VPi" <+> prettyVerbose args)

  showAppExit result
  return result

evalBuiltin ::
  (MonadNorm builtin m) =>
  builtin ->
  WHNFSpine builtin ->
  m (WHNFValue builtin)
evalBuiltin = Builtin.evalBuiltin evalApp

lookupIdentValueInEnv ::
  forall builtin m.
  (MonadNorm builtin m) =>
  Identifier ->
  m (WHNFValue builtin)
lookupIdentValueInEnv ident = do
  decl <- getDecl (Proxy @builtin) currentPass ident
  return $ case bodyOf decl of
    Just value -> value
    _ -> VFreeVar ident []

lookupIxValueInEnv ::
  (MonadNorm builtin m) =>
  WHNFBoundEnv builtin ->
  Ix ->
  m (WHNFValue builtin)
lookupIxValueInEnv boundEnv ix = do
  (_binder, value) <- lookupIxInBoundCtx currentPass ix boundEnv
  case value of
    Defined v -> return v
    Bound -> do
      -- We need to calculate the level in the final scope. Note this
      -- step could probably be optimised by storing the required level
      -- on the `Bound` contructor.
      let inScopeEnv = drop (unIx ix) boundEnv
      let lv = Lv $ length (filter isBoundEntry inScopeEnv) - 1
      return $ VBoundVar lv []

-----------------------------------------------------------------------------
-- Other

currentPass :: Doc ()
currentPass = "normalisation by evaluation"

showEntry :: (MonadNorm builtin m) => WHNFBoundEnv builtin -> Expr Ix builtin -> m ()
showEntry _env _expr = do
  -- logDebug MidDetail $ "nbe-entry" <+> prettyVerbose expr <+> "   { env=" <+> prettyVerbose env <+> "}"
  -- logDebug MidDetail $ "nbe-entry" <+> prettyFriendly (WithContext expr (fmap fst env)) <+> "   { env=" <+> hang 0 (prettyVerbose env) <+> "}"
  -- incrCallDepth
  return ()

showExit :: (MonadNorm builtin m) => WHNFBoundEnv builtin -> WHNFValue builtin -> m ()
showExit _env _result = do
  -- decrCallDepth
  -- logDebug MidDetail $ "nbe-exit" <+> prettyVerbose result
  -- logDebug MidDetail $ "nbe-exit" <+> prettyFriendly (WithContext result (fmap fst env))
  return ()

showApp :: (MonadNorm builtin m) => WHNFValue builtin -> WHNFSpine builtin -> m ()
showApp _fun _spine = do
  -- logDebug MaxDetail $ "nbe-app:" <+> prettyVerbose fun <+> "@" <+> prettyVerbose spine
  -- incrCallDepth
  return ()

showAppExit :: (MonadNorm builtin m) => Value nf builtin -> m ()
showAppExit _result = do
  -- decrCallDepth
  -- logDebug MaxDetail $ "nbe-app-exit:" <+> prettyVerbose result
  return ()

findInstanceArg :: (MonadCompile m, Show op) => op -> [GenericArg a] -> m (a, [GenericArg a])
findInstanceArg op = \case
  (InstanceArg _ _ inst : xs) -> return (inst, xs)
  (_ : xs) -> findInstanceArg op xs
  [] -> compilerDeveloperError $ "Malformed type class operation:" <+> pretty (show op)
