{-# HLINT ignore "Use <|>" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Compile.Normalise.NBE
  ( AbstractFreeVarInterpretation,
    noAbstractFreeVarInterpretation,
    MonadNorm,
    normalise,
    normaliseInEnv,
    normaliseInEmptyEnv,
    normaliseApp,
    renormalise,
    eval,
    lookupIdentValueInEnv,
    findInstanceArg,
  )
where

import Data.Data (Proxy (..))
import Data.List.NonEmpty as NonEmpty (toList)
import Vehicle.Compile.Context.Free.Class (MonadFreeContext, getDecl)
import Vehicle.Compile.Context.Var (MonadVarContext, getBoundCtx)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Builtin (evalBuiltin)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Data.BuiltinInterface
import Vehicle.Data.NormalisedExpr

-- NOTE: there is no evaluate to NF in this file. To do it
-- efficiently you should just evaluate to WHNF and then recursively
-- evaluate as required.

normalise ::
  forall builtin m.
  (MonadVarContext builtin m) =>
  Expr Ix builtin ->
  m (WHNFValue builtin)
normalise e = do
  boundCtx <- getBoundCtx (Proxy @builtin)
  let env = boundContextToEnv boundCtx
  normaliseInEnv env e

normaliseInEnv ::
  forall builtin m.
  (MonadFreeContext builtin m) =>
  WHNFEnv builtin ->
  Expr Ix builtin ->
  m (WHNFValue builtin)
normaliseInEnv = eval (\ident args -> return $ VFreeVar ident args)

normaliseInEmptyEnv ::
  (MonadFreeContext builtin m) =>
  Expr Ix builtin ->
  m (WHNFValue builtin)
normaliseInEmptyEnv = normaliseInEnv mempty

normaliseApp ::
  (MonadNorm builtin m) =>
  WHNFValue builtin ->
  WHNFSpine builtin ->
  m (WHNFValue builtin)
normaliseApp = evalApp noAbstractFreeVarInterpretation

renormalise ::
  (MonadNorm builtin m) =>
  WHNFEnv builtin ->
  WHNFValue builtin ->
  m (WHNFValue builtin)
renormalise = reeval noAbstractFreeVarInterpretation

-----------------------------------------------------------------------------
-- Options during NBE

type AbstractFreeVarInterpretation builtin m =
  (MonadNorm builtin m) =>
  Identifier ->
  WHNFSpine builtin ->
  m (WHNFValue builtin)

noAbstractFreeVarInterpretation :: AbstractFreeVarInterpretation builtin m
noAbstractFreeVarInterpretation ident args = return $ VFreeVar ident args

-----------------------------------------------------------------------------
-- Evaluation

type MonadNorm builtin m =
  ( MonadFreeContext builtin m
  )

eval ::
  (MonadNorm builtin m) =>
  AbstractFreeVarInterpretation builtin m ->
  WHNFEnv builtin ->
  Expr Ix builtin ->
  m (WHNFValue builtin)
eval interp env expr = do
  showEntry env expr
  result <- case expr of
    Hole {} -> resolutionError currentPass "Hole"
    Meta _ m -> return $ VMeta m []
    Universe _ u -> return $ VUniverse u
    Builtin _ b -> return $ VBuiltin b []
    Lam _ binder body -> do
      binder' <- evalBinder interp env binder
      return $ VLam binder' (WHNFBody env body)
    Pi _ binder body -> do
      binder' <- evalBinder interp env binder
      let newEnv = extendEnvWithBound binder' env
      body' <- eval interp newEnv body
      return $ VPi binder' body'
    BoundVar _ i ->
      lookupIxValueInEnv i env
    FreeVar _ ident ->
      lookupIdentValueInEnv interp env ident
    Let _ bound binder body -> do
      binder' <- evalBinder interp env binder
      boundNormExpr <- eval interp env bound
      let newEnv = extendEnvWithDefined boundNormExpr binder' env
      eval interp newEnv body
    App _ fun args -> do
      fun' <- eval interp env fun
      args' <- traverse (traverse (eval interp env)) (NonEmpty.toList args)
      evalApp interp fun' args'

  showExit env result
  return result

evalBinder ::
  (MonadNorm builtin m) =>
  AbstractFreeVarInterpretation builtin m ->
  WHNFEnv builtin ->
  Binder Ix builtin ->
  m (WHNFBinder builtin)
evalBinder interp env = traverse (eval interp env)

evalApp ::
  (MonadNorm builtin m) =>
  AbstractFreeVarInterpretation builtin m ->
  WHNFValue builtin ->
  WHNFSpine builtin ->
  m (WHNFValue builtin)
evalApp _interp fun [] = return fun
evalApp interp fun args@(a : as) = do
  showApp fun args
  result <- case fun of
    VMeta v spine -> return $ VMeta v (spine <> args)
    VBoundVar v spine -> return $ VBoundVar v (spine <> args)
    VFreeVar v spine -> interp v (spine <> args)
    VLam binder (WHNFBody env body)
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
          let newEnv = extendEnvWithDefined (argExpr a) binder env
          body' <- eval interp newEnv body
          case as of
            [] -> return body'
            (b : bs) -> evalApp interp body' (b : bs)
    VBuiltin b spine
      | not (isTypeClassOp b) -> do
          evalBuiltin (evalApp interp) b (spine <> args)
      | otherwise -> do
          (inst, remainingArgs) <- findInstanceArg b args
          evalApp interp inst remainingArgs
    VUniverse {} -> unexpectedExprError currentPass ("VUniverse" <+> prettyVerbose args)
    VPi {} -> unexpectedExprError currentPass ("VPi" <+> prettyVerbose args)

  showAppExit result
  return result

lookupIdentValueInEnv ::
  forall builtin m.
  (MonadNorm builtin m) =>
  AbstractFreeVarInterpretation builtin m ->
  WHNFEnv builtin ->
  Identifier ->
  m (WHNFValue builtin)
lookupIdentValueInEnv interp env ident = do
  decl <- getDecl (Proxy @builtin) currentPass ident
  case bodyOf decl of
    Just expr -> eval interp env expr
    _ -> return $ VFreeVar ident []

lookupIxValueInEnv ::
  (MonadNorm builtin m) =>
  Ix ->
  WHNFEnv builtin ->
  m (WHNFValue builtin)
lookupIxValueInEnv ix env = do
  (_binder, value) <- lookupIxInBoundCtx currentPass ix env
  case value of
    Defined v -> return v
    Bound -> do
      -- We need to calculate the level in the final scope. Note this
      -- step could probably be optimised by storing the required level
      -- on the `Bound` contructor.
      let inScopeEnv = drop (unIx ix) env
      let lv = Lv $ length (filter isBoundEntry inScopeEnv) - 1
      return $ VBoundVar lv []

-----------------------------------------------------------------------------
-- Reevaluation

reeval ::
  (MonadNorm builtin m) =>
  AbstractFreeVarInterpretation builtin m ->
  WHNFEnv builtin ->
  WHNFValue builtin ->
  m (WHNFValue builtin)
reeval interp env expr = do
  showNormEntry env expr
  result <- case expr of
    VUniverse {} -> return expr
    VLam binder (WHNFBody lamEnv body) -> do
      lamEnv' <- traverse (reevalEnvEntry interp env) lamEnv
      return $ VLam binder (WHNFBody lamEnv' body)
    VPi {} -> return expr
    VMeta m spine -> VMeta m <$> reevalSpine interp env spine
    VFreeVar v spine -> do
      value <- lookupIdentValueInEnv interp env v
      spine' <- reevalSpine interp env spine
      evalApp interp value spine'
    VBoundVar v spine -> do
      value <- lookupLvValueInEnv v env
      spine' <- reevalSpine interp env spine
      evalApp interp value spine'
    VBuiltin b spine ->
      evalBuiltin (evalApp interp) b =<< reevalSpine interp env spine
  showNormExit env result
  return result

reevalEnvEntry ::
  (MonadNorm builtin m) =>
  AbstractFreeVarInterpretation builtin m ->
  WHNFEnv builtin ->
  EnvEntry 'WHNF builtin ->
  m (EnvEntry 'WHNF builtin)
reevalEnvEntry interp env (binder, value) =
  (binder,) <$> case value of
    Bound -> return Bound
    Defined v -> Defined <$> reeval interp env v

reevalSpine ::
  (MonadNorm builtin m) =>
  AbstractFreeVarInterpretation builtin m ->
  WHNFEnv builtin ->
  WHNFSpine builtin ->
  m (WHNFSpine builtin)
reevalSpine interp env = traverse (traverse (reeval interp env))

lookupLvValueInEnv ::
  (MonadNorm builtin m) =>
  Lv ->
  WHNFEnv builtin ->
  m (WHNFValue builtin)
lookupLvValueInEnv l env = do
  (_binder, value) <- lookupLvInBoundCtx currentPass l env
  case value of
    Defined v -> return v
    Bound -> return $ VBoundVar l []

-----------------------------------------------------------------------------
-- Other

currentPass :: Doc ()
currentPass = "normalisation by evaluation"

showEntry :: (MonadNorm builtin m) => WHNFEnv builtin -> Expr Ix builtin -> m ()
showEntry _env _expr = do
  -- logDebug MidDetail $ "nbe-entry" <+> prettyVerbose expr <+> "   { env=" <+> prettyVerbose env <+> "}"
  -- logDebug MidDetail $ "nbe-entry" <+> prettyFriendly (WithContext expr (fmap fst env)) <+> "   { env=" <+> hang 0 (prettyVerbose env) <+> "}"
  -- incrCallDepth
  return ()

showExit :: (MonadNorm builtin m) => WHNFEnv builtin -> WHNFValue builtin -> m ()
showExit _env _result = do
  -- decrCallDepth
  -- logDebug MidDetail $ "nbe-exit" <+> prettyVerbose result
  -- logDebug MidDetail $ "nbe-exit" <+> prettyFriendly (WithContext result (fmap fst env))
  return ()

showNormEntry :: (MonadNorm builtin m) => WHNFEnv builtin -> WHNFValue builtin -> m ()
showNormEntry _env _expr = do
  -- logDebug MidDetail $ "reeval-entry" <+> prettyVerbose expr -- <+> "   { env=" <+> prettyVerbose env <+> "}"
  -- logDebug MidDetail $ "reeval-entry" <+> prettyFriendly (WithContext expr (fmap fst env)) -- <+> "   { env=" <+> hang 0 (prettyVerbose env) <+> "}"
  -- incrCallDepth
  return ()

showNormExit :: (MonadNorm builtin m) => WHNFEnv builtin -> WHNFValue builtin -> m ()
showNormExit _env _result = do
  -- decrCallDepth
  -- logDebug MidDetail $ "reeval-exit" <+> prettyVerbose result
  -- logDebug MidDetail $ "reeval-exit" <+> prettyFriendly (WithContext result (fmap fst env))
  return ()

showApp :: (MonadNorm builtin m) => WHNFValue builtin -> WHNFSpine builtin -> m ()
showApp _fun _spine = do
  -- logDebug MaxDetail $ "nbe-app:" <+> prettyVerbose fun <+> "@" <+> prettyVerbose spine
  -- incrCallDepth
  return ()

showAppExit :: (MonadNorm builtin m) => WHNFValue builtin -> m ()
showAppExit _result = do
  -- decrCallDepth
  -- logDebug MaxDetail $ "nbe-app-exit:" <+> prettyVerbose result
  return ()

findInstanceArg :: (MonadCompile m, Show op) => op -> [GenericArg a] -> m (a, [GenericArg a])
findInstanceArg op = \case
  (InstanceArg _ _ inst : xs) -> return (inst, xs)
  (_ : xs) -> findInstanceArg op xs
  [] -> compilerDeveloperError $ "Malformed type class operation:" <+> pretty (show op)
