module Vehicle.Compile.Normalise.Quote where

import Control.Monad.Except (runExceptT)
import Vehicle.Compile.Error (MonadCompile)
import Vehicle.Compile.Prelude
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised

-- | Converts from a normalised representation to an unnormalised representation.
-- Do not call except for logging and debug purposes, very expensive with nested
-- lambdas.
unnormalise :: forall a b. Quote a b => DBLevel -> a -> b
unnormalise level e = do
  let r = runSilentLogger $ runExceptT (quote level e)
  case r of
    Left err -> developerError $ "Error thrown while unquoting" <+> pretty (show err)
    Right v -> v

-----------------------------------------------------------------------------
-- Quoting

-- i.e. converting back to unnormalised expressions

class Quote a b where
  quote :: MonadCompile m => DBLevel -> a -> m b

instance Quote NormExpr CheckedExpr where
  quote level = \case
    VUniverse p u -> return $ Universe p u
    VLiteral p l -> return $ Literal p l
    VMeta p m spine -> quoteSpine level p (Meta p m) spine
    VFreeVar p v spine -> quoteSpine level p (Var p (Free v)) spine
    VBoundVar p v spine -> quoteSpine level p (Var p (Bound (dbLevelToIndex level v))) spine
    VBuiltin p b spine -> quoteSpine level p (Builtin p b) spine
    VLVec p xs spine -> do
      xs' <- traverse (quote level) xs
      quoteSpine level p (LVec p xs') spine
    VPi p binder body ->
      Pi p <$> quote level binder <*> quote (level + 1) body
    VLam p binder env body -> do
      quotedBinder <- quote level binder
      quotedEnv <- traverse (quote (level + 1)) (liftEnvOverBinder p env)
      let quotedBody = substituteDB 0 (envSubst quotedEnv) body
      -- Here we deliberately avoid using the standard `quote . eval` approach below
      -- on the body of the lambda, in order to avoid the dependency cycles that
      -- prevent us from printing during NBE.
      --
      -- normBody <- runReaderT (eval (liftEnvOverBinder p env) body) mempty
      -- quotedBody <- quote (level + 1) normBody
      return $ Lam p quotedBinder quotedBody

instance Quote NormBinder CheckedBinder where
  quote level = traverse (quote level)

instance Quote NormArg CheckedArg where
  quote level = traverse (quote level)

quoteSpine :: MonadCompile m => DBLevel -> Provenance -> CheckedExpr -> Spine -> m CheckedExpr
quoteSpine l p fn spine = normAppList p fn <$> traverse (quote l) spine

envSubst :: BoundCtx CheckedExpr -> Substitution DBExpr
envSubst env i = case lookupVar env i of
  Just v -> Right v
  Nothing ->
    developerError $
      "Mis-sized environment" <+> pretty (show env) <+> "when quoting variable" <+> pretty i
