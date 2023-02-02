module Vehicle.Compile.Normalise.Quote where

import Vehicle.Compile.Error (MonadCompile, runCompileMonadSilently)
import Vehicle.Compile.Prelude
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised

-- | Converts from a normalised representation to an unnormalised representation.
-- Do not call except for logging and debug purposes, very expensive with nested
-- lambdas.
unnormalise :: forall a b. Quote a b => DBLevel -> a -> b
unnormalise level e = runCompileMonadSilently "unquoting" (quote mempty level e)

-----------------------------------------------------------------------------
-- Quoting

-- i.e. converting back to unnormalised expressions

class Quote a b where
  quote :: MonadCompile m => Provenance -> DBLevel -> a -> m b

instance Quote NormExpr CheckedExpr where
  quote p level = \case
    VUniverse u -> return $ Universe p u
    VLiteral l -> return $ Literal p l
    VMeta m spine -> quoteSpine level p (Meta p m) spine
    VFreeVar v spine -> quoteSpine level p (Var p (Free v)) spine
    VBoundVar v spine -> quoteSpine level p (Var p (Bound (dbLevelToIndex level v))) spine
    VBuiltin b spine -> quoteSpine level p (Builtin p b) spine
    VLVec xs spine -> do
      xs' <- traverse (quote p level) xs
      quoteSpine level p (LVec p xs') spine
    VPi binder body ->
      Pi p <$> quote p level binder <*> quote p (level + 1) body
    VLam binder env body -> do
      quotedBinder <- quote p level binder
      quotedEnv <- traverse (quote p (level + 1)) env
      let liftedEnv = BoundVar p 0 : quotedEnv
      let quotedBody = substituteDB 0 (envSubst liftedEnv) body
      -- Here we deliberately avoid using the standard `quote . eval` approach below
      -- on the body of the lambda, in order to avoid the dependency cycles that
      -- prevent us from printing during NBE.
      --
      -- normBody <- runReaderT (eval (liftEnvOverBinder p env) body) mempty
      -- quotedBody <- quote (level + 1) normBody
      return $ Lam mempty quotedBinder quotedBody

instance Quote NormBinder CheckedBinder where
  quote p level = traverse (quote p level)

instance Quote NormArg CheckedArg where
  quote p level = traverse (quote p level)

quoteSpine :: MonadCompile m => DBLevel -> Provenance -> CheckedExpr -> Spine -> m CheckedExpr
quoteSpine l p fn spine = normAppList p fn <$> traverse (quote p l) spine

envSubst :: BoundCtx CheckedExpr -> Substitution DBExpr
envSubst env i = case lookupVar env i of
  Just v -> Right v
  Nothing ->
    developerError $
      "Mis-sized environment" <+> pretty (show env) <+> "when quoting variable" <+> pretty i
