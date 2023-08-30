module Vehicle.Compile.Normalise.Quote where

import Vehicle.Compile.Error (MonadCompile, runCompileMonadSilently)
import Vehicle.Compile.Prelude
import Vehicle.Data.DeBruijn
import Vehicle.Data.NormalisedExpr

-- | Converts from a normalised representation to an unnormalised representation.
-- Do not call except for logging and debug purposes, very expensive with nested
-- lambdas.
unnormalise :: forall a b. (Quote a b) => Lv -> a -> b
unnormalise level e = runCompileMonadSilently "unquoting" (quote mempty level e)

-----------------------------------------------------------------------------
-- Quoting

-- i.e. converting back to unnormalised expressions

class Quote a b where
  quote :: (MonadCompile m) => Provenance -> Lv -> a -> m b

instance Quote (Value builtin) (Expr Ix builtin) where
  quote p level = \case
    VUniverse u -> return $ Universe p u
    VMeta m spine -> quoteApp level p (Meta p m) spine
    VFreeVar v spine -> quoteApp level p (FreeVar p v) spine
    VBoundVar v spine -> quoteApp level p (BoundVar p (dbLevelToIndex level v)) spine
    VBuiltin b spine -> quoteApp level p (Builtin p b) spine
    VPi binder body ->
      Pi p <$> quote p level binder <*> quote p (level + 1) body
    VLam binder env body -> do
      quotedBinder <- quote p level binder
      let lamBinderEnvEntry = fmap (const (VBoundVar level [])) binder
      quotedEnv <- traverse (\b -> quote p (level + 1) b) (lamBinderEnvEntry : env)
      let quotedBody = substituteDB 0 (envSubst quotedEnv) body
      -- Here we deliberately avoid using the standard `quote . eval` approach below
      -- on the body of the lambda, in order to avoid the dependency cycles that
      -- prevent us from printing during NBE.
      --
      -- normBody <- runReaderT (eval (liftEnvOverBinder p env) body) mempty
      -- quotedBody <- quote (level + 1) normBody
      return $ Lam mempty quotedBinder quotedBody

instance Quote (VBinder builtin) (Binder Ix builtin) where
  quote p level = traverse (quote p level)

instance Quote (VArg builtin) (Arg Ix builtin) where
  quote p level = traverse (quote p level)

quoteApp :: (MonadCompile m) => Lv -> Provenance -> Expr Ix builtin -> Spine builtin -> m (Expr Ix builtin)
quoteApp l p fn spine = normAppList p fn <$> traverse (quote p l) spine

envSubst :: BoundCtx builtin -> Substitution (Expr Ix builtin)
envSubst env i = case lookupIx env i of
  Just v -> Right (binderValue v)
  Nothing ->
    developerError $
      "Mis-sized environment" <+> pretty (length env) <+> "when quoting variable" <+> pretty i
