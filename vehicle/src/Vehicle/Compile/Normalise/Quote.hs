module Vehicle.Compile.Normalise.Quote where

import Control.Monad.Reader (MonadReader(..), runReader)

import Vehicle.Compile.Prelude
import Vehicle.Expr.Normalised
import Vehicle.Compile.Error (MonadCompile)
import Control.Monad.Except (runExceptT)
import Vehicle.Expr.DeBruijn

-----------------------------------------------------------------------------
-- DeBruijn adjustment

-- | Alters the DB indices in the expression. Can't use normal DB substitution
-- as the values being substituted in NormExpr change when you go under a lambda
-- binder. Crucially it also doesn't trigger any renormalisation, unlike a
-- standard subsitution would.
class DBAdjustment a where
  adjustIndices :: MonadReader (BindingDepth, DBIndex -> Maybe DBIndex) m => a -> m a

instance DBAdjustment NormExpr where
  adjustIndices expr = case expr of
    VUniverse{} -> return expr
    VLiteral{}  -> return expr

    VMeta    p m  spine -> VMeta    p m <$> traverse adjustIndices spine
    VBuiltin p b  spine -> VBuiltin p b <$> traverse adjustIndices spine
    VLVec    p xs spine -> VLVec    p   <$> traverse adjustIndices xs <*> traverse adjustIndices spine

    VVar p v@(Free _) args -> VVar p v <$> traverse adjustIndices args
    VPi  p binder body -> VPi p <$> traverse adjustIndices binder <*> underDBBinder (adjustIndices body)

    VVar p (Bound i) args -> do
      args' <- traverse adjustIndices args
      (bindingDepth, sub) <- ask
      -- Calculate the new index
      let i' = if i < bindingDepth
          then i
          else case sub (i - bindingDepth) of
            Nothing -> i
            Just v  -> v + bindingDepth
      return $ VVar p (Bound i') args'

    VLam p binder env body -> do
      binder' <- traverse adjustIndices binder
      env' <- adjustIndices env
      body' <- underDBBinder $ do
        (bindingDepth, sub) <- ask
        return $ substAll bindingDepth sub body
      return $ VLam p binder' env' body'

instance DBAdjustment a => DBAdjustment (GenericArg a) where
  adjustIndices = traverse adjustIndices

instance DBAdjustment a => DBAdjustment (GenericBinder binder a) where
  adjustIndices = traverse adjustIndices

instance DBAdjustment Env where
  adjustIndices boundCtx = do
    newBoundCtx <- traverse adjustIndices boundCtx
    return newBoundCtx

adjustDBIndices :: DBAdjustment a => BindingDepth -> (DBIndex -> Maybe DBIndex) -> a -> a
adjustDBIndices d sub e = runReader (adjustIndices e) (d, sub)

liftFreeDBIndicesNorm :: BindingDepth -> NormExpr -> NormExpr
liftFreeDBIndicesNorm amount = adjustDBIndices 0 (\i -> Just (i + amount))

liftEnvOverBinder :: Provenance -> Env -> Env
liftEnvOverBinder p = extendEnv (VVar p (Bound 0) [])

extendEnv :: NormExpr -> Env -> Env
extendEnv value boundCtx = value : fmap (liftFreeDBIndicesNorm 1) boundCtx

-----------------------------------------------------------------------------
-- Quoting

-- i.e. converting back to unnormalised expressions

class Quote a b where
  quote :: MonadCompile m => a -> m b

  -- | Converts from a normalised representation to an unnormalised representation.
  -- Do not call except for logging and debug purposes, very expensive with nested
  -- lambdas.
  unnormalise :: a -> b
  unnormalise e = do
    let r = discardLogger $ runExceptT (quote e)
    case r of
      Left err -> developerError $ "Error thrown while unquoting" <+> pretty (show err)
      Right v  -> v

instance Quote NormExpr CheckedExpr where
  quote = \case
    VUniverse p u -> return $ Universe p u
    VLiteral  p l -> return $ Literal p l

    VMeta     p m spine -> normAppList p (Meta p m)    <$> traverse quote spine
    VVar      p v spine -> normAppList p (Var p v)     <$> traverse quote spine
    VBuiltin  p b spine -> normAppList p (Builtin p b) <$> traverse quote spine

    VLVec p xs spine -> normAppList p <$> (LVec p <$> traverse quote xs) <*> traverse quote spine
    VPi   p binder body -> Pi p <$> quote binder <*> quote body

    VLam  p binder env body -> do
      -- First quote the binder
      quotedBinder <- quote binder
      -- Then quote the lifted environment
      quotedBoundCtx <- traverse quote (liftEnvOverBinder p env)
      -- Then substitute the environment through the body
      let quotedBody = substitute 0 (envSubst quotedBoundCtx) body

      return $ Lam p quotedBinder quotedBody

envSubst :: [CheckedExpr] -> Substitution DBExpr
envSubst env i = case env !!? i of
  Just v  -> Right v
  Nothing -> developerError $
    "Mis-sized environment" <+> pretty (show env) <+> "when quoting variable" <+> pretty i

instance Quote NormBinder CheckedBinder where
  quote = traverse quote

instance Quote NormArg CheckedArg where
  quote = traverse quote
