module Vehicle.Compile.Normalise.Quote where

import Vehicle.Compile.Context.Bound.Core
import Vehicle.Compile.Print.Builtin
import Vehicle.Data.Code.Expr
import Vehicle.Data.Code.Value
import Vehicle.Data.DeBruijn (Lv, Substitution, dbLevelToIndex)
import Vehicle.Prelude

-- | Converts from a normalised representation to an unnormalised representation.
-- Do not call except for logging and debug purposes, very expensive with nested
-- lambdas.
unnormalise :: forall a b. (Quote a b) => Lv -> a -> b
unnormalise = quote mempty

-----------------------------------------------------------------------------
-- Quoting closures

quoteClosure :: (ConvertableBuiltin builtin1 builtin2) => Provenance -> Lv -> (GenericBinder expr, Closure builtin1) -> Expr builtin2
quoteClosure p lv (binder, Closure env body) = do
  -- Here we deliberately avoid using the standard `quote . eval` approach below
  -- on the body of the lambda, in order to avoid the dependency cycles that
  -- prevent us from printing during NBE.
  --
  -- normBody <- runReaderT (eval (liftEnvOverBinder p env) body) mempty
  -- quotedBody <- quote (level + 1) normBody
  let newEnv = extendEnvWithBound lv binder env
  let subst = quoteCtx p (lv + 1) newEnv
  substituteDB 0 subst (convertExprBuiltins body)

quoteCtx :: (ConvertableBuiltin builtin1 builtin2) => Provenance -> Lv -> BoundEnv builtin1 -> Substitution (Expr builtin2)
quoteCtx p level env i = case lookupIx env i of
  Nothing -> developerError $ "Mis-sized environment" <+> pretty (length env) <+> "when quoting variable" <+> pretty i
  Just (_binder, value) -> Right (quote p level value)

-----------------------------------------------------------------------------
-- Quoting expressions

class Quote a b where
  quote :: Provenance -> Lv -> a -> b

instance (ConvertableBuiltin builtin1 builtin2) => Quote (Value builtin1) (Expr builtin2) where
  quote p level = \case
    VUniverse u -> Universe p u
    VMeta m spine -> quoteApp level p (Meta p m) spine
    VFreeVar v spine -> quoteApp level p (FreeVar p v) spine
    VBoundVar v spine -> do
      let var = BoundVar p (dbLevelToIndex level v)
      quoteApp level p var spine
    VBuiltin b spine -> do
      let fn = convertBuiltin p b
      quoteApp level p fn spine
    VPi binder body -> do
      let quotedBinder = quote p level binder
      let quotedBody = quote p (level + 1) body
      Pi p quotedBinder quotedBody
    VLam binder closure -> do
      let quotedBinder = quote p level binder
      let quotedBody = quoteClosure p level (binder, closure)
      Lam mempty quotedBinder quotedBody

instance (Quote expr1 expr2) => Quote (GenericBinder expr1) (GenericBinder expr2) where
  quote p level = fmap (quote p level)

instance (Quote expr1 expr2) => Quote (GenericArg expr1) (GenericArg expr2) where
  quote p level = fmap (quote p level)

quoteApp :: (Quote a (Expr builtin2)) => Lv -> Provenance -> Expr builtin2 -> [GenericArg a] -> Expr builtin2
quoteApp l p fn spine = normAppList fn $ fmap (quote p l) spine
