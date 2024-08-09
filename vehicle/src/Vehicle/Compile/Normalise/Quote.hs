module Vehicle.Compile.Normalise.Quote where

import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Interface (ConvertableBuiltin (..))
import Vehicle.Data.Builtin.Loss.Core (MixedClosure (..))
import Vehicle.Data.Builtin.Standard.Core (Builtin)
import Vehicle.Data.DeBruijn
import Vehicle.Data.Expr.Normalised

-- | Converts from a normalised representation to an unnormalised representation.
-- Do not call except for logging and debug purposes, very expensive with nested
-- lambdas.
unnormalise :: forall a b. (Quote a b) => Lv -> a -> b
unnormalise = quote mempty

-----------------------------------------------------------------------------
-- Quoting closures

class QuoteClosure closure builtin where
  quoteClosure :: Provenance -> Lv -> (GenericBinder expr, closure) -> Expr Ix builtin

instance (ConvertableBuiltin builtin1 builtin2) => QuoteClosure (WHNFClosure builtin1) builtin2 where
  quoteClosure p lv (binder, WHNFClosure env body) = do
    -- Here we deliberately avoid using the standard `quote . eval` approach below
    -- on the body of the lambda, in order to avoid the dependency cycles that
    -- prevent us from printing during NBE.
    --
    -- normBody <- runReaderT (eval (liftEnvOverBinder p env) body) mempty
    -- quotedBody <- quote (level + 1) normBody
    let newEnv = extendEnvWithBound lv binder env
    let subst = quoteCtx p (lv + 1) newEnv
    substituteDB 0 subst (convertExprBuiltins body)

instance (ConvertableBuiltin builtin1 builtin2) => QuoteClosure (NFClosure builtin1) builtin2 where
  quoteClosure p level (_binder, body) = case body of
    NFClosure value -> quote p (level + 1) value

instance QuoteClosure MixedClosure Builtin where
  quoteClosure p lv (binder, closure) = case closure of
    StandardClosure standardClosure -> quoteClosure p lv (binder, standardClosure)
    LossClosure env body -> do
      let newEnv = extendEnvWithBound lv binder env
      let subst = quoteCtx p (lv + 1) newEnv
      substituteDB 0 subst (convertExprBuiltins body)

quoteCtx :: (ConvertableBuiltin builtin1 builtin2, QuoteClosure closure builtin2) => Provenance -> Lv -> BoundEnv closure builtin1 -> Substitution (Expr Ix builtin2)
quoteCtx p level env i = case lookupIx env i of
  Nothing -> developerError $ "Mis-sized environment" <+> pretty (length env) <+> "when quoting variable" <+> pretty i
  Just (_binder, value) -> Right (quote p level value)

-----------------------------------------------------------------------------
-- Quoting expressions

class Quote a b where
  quote :: Provenance -> Lv -> a -> b

instance (ConvertableBuiltin builtin1 builtin2, QuoteClosure closure builtin2) => Quote (Value closure builtin1) (Expr Ix builtin2) where
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

quoteApp :: (Quote a (Expr Ix builtin2)) => Lv -> Provenance -> Expr Ix builtin2 -> [GenericArg a] -> Expr Ix builtin2
quoteApp l p fn spine = normAppList fn $ fmap (quote p l) spine
