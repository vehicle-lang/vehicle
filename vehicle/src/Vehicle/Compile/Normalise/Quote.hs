module Vehicle.Compile.Normalise.Quote where

import Vehicle.Compile.Prelude
import Vehicle.Data.DeBruijn
import Vehicle.Data.NormalisedExpr

-- | Converts from a normalised representation to an unnormalised representation.
-- Do not call except for logging and debug purposes, very expensive with nested
-- lambdas.
unnormalise :: forall a b. (Quote a b) => Lv -> a -> b
unnormalise = quote mempty

-----------------------------------------------------------------------------
-- Quoting

-- i.e. converting back to unnormalised expressions

class Quote a b where
  quote :: Provenance -> Lv -> a -> b

instance (Show builtin) => Quote (Value strategy builtin) (Expr Ix builtin) where
  quote p level = \case
    VUniverse u -> Universe p u
    VMeta m spine -> quoteApp level p (Meta p m) spine
    VFreeVar v spine -> quoteApp level p (FreeVar p v) spine
    VBoundVar v spine -> do
      let var = BoundVar p (dbLevelToIndex level v)
      quoteApp level p var spine
    VBuiltin b spine -> quoteApp level p (Builtin p b) spine
    VPi binder body -> do
      let quotedBinder = quote p level binder
      let quotedBody = quote p (level + 1) body
      Pi p quotedBinder quotedBody
    VLam binder body -> do
      let quotedBinder = quote p level binder
      let quotedBody = quote p level (binder, body)
      Lam mempty quotedBinder quotedBody

instance (Show builtin) => Quote (VBinder strategy builtin, Body strategy builtin) (Expr Ix builtin) where
  quote p level (binder, body) = case body of
    NFBody value -> quote p (level + 1) value
    WHNFBody env value -> do
      -- Here we deliberately avoid using the standard `quote . eval` approach below
      -- on the body of the lambda, in order to avoid the dependency cycles that
      -- prevent us from printing during NBE.
      --
      -- normBody <- runReaderT (eval (liftEnvOverBinder p env) body) mempty
      -- quotedBody <- quote (level + 1) normBody
      let newEnv = extendEnvWithBound level binder env
      substituteDB 0 (envSubst p (level + 1) newEnv) value

instance (Show builtin) => Quote (VBinder strategy builtin) (Binder Ix builtin) where
  quote p level = fmap (quote p level)

instance (Show builtin) => Quote (VArg strategy builtin) (Arg Ix builtin) where
  quote p level = fmap (quote p level)

quoteApp :: (Show builtin) => Lv -> Provenance -> Expr Ix builtin -> Spine strategy builtin -> Expr Ix builtin
quoteApp l p fn spine = normAppList fn $ fmap (quote p l) spine

envSubst :: (Show builtin) => Provenance -> Lv -> WHNFBoundEnv builtin -> Substitution (Expr Ix builtin)
envSubst p level env i = case lookupIx env i of
  Nothing ->
    developerError $
      "Mis-sized environment" <+> pretty (length env) <+> "when quoting variable" <+> pretty i
  Just (_binder, value) -> case value of
    Bound {} -> Left i
    Defined v -> Right (quote p level v)
