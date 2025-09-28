module Vehicle.Compile.Normalise.Quote where

import Data.Map.Ordered qualified as OMap
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Code.Expr (Expr (..), Substitution, normAppList, substituteDB)
import Vehicle.Data.Code.Value
import Vehicle.Data.Variable.Bound.Level (Lv, dbLevelToIndex)
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
quoteCtx p level env i = Right (quote p level (lookupIxInEnv env i))

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
    VPi binder closure -> do
      let quotedBinder = quote p level binder
      let quotedBody = quoteClosure p level (binder, closure)
      Pi p quotedBinder quotedBody
    VLam binder closure -> do
      let quotedBinder = quote p level binder
      let quotedBody = quoteClosure p level (binder, closure)
      Lam mempty quotedBinder quotedBody
    VRecord ident fields -> do
      let quotedFields = mapRecordFields (quote p level) $ OMap.assocs fields
      Record p ident quotedFields
    VRecordAcc r field -> do
      let quotedRecord = quote p level r
      RecordAcc p quotedRecord field

instance (Quote expr1 expr2) => Quote (GenericBinder expr1) (GenericBinder expr2) where
  quote p level = fmap (quote p level)

instance (Quote expr1 expr2) => Quote (GenericArg expr1) (GenericArg expr2) where
  quote p level = fmap (quote p level)

quoteApp :: (Quote a (Expr builtin2)) => Lv -> Provenance -> Expr builtin2 -> [GenericArg a] -> Expr builtin2
quoteApp l p fn spine = normAppList fn $ fmap (quote p l) spine
