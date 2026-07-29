module Vehicle.Compile.Normalise.Quote where

import Data.Map.Ordered qualified as OMap
import GHC.Stack (HasCallStack)
import Vehicle.Compile.Normalise.Core (MetaLike (..))
import Vehicle.Data.AST.Expr.Scoped (Expr (..), Substitution, normAppList, substituteDB)
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Code.ForcedValue (GenericForcedValue, GenericThunk)
import Vehicle.Data.Code.ForcedValue qualified as F
import Vehicle.Data.Variable.Bound.Level (Lv, dbLevelToIndex)
import Vehicle.Prelude

-- | Converts from a normalised representation to an unnormalised representation.
-- Do not call except for logging and debug purposes, very expensive with nested
-- lambdas.
unnormalise :: forall a b. (HasCallStack, Quote a b) => Lv -> a -> b
unnormalise = quote mempty

-----------------------------------------------------------------------------
-- Quoting closures

class Quote a b where
  quote :: (HasCallStack) => Provenance -> Lv -> a -> b

instance (Quote expr1 expr2) => Quote (GenericBinder expr1) (GenericBinder expr2) where
  quote p level = fmap (quote p level)

instance (Quote expr1 expr2) => Quote (GenericArg expr1) (GenericArg expr2) where
  quote p level = fmap (quote p level)

quoteApp :: (HasCallStack, Quote a (Expr builtin2)) => Lv -> Provenance -> Expr builtin2 -> [GenericArg a] -> Expr builtin2
quoteApp l p fn spine = normAppList fn $ fmap (quote p l) spine

-----------------------------------------------------------------------------
-- Quoting forced values

instance (ConvertableBuiltin builtin1 builtin2, MetaLike meta) => Quote (GenericThunk meta builtin1) (Expr builtin2) where
  quote p level = \case
    F.Forced value -> quote p level value
    F.Unforced env expr -> do
      let subst = quoteForcedCtx p level env
      substituteDB 0 subst (convertExprBuiltins expr)

instance (ConvertableBuiltin builtin1 builtin2, MetaLike meta) => Quote (GenericForcedValue meta builtin1) (Expr builtin2) where
  quote p level = \case
    F.VUniverse u -> Universe p u
    F.VMeta m spine -> quoteApp level p (Meta p (toMetaID m)) spine
    F.VFreeVar v spine -> quoteApp level p (FreeVar p v) spine
    F.VBoundVar v spine -> do
      let var = BoundVar p (dbLevelToIndex level v)
      quoteApp level p var spine
    F.VBuiltin b spine -> do
      let fn = convertBuiltin p b
      quoteApp level p fn spine
    F.VPi binder closure -> do
      let quotedBinder = quote p level binder
      let quotedBody = quoteClosure p level (binder, closure)
      Pi p quotedBinder quotedBody
    F.VLam binder closure -> do
      let quotedBinder = quote p level binder
      let quotedBody = quoteClosure p level (binder, closure)
      Lam mempty quotedBinder quotedBody
    F.VRecord recordType fields -> do
      let quotedRecordType = quote p level recordType
      let quotedFields = mapRecordFields (quote p level) $ OMap.assocs fields
      Record p quotedRecordType quotedFields
    F.VRecordAcc recordType record field spine -> do
      let quotedRecordType = quote p level recordType
      let quotedRecord = quote p level record
      let quotedProj = RecordProj p quotedRecordType quotedRecord field
      quoteApp level p quotedProj spine

quoteForcedCtx ::
  (ConvertableBuiltin builtin1 builtin2, MetaLike meta) =>
  Provenance ->
  Lv ->
  F.GenericBoundEnv meta builtin1 ->
  Substitution (Expr builtin2)
quoteForcedCtx p level env i = Right (quote p level (F.lookupIxInEnv env i))

quoteClosure ::
  (ConvertableBuiltin builtin1 builtin2, MetaLike meta) =>
  Provenance ->
  Lv ->
  (GenericBinder expr, F.GenericClosure meta builtin1) ->
  Expr builtin2
quoteClosure p lv (binder, F.Closure env body) = do
  -- Here we deliberately avoid using the standard `quote . eval` approach below
  -- on the body of the lambda, in order to avoid the dependency cycles that
  -- prevent us from printing during NBE.
  --
  -- normBody <- runReaderT (eval (liftEnvOverBinder p env) body) mempty
  -- quotedBody <- quote (level + 1) normBody
  let newEnv = F.extendEnvWithBound lv binder env
  let subst = quoteForcedCtx p (lv + 1) newEnv
  substituteDB 0 subst (convertExprBuiltins body)
