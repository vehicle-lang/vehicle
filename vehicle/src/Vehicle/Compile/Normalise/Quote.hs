module Vehicle.Compile.Normalise.Quote
  ( unnormalise,
  )
where

import Data.Map.Ordered qualified as OMap
import GHC.Stack (HasCallStack)
import Vehicle.Compile.Normalise.Core (MetaLike (..))
import Vehicle.Data.AST.Expr.Scoped (Arg, Binder, Expr (..), Substitution, normAppList, substituteDB)
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Variable.Bound.Level (Lv, dbLevelToIndex)
import Vehicle.Prelude

-- | Converts from a normalised representation to an unnormalised representation.
-- Do not call except for logging and debug purposes, very expensive with nested
-- lambdas.
unnormalise :: (HasCallStack, Quote a b builtin) => Lv -> a -> b
unnormalise = quote $ \level var -> BoundVar mempty (dbLevelToIndex level var)

-----------------------------------------------------------------------------
-- Quoting

type BoundVarHandler builtin = Lv -> Lv -> Expr builtin

class Quote a b builtin | a -> builtin where
  quote :: (HasCallStack) => BoundVarHandler builtin -> Lv -> a -> b

instance (MetaLike meta) => Quote (GenericUnforcedBinder meta builtin) (Binder builtin) builtin where
  quote handler level = fmap (quote handler level)

instance (MetaLike meta) => Quote (GenericUnforcedArg meta builtin) (Arg builtin) builtin where
  quote handler level = fmap (quote handler level)

instance (MetaLike meta) => Quote (GenericThunk meta builtin) (Expr builtin) builtin where
  quote handler level = \case
    Forced value -> quote handler level value
    Unforced env expr -> do
      let subst = quoteForcedCtx handler level env
      substituteDB 0 subst expr

instance (MetaLike meta) => Quote (GenericForcedValue meta builtin) (Expr builtin) builtin where
  quote handler level = \case
    VUniverse u ->
      Universe p u
    VMeta m spine ->
      quoteApp handler level (Meta p (toMetaID m)) spine
    VFreeVar v spine ->
      quoteApp handler level (FreeVar p v) spine
    VBoundVar v spine -> do
      let var = BoundVar p (dbLevelToIndex level v)
      quoteApp handler level var spine
    VBuiltin b spine -> do
      let fn = convertBuiltin p b
      quoteApp handler level fn spine
    VPi binder closure -> do
      let quotedBinder = quote handler level binder
      let quotedBody = quoteClosure handler level (binder, closure)
      Pi p quotedBinder quotedBody
    VLam binder closure -> do
      let quotedBinder = quote handler level binder
      let quotedBody = quoteClosure handler level (binder, closure)
      Lam mempty quotedBinder quotedBody
    VRecord recordType fields -> do
      let quotedRecordType = quote handler level recordType
      let quotedFields = mapRecordFields (quote handler level) $ OMap.assocs fields
      Record p quotedRecordType quotedFields
    VRecordAcc recordType record field spine -> do
      let quotedRecordType = quote handler level recordType
      let quotedRecord = quote handler level record
      let quotedProj = RecordProj p quotedRecordType quotedRecord field
      quoteApp handler level quotedProj spine
    where
      p = mempty

quoteApp :: (MetaLike meta) => BoundVarHandler builtin -> Lv -> Expr builtin -> GenericUnforcedSpine meta builtin -> Expr builtin
quoteApp handler level fn spine = normAppList fn $ fmap (quote handler level) spine

quoteForcedCtx ::
  (MetaLike meta) =>
  BoundVarHandler builtin ->
  Lv ->
  GenericBoundEnv meta builtin ->
  Substitution (Expr builtin)
quoteForcedCtx handler level env i = Right (quote handler level (lookupIxInEnv env i))

quoteClosure ::
  (MetaLike meta) =>
  BoundVarHandler builtin ->
  Lv ->
  (GenericBinder expr, GenericClosure meta builtin) ->
  Expr builtin
quoteClosure handler lv (binder, Closure env body) = do
  -- Here we deliberately avoid using the standard `quote . eval` approach below
  -- on the body of the lambda, in order to avoid the dependency cycles that
  -- prevent us from printing during NBE.
  let newEnv = extendEnvWithBound lv binder env
  let subst = quoteForcedCtx handler (lv + 1) newEnv
  substituteDB 0 subst (convertExprBuiltins body)
