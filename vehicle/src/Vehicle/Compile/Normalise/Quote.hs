module Vehicle.Compile.Normalise.Quote where

import Data.Map.Ordered qualified as OMap
import Vehicle.Data.AST.Expr.Scoped (Expr (..), Substitution, normAppList, substituteDB)
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Code.Value
import Vehicle.Data.Variable.Bound.Context.Name.Class (MonadReadableNameContext, getBinderDepth)
import Vehicle.Data.Variable.Bound.Index (Ix (..))
import Vehicle.Data.Variable.Bound.Level (Lv (..), dbLevelToIndex)
import Vehicle.Prelude

-- | Converts from a normalised representation to an unnormalised representation.
-- Do not call except for logging and debug purposes, very expensive with nested
-- lambdas.
unnormalise :: forall a b. (Quote a b) => Lv -> a -> b
unnormalise = quote mempty

unnormaliseInCtx ::
  forall expr m.
  (MonadReadableNameContext m, Show expr) =>
  Value expr ->
  m (Expr expr)
unnormaliseInCtx e = do
  lv <- getBinderDepth
  return $ unnormalise lv e

-----------------------------------------------------------------------------
-- Quoting closures

quoteClosure :: (ConvertableBuiltin builtin1 builtin2) => Provenance -> Lv -> (GenericBinder expr, Closure builtin1) -> Expr builtin2
quoteClosure p lv (binder, ExprClosure env body) = do
  -- Here we deliberately avoid using the standard `quote . eval` approach below
  -- on the body of the lambda, in order to avoid the dependency cycles that
  -- prevent us from printing during NBE.
  --
  -- normBody <- runReaderT (eval (liftEnvOverBinder p env) body) mempty
  -- quotedBody <- quote (level + 1) normBody
  let newEnv = extendEnvWithBound lv binder env
  let subst = quoteCtx p (lv + 1) newEnv
  substituteDB 0 subst (convertExprBuiltins body)
quoteClosure p lv (_binder, ValueClosure binderLv body) =
  quoteValueWithBinder p lv binderLv body

quoteCtx :: (ConvertableBuiltin builtin1 builtin2) => Provenance -> Lv -> BoundEnv builtin1 -> Substitution (Expr builtin2)
quoteCtx p level env i = Right (quote p level (lookupIxInEnv env i))

-- | Quote a 'ValueClosure' body to an 'Expr': map `binderLv` to `Ix 0` and
-- other VBoundVars via `dbLevelToIndex` at level `outerLv + 1`.
quoteValueWithBinder ::
  forall builtin1 builtin2.
  (ConvertableBuiltin builtin1 builtin2) =>
  Provenance ->
  Lv ->
  Lv ->
  Value builtin1 ->
  Expr builtin2
quoteValueWithBinder p outerLv binderLv = go (outerLv + 1)
  where
    -- `currentLv` is the depth at which we're quoting; bumps under inner Lam/Pi.
    go :: Lv -> Value builtin1 -> Expr builtin2
    go currentLv = \case
      VUniverse u -> Universe p u
      VMeta m spine ->
        quoteAppWith currentLv (Meta p m) spine
      VFreeVar v spine ->
        quoteAppWith currentLv (FreeVar p v) spine
      VBoundVar v spine ->
        let var = BoundVar p (boundIx currentLv v)
         in quoteAppWith currentLv var spine
      VBuiltin b spine ->
        quoteAppWith currentLv (convertBuiltin p b) spine
      VRecord typ fields ->
        Record p (go currentLv typ) (mapRecordFields (go currentLv) (OMap.assocs fields))
      VRecordAcc typ rec field spine ->
        let proj = RecordProj p (go currentLv typ) (go currentLv rec) field
         in quoteAppWith currentLv proj spine
      VPi binder closure -> Pi p (quoteBinder currentLv binder) (quoteInner currentLv binder closure)
      VLam binder closure -> Lam mempty (quoteBinder currentLv binder) (quoteInner currentLv binder closure)

    -- `binderLv` → Ix relative to the new Lam (0 at outermost, shifts up
    -- under inner binders). All other Lvs use the standard conversion.
    boundIx :: Lv -> Lv -> Ix
    boundIx currentLv v
      | v == binderLv = Ix (unLv currentLv - unLv outerLv - 1)
      | otherwise = dbLevelToIndex currentLv v

    quoteAppWith :: Lv -> Expr builtin2 -> [GenericArg (Value builtin1)] -> Expr builtin2
    quoteAppWith currentLv fn spine =
      normAppList fn (fmap (fmap (go currentLv)) spine)

    quoteBinder :: Lv -> VBinder builtin1 -> GenericBinder (Expr builtin2)
    quoteBinder currentLv = fmap (go currentLv)

    quoteInner :: Lv -> VBinder builtin1 -> Closure builtin1 -> Expr builtin2
    quoteInner currentLv binder = \case
      ExprClosure env body ->
        let newEnv = extendEnvWithBound currentLv binder env
            subst i = Right (go (currentLv + 1) (lookupIxInEnv newEnv i))
         in substituteDB 0 subst (convertExprBuiltins body)
      ValueClosure innerLv innerBody ->
        quoteValueWithBinder p currentLv innerLv innerBody

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
    VRecord recordType fields -> do
      let quotedRecordType = quote p level recordType
      let quotedFields = mapRecordFields (quote p level) $ OMap.assocs fields
      Record p quotedRecordType quotedFields
    VRecordAcc recordType record field spine -> do
      let quotedRecordType = quote p level recordType
      let quotedRecord = quote p level record
      let quotedProj = RecordProj p quotedRecordType quotedRecord field
      quoteApp level p quotedProj spine

instance (Quote expr1 expr2) => Quote (GenericBinder expr1) (GenericBinder expr2) where
  quote p level = fmap (quote p level)

instance (Quote expr1 expr2) => Quote (GenericArg expr1) (GenericArg expr2) where
  quote p level = fmap (quote p level)

quoteApp :: (Quote a (Expr builtin2)) => Lv -> Provenance -> Expr builtin2 -> [GenericArg a] -> Expr builtin2
quoteApp l p fn spine = normAppList fn $ fmap (quote p l) spine
