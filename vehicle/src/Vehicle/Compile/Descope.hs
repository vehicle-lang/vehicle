module Vehicle.Compile.Descope
  ( DescopableClosure (..),
    descopeExpr,
    descopeExprInEmptyCtx,
    descopeExprNaively,
    descopeValueNaively,
    descopeRelExprInEmptyCtx,
    genericDescopeRelExpr,
    genericDescopeExpr,
    ixToName,
  )
where

import Control.Monad.Reader (MonadReader (..), runReader)
import Vehicle.Backend.LossFunction.Core (LossClosure (..), MixedClosure (..))
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Interface (ConvertableBuiltin, convertBuiltin)
import Vehicle.Data.Builtin.Standard.Core (Builtin)
import Vehicle.Data.Expr.Relevant (RelBinder, RelExpr)
import Vehicle.Data.Expr.Relevant qualified as R
import Vehicle.Data.Expr.Value

--------------------------------------------------------------------------------
-- Interface

descopeExpr ::
  Contextualised (Expr Ix builtin) NamedBoundCtx ->
  Expr Name builtin
descopeExpr (WithContext e ctx) = runReader (genericDescopeExpr (ixToName Named) e) ctx

descopeExprInEmptyCtx ::
  Expr Ix builtin ->
  Expr Name builtin
descopeExprInEmptyCtx e = descopeExpr (WithContext e mempty)

descopeExprNaively ::
  Expr Ix builtin ->
  Expr Name builtin
descopeExprNaively e = runReader (genericDescopeExpr (ixToName Naive) e) mempty

-- | Note that you cannot descope `Value` non-naively as you can't descope
-- closures properly. You have to quote the `Value` first.
descopeValueNaively ::
  (ConvertableBuiltin builtin1 builtin2, DescopableClosure closure builtin2) =>
  Value closure builtin1 ->
  Expr Name builtin2
descopeValueNaively e = runReader (genericDescopeValue Naive e) mempty

descopeRelExprInEmptyCtx ::
  RelExpr Ix builtin ->
  RelExpr Name builtin
descopeRelExprInEmptyCtx e = runReader (genericDescopeRelExpr (ixToName Named) e) mempty

--------------------------------------------------------------------------------
-- Monad

type MonadDescope m = MonadReader NamedBoundCtx m

addBinderToCtx :: (MonadDescope m, HasName binder (Maybe Name)) => binder -> m a -> m a
addBinderToCtx binder = local (nameOf binder :)

addBindersToCtx :: (MonadDescope m, HasName binder (Maybe Name)) => [binder] -> m a -> m a
addBindersToCtx binders = local (fmap nameOf (reverse binders) <>)

--------------------------------------------------------------------------------
-- Variable conversion methods

type VarConversion var m = (MonadDescope m) => Provenance -> var -> m Name

data VarStrategy = Named | Naive

ixToName :: VarStrategy -> VarConversion Ix m
ixToName s p ix = case s of
  Naive -> return $ layoutAsText $ pretty ix
  Named -> do
    ctx <- ask
    case lookupIx ctx ix of
      Nothing -> varOutOfBounds "DeBruijn index" p ix (length ctx)
      Just Nothing -> return "_"
      Just (Just name) -> return name

lvToName :: VarStrategy -> VarConversion Lv m
lvToName s p lv = case s of
  Naive -> return $ layoutAsText $ pretty lv
  Named -> do
    ctx <- ask
    case lookupLv ctx lv of
      Nothing -> varOutOfBounds "DeBruijn level" p lv (length ctx)
      Just Nothing -> return "_"
      Just (Just name) -> return name

--------------------------------------------------------------------------------
-- Expr

genericDescopeExpr ::
  (MonadDescope m, Show var) =>
  VarConversion var m ->
  Expr var builtin ->
  m (Expr Name builtin)
genericDescopeExpr f e = showScopeExit $ case showScopeEntry e of
  Universe p l -> return $ Universe p l
  Hole p name -> return $ Hole p name
  Builtin p op -> return $ convertBuiltin p op
  Meta p i -> return $ Meta p i
  FreeVar p v -> return $ FreeVar p v
  BoundVar p v -> BoundVar p <$> f p v
  App fun args -> do
    fun' <- genericDescopeExpr f fun
    args' <- traverse (traverse (genericDescopeExpr f)) args
    return $ App fun' args'
  Let p bound binder body -> do
    bound' <- genericDescopeExpr f bound
    binder' <- traverse (genericDescopeExpr f) binder
    body' <- addBinderToCtx binder' (genericDescopeExpr f body)
    return $ Let p bound' binder' body'
  Lam p binder body -> do
    binder' <- traverse (genericDescopeExpr f) binder
    body' <- addBinderToCtx binder' (genericDescopeExpr f body)
    return $ Lam p binder' body'
  Pi p binder body -> do
    binder' <- traverse (genericDescopeExpr f) binder
    body' <- addBinderToCtx binder' (genericDescopeExpr f body)
    return $ Pi p binder' body'

--------------------------------------------------------------------------------
-- Value

class DescopableClosure closure builtin where
  descopeClosure :: (MonadDescope m) => VarStrategy -> GenericBinder binder -> closure -> m (Expr Name builtin)

instance (ConvertableBuiltin builtin Builtin) => DescopableClosure (WHNFClosure builtin) Builtin where
  descopeClosure :: forall m binder. (MonadDescope m) => VarStrategy -> GenericBinder binder -> WHNFClosure builtin -> m (Expr Name Builtin)
  descopeClosure f _binder (WHNFClosure env body) = do
    body' <- genericDescopeExpr (ixToName f) $ convertExprBuiltins body
    env' <- traverse (genericDescopeValue f) (cheatEnvToValues env) :: m [Expr Name Builtin]
    let envExpr = normAppList (BoundVar mempty "ENV") $ fmap (Arg mempty Explicit Relevant) env'
    return $ App envExpr [explicit body']

-- genericDescopeExpr (ixToName f) $ convertExprBuiltins body

instance DescopableClosure (NFClosure builtin) builtin where
  descopeClosure f _binder (NFClosure body) = genericDescopeValue f body

instance DescopableClosure MixedClosure Builtin where
  descopeClosure :: forall m binder. (MonadDescope m) => VarStrategy -> GenericBinder binder -> MixedClosure -> m (Expr Name Builtin)
  descopeClosure f binder = \case
    StandardClos closure -> descopeClosure f binder closure
    LossClos (LossClosure env body) -> do
      body' <- genericDescopeExpr (ixToName f) $ convertExprBuiltins body
      env' <- traverse (genericDescopeValue f) (cheatEnvToValues env) :: m [Expr Name Builtin]
      let envExpr = normAppList (BoundVar mempty "ENV") $ fmap (Arg mempty Explicit Relevant) env'
      return $ App envExpr [explicit body']

-- | This function is not meant to do anything sensible and is merely
-- used for printing `WHNF`s in a readable form.
genericDescopeValue ::
  (MonadDescope m, DescopableClosure closure builtin2, ConvertableBuiltin builtin1 builtin2) =>
  VarStrategy ->
  Value closure builtin1 ->
  m (Expr Name builtin2)
genericDescopeValue f e = case e of
  VUniverse u ->
    return $ Universe p u
  VMeta m spine ->
    normAppList (Meta p m) <$> traverseArgs (genericDescopeValue f) spine
  VFreeVar v spine ->
    normAppList (FreeVar p v) <$> traverseArgs (genericDescopeValue f) spine
  VBuiltin b spine ->
    normAppList (convertBuiltin p b) <$> traverseArgs (genericDescopeValue f) spine
  VBoundVar v spine -> do
    var <- BoundVar p <$> lvToName f p v
    args <- traverseArgs (genericDescopeValue f) spine
    return $ normAppList var args
  VPi binder body -> do
    binder' <- traverse (genericDescopeValue f) binder
    body' <- addBinderToCtx binder $ genericDescopeValue f body
    return $ Pi p binder' body'
  VLam binder closure -> do
    binder' <- traverse (genericDescopeValue f) binder
    body' <- addBinderToCtx binder $ descopeClosure f binder closure
    return $ Lam p binder' body'
  where
    p = mempty

--------------------------------------------------------------------------------
-- RelExpr

genericDescopeRelExpr ::
  (MonadDescope m, Show var) =>
  VarConversion var m ->
  RelExpr var builtin ->
  m (RelExpr Name builtin)
genericDescopeRelExpr f e = case e of
  R.Universe p l -> return $ R.Universe p l
  R.Builtin p op -> return $ R.Builtin p op
  R.FreeVar p v -> return $ R.FreeVar p v
  R.BoundVar p v -> R.BoundVar p <$> f p v
  R.App fun args -> R.App <$> genericDescopeRelExpr f fun <*> traverse (genericDescopeRelExpr f) args
  R.PartialApp arity fun args -> R.PartialApp arity <$> genericDescopeRelExpr f fun <*> traverse (genericDescopeRelExpr f) args
  R.Let p bound binder body -> do
    bound' <- genericDescopeRelExpr f bound
    binder' <- descopeRelBinder f binder
    body' <- addBinderToCtx binder' (genericDescopeRelExpr f body)
    return $ R.Let p bound' binder' body'
  R.Lam p binders body -> do
    binders' <- descopeRelBinders f binders
    body' <- addBindersToCtx binders' (genericDescopeRelExpr f body)
    return $ R.Lam p binders' body'
  R.Pi p binder body -> do
    binder' <- descopeRelBinder f binder
    body' <- addBinderToCtx binder' (genericDescopeRelExpr f body)
    return $ R.Pi p binder' body'

descopeRelBinder ::
  (MonadDescope m, Show var) =>
  VarConversion var m ->
  RelBinder var builtin ->
  m (RelBinder Name builtin)
descopeRelBinder f (R.Binder p r t) = R.Binder p r <$> genericDescopeRelExpr f t

descopeRelBinders ::
  (MonadDescope m, Show var) =>
  VarConversion var m ->
  [RelBinder var builtin] ->
  m [RelBinder Name builtin]
descopeRelBinders f = \case
  [] -> return []
  (b : bs) -> do
    b' <- descopeRelBinder f b
    bs' <- addBinderToCtx b $ descopeRelBinders f bs
    return $ b' : bs'

--------------------------------------------------------------------------------
-- Logging and errors

showScopeEntry :: (Show var) => Expr var builtin -> Expr var builtin
showScopeEntry e = e

showScopeExit :: (Monad m) => m (Expr Name builtin) -> m (Expr Name builtin)
showScopeExit m = do
  e <- m
  return e

-- | Throw an |IndexOutOfBounds| error using an arbitrary var.
varOutOfBounds :: (MonadDescope m, Pretty var) => Doc a -> Provenance -> var -> Int -> m a
varOutOfBounds varType p var ctxSize =
  developerError $
    "During descoping found"
      <+> varType
      <+> pretty var
      <+> "greater than current context size"
      <+> pretty ctxSize
      <+> parens (pretty p)
