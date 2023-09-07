module Vehicle.Compile.Descope
  ( DescopeNamed (..),
    DescopeNaive (..),
  )
where

import Control.Monad.Reader (MonadReader (..), Reader, runReader)
import Vehicle.Compile.Prelude
import Vehicle.Data.NormalisedExpr
import Vehicle.Data.RelevantExpr (RelBinder, RelExpr, RelProg)
import Vehicle.Data.RelevantExpr qualified as R

-------------------------------------------------------------------------------
-- Named descoping

-- | Converts DB indices back to names
class DescopeNamed a b | a -> b where
  descopeNamed :: a -> b

instance DescopeNamed (Prog Ix builtin) (Prog Name builtin) where
  descopeNamed = fmap (runWithNoCtx descopeNamed)

instance DescopeNamed (Decl Ix builtin) (Decl Name builtin) where
  descopeNamed = fmap (runWithNoCtx descopeNamed)

instance DescopeNamed (Contextualised (Expr Ix builtin) NamedBoundCtx) (Expr Name builtin) where
  descopeNamed = performDescoping descopeDBIndexVar

instance
  (DescopeNamed (Contextualised expr1 NamedBoundCtx) expr2) =>
  DescopeNamed (Contextualised (GenericArg expr1) NamedBoundCtx) (GenericArg expr2)
  where
  descopeNamed (WithContext arg ctx) = fmap (\e -> descopeNamed (WithContext e ctx)) arg

instance
  (DescopeNamed (Contextualised expr1 NamedBoundCtx) expr2) =>
  DescopeNamed (Contextualised (GenericBinder expr1) NamedBoundCtx) (GenericBinder expr2)
  where
  descopeNamed (WithContext binder ctx) = fmap (\e -> descopeNamed (WithContext e ctx)) binder

instance DescopeNamed (RelProg Ix builtin) (RelProg Name builtin) where
  descopeNamed = R.fmapRelProg (\e -> descopeNamed (WithContext e emptyNamedCtx))

instance DescopeNamed (Contextualised (RelExpr Ix builtin) NamedBoundCtx) (RelExpr Name builtin) where
  descopeNamed (WithContext e ctx) = runReader (descopeRelExpr descopeDBIndexVar e) ctx

--------------------------------------------------------------------------------
-- Naive descoping

-- | Converts DB indices to names naively (i.e. index 0 -> "i0").
-- Used for debugging purposes.
class DescopeNaive a b | a -> b where
  descopeNaive :: a -> b

instance DescopeNaive (Prog Ix builtin) (Prog Name builtin) where
  descopeNaive = fmap descopeNaive

instance DescopeNaive (Decl Ix builtin) (Decl Name builtin) where
  descopeNaive = fmap descopeNaive

instance DescopeNaive (Expr Ix builtin) (Expr Name builtin) where
  descopeNaive = runWithNoCtx (performDescoping descopeDBIndexVarNaive)

instance
  (DescopeNaive expr1 expr2) =>
  DescopeNaive (GenericArg expr1) (GenericArg expr2)
  where
  descopeNaive = fmap descopeNaive

instance
  (DescopeNaive expr1 expr2) =>
  DescopeNaive (GenericBinder expr1) (GenericBinder expr2)
  where
  descopeNaive = fmap descopeNaive

instance DescopeNaive (WHNFValue builtin) (Expr Name builtin) where
  descopeNaive = descopeNormExpr descopeDBLevelVarNaive

--------------------------------------------------------------------------------
-- Core utils

type MonadDescope m = MonadReader NamedBoundCtx m

runWithNoCtx :: (Contextualised a NamedBoundCtx -> b) -> a -> b
runWithNoCtx run e = run (WithContext e mempty)

addBinderToCtx :: (HasName binder (Maybe Name)) => binder -> NamedBoundCtx -> NamedBoundCtx
addBinderToCtx binder ctx = nameOf binder : ctx

addBindersToCtx :: (HasName binder (Maybe Name)) => [binder] -> NamedBoundCtx -> NamedBoundCtx
addBindersToCtx binders ctx = fmap nameOf (reverse binders) <> ctx

descopeDBIndexVarNaive :: (MonadDescope m) => Provenance -> Ix -> m Name
descopeDBIndexVarNaive _ i = return $ layoutAsText (pretty i)

descopeDBLevelVarNaive :: Provenance -> Lv -> Name
descopeDBLevelVarNaive _ l = layoutAsText $ pretty l

--------------------------------------------------------------------------------
-- Expr

performDescoping ::
  (Show var) =>
  (Provenance -> var -> Reader NamedBoundCtx Name) ->
  Contextualised (Expr var builtin) NamedBoundCtx ->
  Expr Name builtin
performDescoping convertVar (WithContext e ctx) =
  runReader (descopeExpr convertVar e) ctx

descopeExpr ::
  (MonadDescope m, Show var) =>
  (Provenance -> var -> m Name) ->
  Expr var builtin ->
  m (Expr Name builtin)
descopeExpr f e = showScopeExit $ case showScopeEntry e of
  Universe p l -> return $ Universe p l
  Hole p name -> return $ Hole p name
  Builtin p op -> return $ Builtin p op
  Meta p i -> return $ Meta p i
  FreeVar p v -> return $ FreeVar p v
  BoundVar p v -> BoundVar p <$> f p v
  App p fun args -> App p <$> descopeExpr f fun <*> traverse (descopeArg f) args
  Let p bound binder body -> do
    bound' <- descopeExpr f bound
    binder' <- descopeBinder f binder
    body' <- local (addBinderToCtx binder') (descopeExpr f body)
    return $ Let p bound' binder' body'
  Lam p binder body -> do
    binder' <- descopeBinder f binder
    body' <- local (addBinderToCtx binder') (descopeExpr f body)
    return $ Lam p binder' body'
  Pi p binder body -> do
    binder' <- descopeBinder f binder
    body' <- local (addBinderToCtx binder') (descopeExpr f body)
    return $ Pi p binder' body'

descopeBinder ::
  (MonadReader NamedBoundCtx f, Show var) =>
  (Provenance -> var -> f Name) ->
  Binder var builtin ->
  f (Binder Name builtin)
descopeBinder f = traverse (descopeExpr f)

descopeArg ::
  (MonadReader NamedBoundCtx f, Show var) =>
  (Provenance -> var -> f Name) ->
  Arg var builtin ->
  f (Arg Name builtin)
descopeArg f = traverse (descopeExpr f)

--------------------------------------------------------------------------------
-- RelExpr

descopeRelExpr ::
  (MonadDescope m, Show var) =>
  (Provenance -> var -> m Name) ->
  RelExpr var builtin ->
  m (RelExpr Name builtin)
descopeRelExpr f e = case e of
  R.Universe p l -> return $ R.Universe p l
  R.Builtin p op -> return $ R.Builtin p op
  R.FreeVar p v -> return $ R.FreeVar p v
  R.BoundVar p v -> R.BoundVar p <$> f p v
  R.App p fun args -> R.App p <$> descopeRelExpr f fun <*> traverse (descopeRelExpr f) args
  R.PartialApp p arity fun args -> R.PartialApp p arity <$> descopeRelExpr f fun <*> traverse (descopeRelExpr f) args
  R.Let p bound binder body -> do
    bound' <- descopeRelExpr f bound
    binder' <- descopeRelBinder f binder
    body' <- local (addBinderToCtx binder') (descopeRelExpr f body)
    return $ R.Let p bound' binder' body'
  R.Lam p binders body -> do
    binders' <- descopeRelBinders f binders
    body' <- local (addBindersToCtx binders') (descopeRelExpr f body)
    return $ R.Lam p binders' body'
  R.Pi p binder body -> do
    binder' <- descopeRelBinder f binder
    body' <- local (addBinderToCtx binder') (descopeRelExpr f body)
    return $ R.Pi p binder' body'

descopeRelBinder ::
  (MonadDescope m, Show var) =>
  (Provenance -> var -> m Name) ->
  RelBinder var builtin ->
  m (RelBinder Name builtin)
descopeRelBinder f (R.Binder p r t) = R.Binder p r <$> descopeRelExpr f t

descopeRelBinders ::
  (MonadDescope m, Show var) =>
  (Provenance -> var -> m Name) ->
  [RelBinder var builtin] ->
  m [RelBinder Name builtin]
descopeRelBinders f = \case
  [] -> return []
  (b : bs) -> do
    b' <- descopeRelBinder f b
    bs' <- local (addBinderToCtx b) $ descopeRelBinders f bs
    return $ b' : bs'

--------------------------------------------------------------------------------
-- Values

-- | This function is not meant to do anything sensible and is merely
-- used for printing `WHNF`s in a readable form.
descopeNormExpr ::
  (Provenance -> Lv -> Name) ->
  WHNFValue builtin ->
  Expr Name builtin
descopeNormExpr f e = case e of
  VUniverse u -> Universe p u
  VMeta m spine -> normAppList p (Meta p m) $ descopeSpine f spine
  VFreeVar v spine -> normAppList p (FreeVar p v) $ descopeSpine f spine
  VBuiltin b spine -> normAppList p (Builtin p b) $ descopeSpine f spine
  VBoundVar v spine -> do
    let var = BoundVar p $ f p v
    let args = descopeSpine f spine
    normAppList p var args
  VPi binder body -> do
    let binder' = descopeNormBinder f binder
    let body' = descopeNormExpr f body
    Pi p binder' body'
  VLam binder (WHNFBody _env body) -> do
    let binder' = descopeNormBinder f binder
    let body' = descopeNaive body
    -- let env' = fmap (descopeNormExpr f) env
    -- let envExpr = normAppList p (BoundVar p "ENV") $ fmap (ExplicitArg p) env'
    -- Lam p binder' (App p envExpr [ExplicitArg p body'])
    Lam p binder' body'
  where
    p = mempty

descopeSpine ::
  (Provenance -> Lv -> Name) ->
  WHNFSpine builtin ->
  [Arg Name builtin]
descopeSpine f = fmap (fmap (descopeNormExpr f))

descopeNormBinder ::
  (Provenance -> Lv -> Name) ->
  WHNFBinder builtin ->
  Binder Name builtin
descopeNormBinder f = fmap (descopeNormExpr f)

descopeDBIndexVar :: (MonadDescope m) => Provenance -> Ix -> m Name
descopeDBIndexVar p i = do
  ctx <- ask
  case lookupIx ctx i of
    Nothing -> indexOutOfBounds p i (length ctx)
    Just Nothing -> return "_"
    Just (Just name) -> return name

--------------------------------------------------------------------------------
-- Logging and errors

showScopeEntry :: (Show var) => Expr var builtin -> Expr var builtin
showScopeEntry e =
  e

showScopeExit :: (MonadDescope m) => m (Expr Name builtin) -> m (Expr Name builtin)
showScopeExit m = do
  e <- m
  return e

-- | Throw an |IndexOutOfBounds| error using an arbitrary index.
indexOutOfBounds :: (MonadDescope m) => Provenance -> Ix -> Int -> m a
indexOutOfBounds p index ctxSize =
  developerError $
    "During descoping found DeBruijn index"
      <+> pretty index
      <+> "greater than current context size"
      <+> pretty ctxSize
      <+> parens (pretty p)
