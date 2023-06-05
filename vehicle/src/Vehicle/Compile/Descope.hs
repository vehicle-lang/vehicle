module Vehicle.Compile.Descope
  ( DescopeNamed (..),
    DescopeNaive (..),
  )
where

import Control.Monad.Reader (MonadReader (..), Reader, runReader)
import Vehicle.Compile.Prelude
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalisable
import Vehicle.Expr.Normalised (Spine, VBinder, Value (..))

--------------------------------------------------------------------------------
-- Public interface
{-
-- | Converts DeBruijn indices into names naively, e.g. 0 becomes "i0".
--  Useful for debugging
runNaiveCoDBDescope ::
  (Descope t, ExtractPositionTrees t) =>
  t (CoDBBinding Name) CoDBVar ->
  (t Name Name, Map Name (Maybe PositionTree))
runNaiveCoDBDescope e1 =
  let (e2, pts) = extractPTs e1
    in let e3 = performDescoping descopeCoDBVarNaive (WithContext e2 mempty)
      in (e3, pts)

-}

--------------------------------------------------------------------------------
-- Named descoping

-- | Converts DB indices back to names
class DescopeNamed a b | a -> b where
  descopeNamed :: a -> b

instance DescopeNamed (Prog Ix builtin) (Prog Name builtin) where
  descopeNamed = fmap (runWithNoCtx descopeNamed)

instance DescopeNamed (Decl Ix builtin) (Decl Name builtin) where
  descopeNamed = fmap (runWithNoCtx descopeNamed)

instance DescopeNamed (Contextualised (Expr Ix builtin) BoundDBCtx) (Expr Name builtin) where
  descopeNamed = performDescoping descopeDBIndexVar

instance
  (DescopeNamed (Contextualised expr1 BoundDBCtx) expr2) =>
  DescopeNamed (Contextualised (GenericArg expr1) BoundDBCtx) (GenericArg expr2)
  where
  descopeNamed (WithContext arg ctx) = fmap (\e -> descopeNamed (WithContext e ctx)) arg

instance
  (DescopeNamed (Contextualised expr1 BoundDBCtx) expr2) =>
  DescopeNamed (Contextualised (GenericBinder expr1) BoundDBCtx) (GenericBinder expr2)
  where
  descopeNamed (WithContext binder ctx) = fmap (\e -> descopeNamed (WithContext e ctx)) binder

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

instance DescopeNaive (Value types) (Expr Name (NormalisableBuiltin types)) where
  descopeNaive = descopeNormExpr descopeDBLevelVarNaive

--------------------------------------------------------------------------------
-- Core operation

-- Can get rid of this newtype and just use BoundDBCtx instead?
newtype Ctx = Ctx BoundDBCtx

runWithNoCtx :: (Contextualised a BoundDBCtx -> b) -> a -> b
runWithNoCtx run e = run (WithContext e mempty)

addBinderToCtx :: Binder var builtin -> Ctx -> Ctx
addBinderToCtx binder (Ctx ctx) = Ctx (nameOf binder : ctx)

performDescoping ::
  (Show var) =>
  (Provenance -> var -> Reader Ctx Name) ->
  Contextualised (Expr var builtin) BoundDBCtx ->
  Expr Name builtin
performDescoping convertVar (WithContext e ctx) =
  runReader (descopeExpr convertVar e) (Ctx ctx)

type MonadDescope m = MonadReader Ctx m

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
  Ann p e1 t -> Ann p <$> descopeExpr f e1 <*> descopeExpr f t
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
  (MonadReader Ctx f, Show var) =>
  (Provenance -> var -> f Name) ->
  Binder var builtin ->
  f (Binder Name builtin)
descopeBinder f = traverse (descopeExpr f)

descopeArg ::
  (MonadReader Ctx f, Show var) =>
  (Provenance -> var -> f Name) ->
  Arg var builtin ->
  f (Arg Name builtin)
descopeArg f = traverse (descopeExpr f)

-- | This function is not meant to do anything sensible and is merely
-- used for printing `Value`s in a readable form.
descopeNormExpr ::
  (Provenance -> Lv -> Name) ->
  Value types ->
  Expr Name (NormalisableBuiltin types)
descopeNormExpr f e = case e of
  VUniverse u -> Universe p u
  VMeta m spine -> normAppList p (Meta p m) $ descopeSpine f spine
  VFreeVar v spine -> normAppList p (FreeVar p v) $ descopeSpine f spine
  VBuiltin b spine -> normAppList p (Builtin p b) $ fmap (ExplicitArg p . descopeNormExpr f) spine
  VBoundVar v spine -> do
    let var = BoundVar p $ f p v
    let args = descopeSpine f spine
    normAppList p var args
  VPi binder body -> do
    let binder' = descopeNormBinder f binder
    let body' = descopeNormExpr f body
    Pi p binder' body'
  VLam binder _env body -> do
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
  Spine types ->
  [Arg Name (NormalisableBuiltin types)]
descopeSpine f = fmap (fmap (descopeNormExpr f))

descopeNormBinder ::
  (Provenance -> Lv -> Name) ->
  VBinder types ->
  Binder Name (NormalisableBuiltin types)
descopeNormBinder f = fmap (descopeNormExpr f)

descopeDBIndexVar :: (MonadDescope m) => Provenance -> Ix -> m Name
descopeDBIndexVar p i = do
  Ctx ctx <- ask
  case lookupVar ctx i of
    Nothing -> indexOutOfBounds p i (length ctx)
    Just Nothing -> return "_" -- usingUnnamedBoundVariable p i
    Just (Just name) -> return name

descopeDBIndexVarNaive :: (MonadDescope m) => Provenance -> Ix -> m Name
descopeDBIndexVarNaive _ i = return $ layoutAsText (pretty i)

descopeDBLevelVarNaive :: Provenance -> Lv -> Name
descopeDBLevelVarNaive _ l = layoutAsText $ pretty l

{-
descopeCoDBVarNaive :: MonadDescope m => Provenance -> CoDBVar -> m Name
descopeCoDBVarNaive _ = \case
  CoDBFree i -> return $ nameOf i
  CoDBBound -> return "CoDBVar"
-}
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

{-
-- | Use of unnamed bound variable error using an arbitrary index.
usingUnnamedBoundVariable :: MonadDescope m => Provenance -> Ix -> m a
usingUnnamedBoundVariable p index =
  developerError $
    "During descoping found use of unnamed bound variable"
      <+> pretty index
      <+> parens (pretty p)
-}
