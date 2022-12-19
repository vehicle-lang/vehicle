module Vehicle.Compile.Descope
  ( DescopeNamed (..),
    DescopeNaive (..),
  )
where

import Control.Monad.Reader (MonadReader (..), Reader, runReader)
import Data.Text (pack)
import Vehicle.Compile.Prelude
import Vehicle.Expr.DeBruijn

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

instance DescopeNamed (Prog NamedBinding DBIndexVar) NamedProg where
  descopeNamed = fmap (runWithNoCtx descopeNamed)

instance DescopeNamed (Decl NamedBinding DBIndexVar) NamedDecl where
  descopeNamed = fmap (runWithNoCtx descopeNamed)

instance DescopeNamed (Contextualised (Expr NamedBinding DBIndexVar) NamedBoundCtx) NamedExpr where
  descopeNamed = performDescoping descopeDBVar

instance
  DescopeNamed (Contextualised expr1 NamedBoundCtx) expr2 =>
  DescopeNamed (Contextualised (GenericArg expr1) NamedBoundCtx) (GenericArg expr2)
  where
  descopeNamed (WithContext arg ctx) = fmap (\e -> descopeNamed (WithContext e ctx)) arg

instance
  DescopeNamed (Contextualised expr1 NamedBoundCtx) expr2 =>
  DescopeNamed (Contextualised (GenericBinder binder expr1) NamedBoundCtx) (GenericBinder binder expr2)
  where
  descopeNamed (WithContext binder ctx) = fmap (\e -> descopeNamed (WithContext e ctx)) binder

--------------------------------------------------------------------------------
-- Naive descoping

-- | Converts DB indices to names naively (i.e. index 0 -> "i0").
-- Used for debugging purposes.
class DescopeNaive a b | a -> b where
  descopeNaive :: a -> b

instance DescopeNaive (Prog NamedBinding DBIndexVar) NamedProg where
  descopeNaive = fmap descopeNaive

instance DescopeNaive (Decl NamedBinding DBIndexVar) NamedDecl where
  descopeNaive = fmap descopeNaive

instance DescopeNaive (Expr NamedBinding DBIndexVar) NamedExpr where
  descopeNaive = runWithNoCtx (performDescoping descopeDBVarNaive)

instance
  DescopeNaive expr1 expr2 =>
  DescopeNaive (GenericArg expr1) (GenericArg expr2)
  where
  descopeNaive = fmap descopeNaive

instance
  DescopeNaive expr1 expr2 =>
  DescopeNaive (GenericBinder binder expr1) (GenericBinder binder expr2)
  where
  descopeNaive = fmap descopeNaive

--------------------------------------------------------------------------------
-- Core operation

-- Can get rid of this newtype and just use NamedBoundCtx instead?
newtype Ctx = Ctx [NamedBinding]

runWithNoCtx :: (Contextualised a NamedBoundCtx -> b) -> a -> b
runWithNoCtx run e = run (WithContext e mempty)

addBinderToCtx :: NamedBinder -> Ctx -> Ctx
addBinderToCtx binder (Ctx ctx) = Ctx (nameOf binder : ctx)

performDescoping ::
  Show var =>
  (Provenance -> var -> Reader Ctx Name) ->
  Contextualised (Expr NamedBinding var) [NamedBinding] ->
  Expr NamedBinding Name
performDescoping convertVar (WithContext e ctx) =
  runReader (descope convertVar e) (Ctx ctx)

type MonadDescope m = MonadReader Ctx m

descope ::
  (MonadDescope m, Show var) =>
  (Provenance -> var -> m Name) ->
  Expr NamedBinding var ->
  m (Expr NamedBinding Name)
descope f e = showScopeExit $ case showScopeEntry e of
  Universe ann l -> return $ Universe ann l
  Hole ann name -> return $ Hole ann name
  Builtin ann op -> return $ Builtin ann op
  Literal ann l -> return $ Literal ann l
  Meta ann i -> return $ Meta ann i
  Var ann v -> Var ann <$> f ann v
  Ann ann e1 t -> Ann ann <$> descope f e1 <*> descope f t
  App ann fun args -> App ann <$> descope f fun <*> traverse (descopeArg f) args
  LVec ann es -> LVec ann <$> traverse (descope f) es
  Let ann bound binder body -> do
    bound' <- descope f bound
    binder' <- descopeBinder f binder
    body' <- local (addBinderToCtx binder') (descope f body)
    return $ Let ann bound' binder' body'
  Lam ann binder body -> do
    binder' <- descopeBinder f binder
    body' <- local (addBinderToCtx binder') (descope f body)
    return $ Lam ann binder' body'
  Pi ann binder body -> do
    binder' <- descopeBinder f binder
    body' <- local (addBinderToCtx binder') (descope f body)
    return $ Pi ann binder' body'

descopeBinder ::
  (MonadReader Ctx f, Show var) =>
  (Provenance -> var -> f Name) ->
  GenericBinder NamedBinding (Expr NamedBinding var) ->
  f (GenericBinder NamedBinding (Expr NamedBinding Name))
descopeBinder f = traverse (descope f)

descopeArg ::
  (MonadReader Ctx f, Show var) =>
  (Provenance -> var -> f Name) ->
  GenericArg (Expr NamedBinding var) ->
  f (GenericArg (Expr NamedBinding Name))
descopeArg f = traverse (descope f)

descopeDBVar :: MonadDescope m => Provenance -> DBIndexVar -> m Name
descopeDBVar _ (Free ident) = return $ nameOf ident
descopeDBVar p (Bound i) = do
  Ctx ctx <- ask
  case lookupVar ctx i of
    Nothing -> indexOutOfBounds p i (length ctx)
    Just x -> return x

descopeDBVarNaive :: MonadDescope m => Provenance -> DBIndexVar -> m Name
descopeDBVarNaive _ = \case
  Free i -> return $ nameOf i
  Bound i -> return $ pack ("i" <> show i)

{-
descopeCoDBVarNaive :: MonadDescope m => Provenance -> CoDBVar -> m Name
descopeCoDBVarNaive _ = \case
  CoDBFree i -> return $ nameOf i
  CoDBBound -> return "CoDBVar"
-}
--------------------------------------------------------------------------------
-- Logging and errors

showScopeEntry :: Show var => Expr NamedBinding var -> Expr NamedBinding var
showScopeEntry e =
  e

showScopeExit :: MonadDescope m => m NamedExpr -> m NamedExpr
showScopeExit m = do
  e <- m
  return e

-- | Throw an |IndexOutOfBounds| error using an arbitrary index.
indexOutOfBounds :: MonadDescope m => Provenance -> DBIndex -> Int -> m a
indexOutOfBounds p index ctxSize =
  developerError $
    "During descoping found DeBruijn index"
      <+> pretty index
      <+> "greater than current context size"
      <+> pretty ctxSize
      <+> parens (pretty p)
