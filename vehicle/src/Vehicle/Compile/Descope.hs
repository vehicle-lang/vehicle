module Vehicle.Compile.Descope
  ( DescopeNamed (..),
    DescopeNaive (..),
  )
where

import Control.Monad.Reader (MonadReader (..), Reader, runReader)
import Vehicle.Compile.Prelude
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised (NormBinder, NormExpr (..), Spine)

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

instance DescopeNamed DBProg InputProg where
  descopeNamed = fmap (runWithNoCtx descopeNamed)

instance DescopeNamed DBDecl InputDecl where
  descopeNamed = fmap (runWithNoCtx descopeNamed)

instance DescopeNamed (Contextualised DBExpr BoundDBCtx) InputExpr where
  descopeNamed = performDescoping descopeDBIndexVar

instance
  DescopeNamed (Contextualised expr1 BoundDBCtx) expr2 =>
  DescopeNamed (Contextualised (GenericArg expr1) BoundDBCtx) (GenericArg expr2)
  where
  descopeNamed (WithContext arg ctx) = fmap (\e -> descopeNamed (WithContext e ctx)) arg

instance
  DescopeNamed (Contextualised expr1 BoundDBCtx) expr2 =>
  DescopeNamed (Contextualised (GenericBinder binder expr1) BoundDBCtx) (GenericBinder binder expr2)
  where
  descopeNamed (WithContext binder ctx) = fmap (\e -> descopeNamed (WithContext e ctx)) binder

--------------------------------------------------------------------------------
-- Naive descoping

-- | Converts DB indices to names naively (i.e. index 0 -> "i0").
-- Used for debugging purposes.
class DescopeNaive a b | a -> b where
  descopeNaive :: a -> b

instance DescopeNaive DBProg InputProg where
  descopeNaive = fmap descopeNaive

instance DescopeNaive DBDecl InputDecl where
  descopeNaive = fmap descopeNaive

instance DescopeNaive DBExpr InputExpr where
  descopeNaive = runWithNoCtx (performDescoping descopeDBIndexVarNaive)

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

instance DescopeNaive NormExpr InputExpr where
  descopeNaive = descopeNormExpr descopeDBLevelVarNaive

--------------------------------------------------------------------------------
-- Core operation

-- Can get rid of this newtype and just use BoundDBCtx instead?
newtype Ctx = Ctx BoundDBCtx

runWithNoCtx :: (Contextualised a BoundDBCtx -> b) -> a -> b
runWithNoCtx run e = run (WithContext e mempty)

addBinderToCtx :: Binder InputBinding var -> Ctx -> Ctx
addBinderToCtx binder (Ctx ctx) = Ctx (nameOf binder : ctx)

performDescoping ::
  Show var =>
  (Provenance -> var -> Reader Ctx Name) ->
  Contextualised (Expr DBBinding var) BoundDBCtx ->
  InputExpr
performDescoping convertVar (WithContext e ctx) =
  runReader (descopeExpr convertVar e) (Ctx ctx)

type MonadDescope m = MonadReader Ctx m

descopeExpr ::
  (MonadDescope m, Show var) =>
  (Provenance -> var -> m Name) ->
  Expr DBBinding var ->
  m InputExpr
descopeExpr f e = showScopeExit $ case showScopeEntry e of
  Universe p l -> return $ Universe p l
  Hole p name -> return $ Hole p name
  Builtin p op -> return $ Builtin p op
  Literal p l -> return $ Literal p l
  Meta p i -> return $ Meta p i
  Var p v -> Var p <$> f p v
  Ann p e1 t -> Ann p <$> descopeExpr f e1 <*> descopeExpr f t
  App p fun args -> App p <$> descopeExpr f fun <*> traverse (descopeArg f) args
  LVec p es -> LVec p <$> traverse (descopeExpr f) es
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
  Binder DBBinding var ->
  f InputBinder
descopeBinder f = traverse (descopeExpr f)

descopeArg ::
  (MonadReader Ctx f, Show var) =>
  (Provenance -> var -> f Name) ->
  Arg DBBinding var ->
  f InputArg
descopeArg f = traverse (descopeExpr f)

-- | This function is not meant to do anything sensible and is merely
-- used for printing `NormExpr`s in a readable form.
descopeNormExpr :: (Provenance -> DBLevel -> Name) -> NormExpr -> InputExpr
descopeNormExpr f e = case e of
  VUniverse p u -> Universe p u
  VLiteral p l -> Literal p l
  VMeta p m spine -> normAppList p (Meta p m) $ descopeSpine f spine
  VFreeVar p v spine -> normAppList p (Var p (nameOf v)) $ descopeSpine f spine
  VBuiltin p b spine -> normAppList p (Builtin p b) $ descopeSpine f spine
  VBoundVar p v spine -> do
    let var = Var p $ f p v
    let args = descopeSpine f spine
    normAppList p var args
  VLVec p xs _spine -> do
    let xs' = fmap (descopeNormExpr f) xs
    -- let args = descopeSpine f spine
    -- normAppList p (LVec p xs') args
    LVec p xs'
  VPi p binder body -> do
    let binder' = descopeNormBinder f binder
    let body' = descopeNormExpr f body
    Pi p binder' body'
  VLam p binder env body -> do
    let binder' = descopeNormBinder f binder
    let env' = fmap (descopeNormExpr f) env
    let body' = descopeNaive body
    let envExpr = App p (Var p "ENV") [ExplicitArg p $ LVec p env']
    Lam p binder' (App p envExpr [ExplicitArg p body'])

descopeSpine ::
  (Provenance -> DBLevel -> Name) ->
  Spine ->
  [InputArg]
descopeSpine f = fmap (fmap (descopeNormExpr f))

descopeNormBinder ::
  (Provenance -> DBLevel -> Name) ->
  NormBinder ->
  InputBinder
descopeNormBinder f = fmap (descopeNormExpr f)

descopeDBIndexVar :: MonadDescope m => Provenance -> DBIndexVar -> m Name
descopeDBIndexVar _ (Free ident) = return $ nameOf ident
descopeDBIndexVar p (Bound i) = do
  Ctx ctx <- ask
  case lookupVar ctx i of
    Nothing -> indexOutOfBounds p i (length ctx)
    Just Nothing -> return "_" -- usingUnnamedBoundVariable p i
    Just (Just name) -> return name

descopeDBIndexVarNaive :: MonadDescope m => Provenance -> DBIndexVar -> m Name
descopeDBIndexVarNaive _ = \case
  Free i -> return $ nameOf i
  Bound i -> return $ layoutAsText (pretty i)

descopeDBLevelVarNaive :: Provenance -> DBLevel -> Name
descopeDBLevelVarNaive _ l = layoutAsText $ pretty l

{-
descopeCoDBVarNaive :: MonadDescope m => Provenance -> CoDBVar -> m Name
descopeCoDBVarNaive _ = \case
  CoDBFree i -> return $ nameOf i
  CoDBBound -> return "CoDBVar"
-}
--------------------------------------------------------------------------------
-- Logging and errors

showScopeEntry :: Show var => Expr DBBinding var -> Expr DBBinding var
showScopeEntry e =
  e

showScopeExit :: MonadDescope m => m InputExpr -> m InputExpr
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

{-
-- | Use of unnamed bound variable error using an arbitrary index.
usingUnnamedBoundVariable :: MonadDescope m => Provenance -> DBIndex -> m a
usingUnnamedBoundVariable p index =
  developerError $
    "During descoping found use of unnamed bound variable"
      <+> pretty index
      <+> parens (pretty p)
-}
