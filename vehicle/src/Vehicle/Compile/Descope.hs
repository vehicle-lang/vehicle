module Vehicle.Compile.Descope
  ( DescopableClosure (..),
    MonadDescope,
    runMonadDescope,
    runMonadDescopeT,
    addBinderNameToContext,
    descopeExpr,
    descopeExprInEmptyCtx,
    descopeExprNaively,
    descopeValueNaively,
    genericDescopeExpr,
    ixToName,
    ixToProperName,
  )
where

import Control.Monad (void)
import Data.Proxy (Proxy (..))
import Vehicle.Backend.LossFunction.Core (LossClosure (..), MixedClosure (..))
import Vehicle.Compile.Context.Bound.Class (MonadBoundContext (..), getNamedBoundCtx)
import Vehicle.Compile.Context.Bound.Instance (BoundContext, BoundContextT, runBoundContext, runFreshBoundContext, runFreshBoundContextT)
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Standard.Core (Builtin)
import Vehicle.Data.Code.Value
import Vehicle.Data.Universe (UniverseLevel)
import Vehicle.Syntax.AST.Expr qualified as S

--------------------------------------------------------------------------------
-- Monad

type MonadDescope m = MonadBoundContext () m

runMonadDescope :: BoundContext () a -> a
runMonadDescope = runFreshBoundContext (Proxy @())

runMonadDescopeT :: (Monad m) => BoundContextT () m a -> m a
runMonadDescopeT = runFreshBoundContextT (Proxy @())

addBinderNameToContext :: (MonadDescope m) => GenericBinder expr -> m a -> m a
addBinderNameToContext binder = addBinderToContext (void binder)

--------------------------------------------------------------------------------
-- Interface

descopeExpr :: (PrintableBuiltin builtin) => Contextualised (Expr builtin) NamedBoundCtx -> S.Expr
descopeExpr (WithContext e ctx) = do
  let binderCtx = fmap (mkDefaultBinder ()) ctx
  let fun = genericDescopeExpr (ixToName Named) (convertExprBuiltins e)
  runBoundContext binderCtx fun

descopeExprInEmptyCtx :: (PrintableBuiltin builtin) => Expr builtin -> S.Expr
descopeExprInEmptyCtx e = descopeExpr (WithContext e mempty)

descopeExprNaively :: (PrintableBuiltin builtin) => Expr builtin -> S.Expr
descopeExprNaively e = do
  let se = convertExprBuiltins e
  runMonadDescope (genericDescopeExpr (ixToName Naive) se)

-- | Note that you cannot descope `Value` non-naively as you can't descope
-- closures properly. You have to quote the `Value` first.
descopeValueNaively ::
  (PrintableBuiltin builtin, DescopableClosure closure) =>
  Value closure builtin ->
  S.Expr
descopeValueNaively e = runMonadDescope (genericDescopeValue Naive e)

--------------------------------------------------------------------------------
-- Variable conversion methods

type VarConversion var m = (MonadDescope m) => Provenance -> var -> m Name

data VarStrategy = Named | Naive

ixToProperName :: VarConversion Ix m
ixToProperName p ix = do
  ctx <- getNamedBoundCtx (Proxy @())
  case lookupIx ctx ix of
    Nothing -> varOutOfBounds "DeBruijn index" p ix (length ctx)
    Just Nothing -> return "_"
    Just (Just name) -> return name

ixToName :: VarStrategy -> VarConversion Ix m
ixToName s p ix = case s of
  Naive -> return $ layoutAsText $ pretty ix
  Named -> ixToProperName p ix

lvToName :: VarStrategy -> VarConversion Lv m
lvToName s p lv = case s of
  Naive -> return $ layoutAsText $ pretty lv
  Named -> do
    ctx <- getNamedBoundCtx (Proxy @())
    case lookupLv ctx lv of
      Nothing -> varOutOfBounds "DeBruijn level" p lv (length ctx)
      Just Nothing -> return "_"
      Just (Just name) -> return name

--------------------------------------------------------------------------------
-- Expr

genericDescopeExpr :: (MonadDescope m) => VarConversion Ix m -> Expr Builtin -> m S.Expr
genericDescopeExpr f e = showScopeExit $ case showScopeEntry e of
  Universe p l -> return $ descopeUniverse p l
  Hole p name -> return $ S.Hole p name
  Builtin p op -> return $ S.Builtin p op
  Meta p i -> return $ descopeMeta p i
  FreeVar p v -> return $ descopeFreeVar p v
  BoundVar p v -> S.Var p <$> f p v
  App fun args -> do
    fun' <- genericDescopeExpr f fun
    args' <- traverse (traverse (genericDescopeExpr f)) args
    return $ S.App fun' args'
  Let p bound binder body -> do
    bound' <- genericDescopeExpr f bound
    binder' <- traverse (genericDescopeExpr f) binder
    body' <- addBinderToContext (void binder) (genericDescopeExpr f body)
    return $ S.Let p bound' binder' body'
  Lam p binder body -> do
    binder' <- traverse (genericDescopeExpr f) binder
    body' <- addBinderToContext (void binder) (genericDescopeExpr f body)
    return $ S.Lam p binder' body'
  Pi p binder body -> do
    binder' <- traverse (genericDescopeExpr f) binder
    body' <- addBinderToContext (void binder) (genericDescopeExpr f body)
    return $ S.Pi p binder' body'

--------------------------------------------------------------------------------
-- Value

class DescopableClosure closure where
  descopeClosure :: (MonadDescope m) => VarStrategy -> GenericBinder expr -> closure -> m S.Expr

instance (PrintableBuiltin builtin) => DescopableClosure (WHNFClosure builtin) where
  descopeClosure :: forall m binder. (MonadDescope m) => VarStrategy -> GenericBinder binder -> WHNFClosure builtin -> m S.Expr
  descopeClosure f _binder (WHNFClosure env body) = do
    body' <- genericDescopeExpr (ixToName f) $ convertExprBuiltins body
    env' <- traverse (genericDescopeValue f) (cheatEnvToValues env) :: m [S.Expr]
    let envExpr = S.normAppList (S.Var mempty "ENV") $ fmap (Arg mempty Explicit Relevant) env'
    return $ S.App envExpr [explicit body']

-- genericDescopeExpr (ixToName f) $ convertExprBuiltins body

instance (PrintableBuiltin builtin) => DescopableClosure (NFClosure builtin) where
  descopeClosure f _binder (NFClosure body) = genericDescopeValue f body

instance DescopableClosure MixedClosure where
  descopeClosure :: forall m binder. (MonadDescope m) => VarStrategy -> GenericBinder binder -> MixedClosure -> m S.Expr
  descopeClosure f binder = \case
    StandardClos closure -> descopeClosure f binder closure
    LossClos (LossClosure env body) -> do
      body' <- genericDescopeExpr (ixToName f) $ convertExprBuiltins body
      env' <- traverse (genericDescopeValue f) (cheatEnvToValues env) :: m [S.Expr]
      let envExpr = S.normAppList (S.Var mempty "ENV") $ fmap (Arg mempty Explicit Relevant) env'
      return $ S.App envExpr [explicit body']

-- | This function is not meant to do anything sensible and is merely
-- used for printing `WHNF`s in a readable form.
genericDescopeValue ::
  (MonadDescope m, DescopableClosure closure, PrintableBuiltin builtin) =>
  VarStrategy ->
  Value closure builtin ->
  m S.Expr
genericDescopeValue f e = case e of
  VUniverse u ->
    return $ descopeUniverse p u
  VMeta m spine ->
    S.normAppList (descopeMeta p m) <$> traverseArgs (genericDescopeValue f) spine
  VFreeVar v spine ->
    S.normAppList (descopeFreeVar p v) <$> traverseArgs (genericDescopeValue f) spine
  VBuiltin b spine -> do
    fn <- genericDescopeExpr (ixToName f) $ convertBuiltin p b
    S.normAppList fn <$> traverseArgs (genericDescopeValue f) spine
  VBoundVar v spine -> do
    var <- S.Var p <$> lvToName f p v
    args <- traverseArgs (genericDescopeValue f) spine
    return $ S.normAppList var args
  VPi binder body -> do
    binder' <- traverse (genericDescopeValue f) binder
    body' <- addBinderToContext (void binder) $ genericDescopeValue f body
    return $ S.Pi p binder' body'
  VLam binder closure -> do
    binder' <- traverse (genericDescopeValue f) binder
    body' <- addBinderToContext (void binder) $ descopeClosure f binder closure
    return $ S.Lam p binder' body'
  where
    p = mempty

descopeUniverse :: Provenance -> UniverseLevel -> S.Expr
descopeUniverse p _u = S.Universe p

descopeMeta :: Provenance -> MetaID -> S.Expr
descopeMeta p m = S.Hole p (layoutAsText $ pretty m)

descopeFreeVar :: Provenance -> Identifier -> S.Expr
descopeFreeVar p ident = S.Var p (nameOf ident)

--------------------------------------------------------------------------------
-- Logging and errors

showScopeEntry :: Expr builtin -> Expr builtin
showScopeEntry e = e

showScopeExit :: (Monad m) => m S.Expr -> m S.Expr
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
