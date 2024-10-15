module Vehicle.Compile.Descope
  ( descopeExpr,
    descopeExprInEmptyCtx,
    descopeExprNaively,
    descopeValueNaively,
    genericDescopeExpr,
    ixToName,
    ixToProperName,
    lvToProperName,
  )
where

import Vehicle.Compile.Context.Name
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print.Builtin
import Vehicle.Data.Builtin.Standard (Builtin)
import Vehicle.Data.Code.Value
import Vehicle.Data.Universe (UniverseLevel)
import Vehicle.Syntax.AST.Expr qualified as S

--------------------------------------------------------------------------------
-- Interface

descopeExpr :: (PrintableBuiltin builtin) => Contextualised (Expr builtin) NamedBoundCtx -> S.Expr
descopeExpr (WithContext e ctx) = do
  let binderCtx = fmap (mkExplicitBinder ()) ctx
  let fun = genericDescopeExpr (ixToName Named) (convertExprBuiltins e)
  runNameContext binderCtx fun

descopeExprInEmptyCtx :: (PrintableBuiltin builtin) => Expr builtin -> S.Expr
descopeExprInEmptyCtx e = descopeExpr (WithContext e mempty)

descopeExprNaively :: (PrintableBuiltin builtin) => Expr builtin -> S.Expr
descopeExprNaively e = do
  let se = convertExprBuiltins e
  runFreshNameContext (genericDescopeExpr (ixToName Naive) se)

-- | Note that you cannot descope `Value` non-naively as you can't descope
-- closures properly. You have to quote the `Value` first.
descopeValueNaively ::
  (PrintableBuiltin builtin) =>
  Value builtin ->
  S.Expr
descopeValueNaively e = runFreshNameContext (genericDescopeValue Naive e)

--------------------------------------------------------------------------------
-- Variable conversion methods

type VarConversion var m = (MonadNameContext m) => Provenance -> var -> m Name

data VarStrategy = Named | Naive

ixToName :: VarStrategy -> VarConversion Ix m
ixToName s p ix = case s of
  Naive -> return $ layoutAsText $ pretty ix
  Named -> ixToProperName p ix

lvToName :: VarStrategy -> VarConversion Lv m
lvToName s p lv = case s of
  Naive -> return $ layoutAsText $ pretty lv
  Named -> lvToProperName p lv

--------------------------------------------------------------------------------
-- Expr

genericDescopeExpr :: (MonadNameContext m) => VarConversion Ix m -> Expr Builtin -> m S.Expr
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
    body' <- addNameToContext binder $ genericDescopeExpr f body
    return $ S.Let p bound' binder' body'
  Lam p binder body -> do
    binder' <- traverse (genericDescopeExpr f) binder
    body' <- addNameToContext binder $ genericDescopeExpr f body
    return $ S.Lam p binder' body'
  Pi p binder body -> do
    binder' <- traverse (genericDescopeExpr f) binder
    body' <- addNameToContext binder $ genericDescopeExpr f body
    return $ S.Pi p binder' body'

--------------------------------------------------------------------------------
-- Value

descopeClosure :: forall m binder builtin. (PrintableBuiltin builtin, MonadNameContext m) => VarStrategy -> GenericBinder binder -> Closure builtin -> m S.Expr
descopeClosure f _binder (Closure env body) = do
  body' <- genericDescopeExpr (ixToName f) $ convertExprBuiltins body
  env' <- traverse (genericDescopeValue f) (cheatEnvToValues env) :: m [S.Expr]
  let envExpr = S.normAppList (S.Var mempty "ENV") $ fmap (Arg mempty Explicit Relevant) env'
  return $ S.App envExpr [explicit body']

-- | This function is not meant to do anything sensible and is merely
-- used for printing `WHNF`s in a readable form.
genericDescopeValue ::
  (MonadNameContext m, PrintableBuiltin builtin) =>
  VarStrategy ->
  Value builtin ->
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
    body' <- addNameToContext binder $ genericDescopeValue f body
    return $ S.Pi p binder' body'
  VLam binder closure -> do
    binder' <- traverse (genericDescopeValue f) binder
    body' <- addNameToContext binder $ descopeClosure f binder closure
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
