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

import Data.Map.Ordered qualified as OMap
import Vehicle.Compile.Context.Name.Core
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Builtin.Standard.Core (Builtin)
import Vehicle.Data.Code.Value
import Vehicle.Data.Universe (UniverseLevel)
import Vehicle.Syntax.AST.Expr qualified as S

--------------------------------------------------------------------------------
-- Interface

descopeExpr :: (PrintableBuiltin builtin) => Expr builtin -> NamedBoundCtx -> S.Expr
descopeExpr e ctx =
  runNameContext ctx $
    genericDescopeExpr (ixToName Named) (convertExprBuiltins e)

descopeExprInEmptyCtx :: (PrintableBuiltin builtin) => Expr builtin -> S.Expr
descopeExprInEmptyCtx e = descopeExpr e mempty

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
genericDescopeExpr f e = showDescopeExit $ case showDescopeEntry e of
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
  Record p _ fields -> do
    fields' <- traverseRecordFields (genericDescopeExpr f) fields
    return $ S.Record p fields'
  RecordAcc p record (_, field) -> do
    record' <- genericDescopeExpr f record
    return $ S.RecordAcc p record' field

--------------------------------------------------------------------------------
-- Value

descopeClosure ::
  forall m binder builtin.
  (PrintableBuiltin builtin, MonadNameContext m) =>
  VarStrategy ->
  GenericBinder binder ->
  Closure builtin ->
  m S.Expr
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
  VPi binder closure -> do
    binder' <- traverse (genericDescopeValue f) binder
    body' <- addNameToContext binder $ descopeClosure f binder closure
    return $ S.Pi p binder' body'
  VLam binder closure -> do
    binder' <- traverse (genericDescopeValue f) binder
    body' <- addNameToContext binder $ descopeClosure f binder closure
    return $ S.Lam p binder' body'
  VRecord _ident fields -> do
    fields' <- traverseRecordFields (genericDescopeValue f) $ OMap.assocs fields
    return $ S.Record p fields'
  VRecordAcc record (_ident, field) -> do
    record' <- genericDescopeValue f record
    return $ S.RecordAcc p record' field
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

showDescopeEntry :: Expr Builtin -> Expr Builtin
showDescopeEntry e = e

showDescopeExit :: (Monad m) => m S.Expr -> m S.Expr
showDescopeExit m = m

{-
showDescopeEntry :: Expr Builtin -> Expr Builtin
showDescopeEntry e = trace ("enter: " <> show e) e

showDescopeExit :: (Monad m) => m S.Expr -> m S.Expr
showDescopeExit m = do
  e <- m
  return $ trace ("exit: " <> show e) e
-}
