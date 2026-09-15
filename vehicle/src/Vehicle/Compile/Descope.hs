module Vehicle.Compile.Descope
  ( descopeDecl,
    descopeExpr,
    descopeExprInEmptyCtx,
    descopeExprNaively,
    genericDescopeExpr,
    descopeForcedValueNaively,
    descopeThunkNaively,
    ixToName,
  )
where

import Data.Map.Ordered qualified as OMap
import Vehicle.Compile.Prelude
import Vehicle.Data.AST.Expr.Desugared qualified as S
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Builtin.Standard.Core (Builtin)
import Vehicle.Data.Code.ForcedValue (GenericForcedValue, GenericThunk)
import Vehicle.Data.Code.ForcedValue qualified as F
import Vehicle.Data.Universe (UniverseLevel)
import Vehicle.Data.Variable.Bound.Context.Name.Class
import Vehicle.Data.Variable.Bound.Context.Name.Core
import Vehicle.Data.Variable.Bound.Context.Name.Instance

--------------------------------------------------------------------------------
-- Interface

descopeDecl :: (PrintableBuiltin builtin) => Decl builtin -> S.Decl Builtin
descopeDecl decl = do
  let builtinDecl = fmap convertExprBuiltins decl
  case builtinDecl of
    DefFunction p ident sort t e -> DefFunction p ident sort (descopeExprInEmptyCtx t) (descopeExprInEmptyCtx e)
    DefAbstract p ident sort t -> DefAbstract p ident sort (descopeExprInEmptyCtx t)
    DefRecord p ident sort t f s -> do
      let (t', f') = descopeRecordTelescope t f
      DefRecord p ident sort t' f' s

descopeRecordTelescope ::
  (PrintableBuiltin Builtin) =>
  Telescope Builtin ->
  RecordFields Builtin ->
  (S.Telescope Builtin, S.RecordFields Builtin)
descopeRecordTelescope telescope fields =
  runFreshNameBoundContext (go telescope)
  where
    go :: (MonadNameContext m) => Telescope Builtin -> m (S.Telescope Builtin, S.RecordFields Builtin)
    go = \case
      [] -> do
        fields' <- traverseRecordFields (genericDescopeExpr (ixToName Named)) fields
        return ([], fields')
      binder : binders -> do
        binder' <- traverse (genericDescopeExpr (ixToName Named)) binder
        (binders', fields') <- addNameToContext binder $ go binders
        return (binder' : binders', fields')

descopeExpr :: (PrintableBuiltin builtin) => NamedBoundCtx -> Expr builtin -> S.Expr Builtin
descopeExpr ctx e =
  runNameBoundContext ctx $
    genericDescopeExpr (ixToName Named) (convertExprBuiltins e)

descopeExprInEmptyCtx :: (PrintableBuiltin builtin) => Expr builtin -> S.Expr Builtin
descopeExprInEmptyCtx = descopeExpr mempty

descopeExprNaively :: (PrintableBuiltin builtin) => Expr builtin -> S.Expr Builtin
descopeExprNaively e = do
  let se = convertExprBuiltins e
  runFreshNameBoundContext (genericDescopeExpr (ixToName Naive) se)

-- | Note that you cannot descope `Value` non-naively as you can't descope
-- closures properly. You have to unnormalise the `Value` first.
descopeForcedValueNaively ::
  (PrintableBuiltin builtin, Pretty meta) =>
  GenericForcedValue meta builtin ->
  S.Expr Builtin
descopeForcedValueNaively e = runFreshNameBoundContext (genericDescopeForcedValue Naive e)

descopeThunkNaively ::
  (PrintableBuiltin builtin, Pretty meta) =>
  GenericThunk meta builtin ->
  S.Expr Builtin
descopeThunkNaively e = runFreshNameBoundContext (descopeThunk Naive e)

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

genericDescopeExpr :: (MonadNameContext m) => VarConversion Ix m -> Expr Builtin -> m (S.Expr Builtin)
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
  Record p _recordType fields -> do
    fields' <- traverseRecordFields (genericDescopeExpr f) fields
    return $ S.Record p fields'
  RecordProj p _recordType record field -> do
    record' <- genericDescopeExpr f record
    return $ S.RecordAcc p record' field

descopeUniverse :: Provenance -> UniverseLevel -> S.Expr Builtin
descopeUniverse p _u = S.Universe p

descopeMeta :: (Pretty meta) => Provenance -> meta -> S.Expr Builtin
descopeMeta p m = S.Hole p (layoutAsText $ pretty m)

descopeFreeVar :: Provenance -> Identifier -> S.Expr Builtin
descopeFreeVar p ident = S.Var p (nameOf ident)

--------------------------------------------------------------------------------
-- Value

descopeThunk ::
  forall m meta builtin.
  (PrintableBuiltin builtin, Pretty meta, MonadNameContext m) =>
  VarStrategy ->
  GenericThunk meta builtin ->
  m (S.Expr Builtin)
descopeThunk f = \case
  F.Forced value -> genericDescopeForcedValue f value
  F.Unforced env value -> do
    body' <- genericDescopeExpr (ixToName f) $ convertExprBuiltins value
    env' <- traverse (genericDescopeForcedValue f) (F.cheatEnvToValues env) :: m [S.Expr Builtin]
    let envExpr = S.normAppList (S.Var mempty "ENV") $ fmap (Arg Explicit Relevant) env'
    return $ S.App envExpr [explicit body']

descopeClosure ::
  forall m binder meta builtin.
  (PrintableBuiltin builtin, Pretty meta, MonadNameContext m) =>
  VarStrategy ->
  GenericBinder binder ->
  F.GenericClosure meta builtin ->
  m (S.Expr Builtin)
descopeClosure f _binder (F.Closure env body) = do
  body' <- genericDescopeExpr (ixToName f) $ convertExprBuiltins body
  env' <- traverse (genericDescopeForcedValue f) (F.cheatEnvToValues env) :: m [S.Expr Builtin]
  let envExpr = S.normAppList (S.Var mempty "ENV") $ fmap (Arg Explicit Relevant) env'
  return $ S.App envExpr [explicit body']

-- | This function is not meant to do anything sensible and is merely
-- used for printing `WHNF`s in a readable form.
genericDescopeForcedValue ::
  (MonadNameContext m, Pretty meta, PrintableBuiltin builtin) =>
  VarStrategy ->
  GenericForcedValue meta builtin ->
  m (S.Expr Builtin)
genericDescopeForcedValue f e = case e of
  F.VUniverse u ->
    return $ descopeUniverse p u
  F.VMeta m spine ->
    S.normAppList (descopeMeta p m) <$> traverseArgs (descopeThunk f) spine
  F.VFreeVar v spine ->
    S.normAppList (descopeFreeVar p v) <$> traverseArgs (descopeThunk f) spine
  F.VBuiltin b spine -> do
    fn <- genericDescopeExpr (ixToName f) $ convertBuiltin p b
    S.normAppList fn <$> traverseArgs (descopeThunk f) spine
  F.VBoundVar v spine -> do
    var <- S.Var p <$> lvToName f p v
    args <- traverseArgs (descopeThunk f) spine
    return $ S.normAppList var args
  F.VPi binder closure -> do
    binder' <- traverse (descopeThunk f) binder
    body' <- addNameToContext binder $ descopeClosure f binder closure
    return $ S.Pi p binder' body'
  F.VLam binder closure -> do
    binder' <- traverse (descopeThunk f) binder
    body' <- addNameToContext binder $ descopeClosure f binder closure
    return $ S.Lam p binder' body'
  F.VRecord _recordType fields -> do
    fields' <- traverseRecordFields (descopeThunk f) $ OMap.assocs fields
    return $ S.Record p fields'
  F.VRecordAcc _recordType record field spine -> do
    record' <- descopeThunk f record
    let recordAcc = S.RecordAcc p record' field
    args <- traverseArgs (descopeThunk f) spine
    return $ S.normAppList recordAcc args
  where
    p = mempty

--------------------------------------------------------------------------------
-- Logging and errors

showDescopeEntry :: Expr Builtin -> Expr Builtin
showDescopeEntry e = e

showDescopeExit :: (Monad m) => m (S.Expr Builtin) -> m (S.Expr Builtin)
showDescopeExit m = m

{-
showDescopeEntry :: Expr Builtin -> Expr Builtin
showDescopeEntry e = trace ("enter: " <> show e) e

showDescopeExit :: (Monad m) => m S.Expr -> m S.Expr
showDescopeExit m = do
  e <- m
  return $ trace ("exit: " <> show e) e
-}
