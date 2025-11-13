module Vehicle.Compile.Scope.Undo
  ( descopeExpr,
    descopeExprNaively,
    descopeValueNaively,
    genericDescopeExpr,
    ixToName,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Ordered qualified as OMap
import Vehicle.Compile.Prelude
import Vehicle.Compile.Scope.Coercions (removeCoercions)
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Builtin.Standard.Core (Builtin)
import Vehicle.Data.Code.Value
import Vehicle.Data.Universe (UniverseLevel)
import Vehicle.Data.Variable.Bound.Context.Name.Class
import Vehicle.Data.Variable.Bound.Context.Name.Core
import Vehicle.Data.Variable.Bound.Context.Name.Instance
import Vehicle.Syntax.AST.Expr qualified as S

--------------------------------------------------------------------------------
-- Interface

-- | Should we undo automatic insertion of variables and coercions that were
-- inserted during scope checking?
type Clean = Bool

descopeExpr :: (PrintableBuiltin builtin) => NamedBoundCtx -> Clean -> Expr builtin -> S.Expr
descopeExpr ctx clean e =
  runNameBoundContext ctx $
    genericDescopeExpr clean (ixToName Named) (convertExprBuiltins e)

descopeExprNaively :: (PrintableBuiltin builtin) => Expr builtin -> S.Expr
descopeExprNaively e = do
  let se = convertExprBuiltins e
  runFreshNameBoundContext (genericDescopeExpr False (ixToName Naive) se)

-- | Note that you cannot descope `Value` non-naively as you can't descope
-- closures properly. You have to quote the `Value` first.
descopeValueNaively ::
  (PrintableBuiltin builtin) =>
  Value builtin ->
  S.Expr
descopeValueNaively e = runFreshNameBoundContext (genericDescopeValue Naive e)

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

genericDescopeExpr :: forall m. (MonadNameContext m) => Clean -> VarConversion Ix m -> Expr Builtin -> m S.Expr
genericDescopeExpr clean f = go
  where
    go :: Expr Builtin -> m S.Expr
    go e = showDescopeExit $ case showDescopeEntry e of
      (coercionRemoval clean -> Just result) -> go result
      Universe p l -> return $ descopeUniverse p l
      Hole p name -> return $ S.Hole p name
      Builtin p op -> return $ S.Builtin p op
      Meta p i -> return $ descopeMeta p i
      FreeVar p v -> return $ descopeFreeVar p v
      BoundVar p v -> S.Var p <$> f p v
      App fun args -> do
        fun' <- go fun
        args' <- genericDescopeArgs clean f args
        return $ S.normAppList fun' args'
      Let p bound binder body -> do
        bound' <- go bound
        binder' <- traverse go binder
        body' <- addNameToContext binder $ go body
        return $ S.Let p bound' binder' body'
      Lam p binder body -> do
        binder' <- traverse go binder
        body' <- addNameToContext binder $ go body
        return $ S.Lam p binder' body'
      Pi p binder body -> do
        binder' <- traverse go binder
        body' <- addNameToContext binder $ go body
        return $ S.Pi p binder' body'
      Record p _ fields -> do
        fields' <- traverseRecordFields go fields
        return $ S.Record p fields'
      RecordAcc p record (_, field) -> do
        record' <- go record
        return $ S.RecordAcc p record' field

genericDescopeArgs :: (MonadNameContext m) => Clean -> VarConversion Ix m -> NonEmpty (Arg Builtin) -> m [S.Arg]
genericDescopeArgs clean f args = do
  let filteredArgs =
        if not clean
          then NonEmpty.toList args
          else do
            let userArgs = filter (not . wasInsertedByCompiler) $ NonEmpty.toList args
            fmap (setRelevance Relevant) userArgs

  traverseArgs (genericDescopeExpr clean f) filteredArgs

coercionRemoval :: Clean -> Expr Builtin -> Maybe (Expr Builtin)
coercionRemoval clean e =
  if not clean
    then Nothing
    else case getBuiltinApp e of
      Just (builtin, args) -> removeCoercions mempty builtin args
      Nothing -> Nothing

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
  body' <- genericDescopeExpr False (ixToName f) $ convertExprBuiltins body
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
    fn <- genericDescopeExpr False (ixToName f) $ convertBuiltin p b
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

{-

instance Simplify Expr where
  clean = mapApp $ \fun args -> do
    let fun' = clean fun
    -- Remove automatically inserted cast functions
    removeInsertedCasts fun' args

  shortenVec = mapApp $ \fun args ->
    case (fun, args) of
      (Builtin p (BuiltinFunction StackTensor), (argExpr -> (Builtin _ (BuiltinConstructor (NatLiteral n)))) :| _) ->
        case getHeadMidTail (drop (length args - n) $ NonEmpty.toList args) of
          Just (firstArg, numberOfMiddleArgs, lastArg)
            | numberOfMiddleArgs > 3 ->
                normAppList
                  fun
                  [ firstArg,
                    Arg p Explicit Relevant (Var p ("<" <> n2 <> " more>")),
                    lastArg
                  ]
            where
              n2 = Text.pack $ show numberOfMiddleArgs
          _ -> App fun args
      _ -> App fun args
    where
      getHeadMidTail :: forall a. [a] -> Maybe (a, Int, a)
      getHeadMidTail [] = Nothing
      getHeadMidTail (x : xs) = go 0 xs
        where
          go :: Int -> [a] -> Maybe (a, Int, a)
          go _ [] = Nothing
          go l [e] = Just (x, l, e)
          go l (_ : ys) = go (l + 1) ys

instance Simplify Binder where
  clean = fmap clean . setRelevance Relevant
  shortenVec = fmap shortenVec

instance Simplify Arg where
  clean = fmap clean . setRelevance Relevant
  shortenVec = fmap shortenVec

-}
