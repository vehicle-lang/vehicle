module Vehicle.Compile.Scope
  ( scopeCheck,
    scopeCheckClosedExpr,
  )
where

import Control.Monad (foldM, when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), asks, runReaderT)
import Control.Monad.Writer (MonadWriter (..), execWriterT)
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (traverse_)
import Data.List (elemIndex)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Universe (UniverseLevel (..))
import Vehicle.Syntax.AST.Expr qualified as S

scopeCheck :: (MonadCompile m) => Imports -> S.Prog -> m (Prog Builtin)
scopeCheck imports e = logCompilerPass MinDetail "scope checking" $ do
  let importCtx = getImportCtx imports
  prog <- runReaderT (scopeProg e) importCtx
  return prog

scopeCheckClosedExpr :: (MonadCompile m) => S.Expr -> m (Expr Builtin)
scopeCheckClosedExpr e = runReaderT (scopeExpr e) (mempty, mempty)

--------------------------------------------------------------------------------
-- Scope checking monad and context

type MonadScope m =
  ( MonadCompile m,
    MonadReader DeclScopeCtx m
  )

--------------------------------------------------------------------------------
-- Algorithm

scopeProg :: (MonadScope m) => S.Prog -> m (Prog Builtin)
scopeProg (Main ds) = Main <$> scopeDecls ds

scopeDecls :: (MonadScope m) => [S.Decl] -> m [Decl Builtin]
scopeDecls = \case
  [] -> return []
  (d : ds) -> do
    let ident = identifierOf d
    let identName = nameOf ident
    d' <- scopeDecl d

    existingEntry <- asks (Map.lookup identName)
    case existingEntry of
      Just existingIdent -> throwError $ DeclarationDeclarationShadowing (provenanceOf d) identName existingIdent
      Nothing -> do
        ds' <- bindDecl ident (scopeDecls ds)
        return (d' : ds')

scopeDecl :: (MonadScope m) => S.Decl -> m (Decl Builtin)
scopeDecl decl = logCompilerPass MidDetail ("scoping" <+> quotePretty (identifierOf decl)) $ do
  result <- case decl of
    DefAbstract p ident r t ->
      DefAbstract p ident r <$> scopeDeclExpr False t
    DefFunction p ident anns t e ->
      DefFunction p ident anns <$> scopeDeclExpr True t <*> scopeDeclExpr False e
  logCompilerPassOutput (prettyFriendly result)
  return result

scopeDeclExpr :: (MonadScope m) => Bool -> S.Expr -> m (Expr Builtin)
scopeDeclExpr generalise expr = do
  freeCtx <- ask

  exprToScope <-
    if generalise
      then generaliseExpr freeCtx expr
      else return expr

  runReaderT (scopeExpr exprToScope) (freeCtx, mempty)

bindDecl :: (MonadScope m) => Identifier -> m a -> m a
bindDecl ident = local (Map.insert (nameOf ident) ident)

--------------------------------------------------------------------------------
-- Expr generalisation

type GeneralisableVariable = (Provenance, Name)

generaliseExpr :: (MonadCompile m) => DeclScopeCtx -> S.Expr -> m S.Expr
generaliseExpr declContext expr = do
  candidates <- findGeneralisableVariables declContext expr
  generaliseOverVariables (reverse candidates) expr

findGeneralisableVariables ::
  (MonadCompile m) =>
  DeclScopeCtx ->
  S.Expr ->
  m [GeneralisableVariable]
findGeneralisableVariables declContext expr =
  execWriterT (runReaderT (findVariables expr) (declContext, mempty))
  where
    findVariables :: (MonadTraverse m, MonadWriter [GeneralisableVariable] m) => S.Expr -> m ()
    findVariables = \case
      S.Var p v -> registerVar p v
      S.Universe {} -> return ()
      S.Hole {} -> return ()
      S.Builtin {} -> return ()
      S.App fun args -> do
        findVariables fun
        traverse_ (traverse_ findVariables) args
      S.Pi _ binder res ->
        findVariablesBinder binder $ findVariables res
      S.Lam _ binder body -> do
        findVariablesBinder binder $ findVariables body
      S.Let _ bound binder body -> do
        findVariables bound
        findVariablesBinder binder $ findVariables body

    findVariablesBinder :: (MonadTraverse m, MonadWriter [GeneralisableVariable] m) => S.Binder -> m () -> m ()
    findVariablesBinder binder update = do
      traverse_ findVariables binder
      local (second (nameOf binder :)) update

    registerVar :: (MonadTraverse m, MonadWriter [GeneralisableVariable] m) => Provenance -> Name -> m ()
    registerVar p symbol = do
      (freeCtx, boundCtx) <- ask
      when (Map.notMember symbol freeCtx && notElem (Just symbol) boundCtx) $ do
        tell [(p, symbol)]

generaliseOverVariables ::
  (MonadCompile m) =>
  [GeneralisableVariable] ->
  S.Expr ->
  m S.Expr
generaliseOverVariables vars e = fst <$> foldM generalise (e, mempty) vars
  where
    generalise ::
      (MonadCompile m) =>
      (S.Expr, Set Name) ->
      GeneralisableVariable ->
      m (S.Expr, Set Name)
    generalise (expr, seenNames) (p, name)
      | name `Set.member` seenNames = return (expr, seenNames)
      | otherwise = do
          logDebug MaxDetail $
            "Generalising over unbound variable" <+> quotePretty name
          let binderType = S.mkHole p ("typeOf[" <> name <> "]")
          let binderDisplayForm = BinderDisplayForm (OnlyName name) True
          let binder = Binder p binderDisplayForm (Implicit True) Relevant binderType
          let newExpr = S.Pi p binder expr
          return (newExpr, Set.insert name seenNames)

--------------------------------------------------------------------------------
-- Expr scoping

type MonadScopeExpr m =
  ( MonadCompile m,
    MonadReader (DeclScopeCtx, LocalScopeCtx) m
  )

scopeExpr :: (MonadScopeExpr m) => S.Expr -> m (Expr Builtin)
scopeExpr = traverseExpr scopeVar scopeBinder

-- | Find the index for a given name of a given sort.
scopeVar :: (MonadScopeExpr m) => VarUpdate m
scopeVar p symbol = do
  (freeCtx, boundCtx) <- ask

  case elemIndex (Just symbol) boundCtx of
    Just i -> return $ BoundVar p $ Ix i
    Nothing -> case Map.lookup symbol freeCtx of
      Just ident -> return $ FreeVar p ident
      Nothing -> do
        throwError $ UnboundName p symbol

scopeBinder :: (MonadScopeExpr m) => S.Binder -> m ()
scopeBinder binder = case nameOf binder of
  Nothing -> return ()
  Just name -> do
    (freeCtx, _boundCtx) <- ask
    when (name `Map.member` freeCtx) $
      -- This restriction is needed so that
      -- `Vehicle.Compile.ResourceFunctionalisation`
      -- doesn't accidentally capture variables.
      throwError $
        DeclarationBoundShadowing (provenanceOf binder) name

--------------------------------------------------------------------------------
-- Utility functions

type MonadTraverse m =
  ( MonadCompile m,
    MonadReader (DeclScopeCtx, LocalScopeCtx) m
  )

type VarUpdate m =
  forall builtin. Provenance -> Name -> m (Expr builtin)

type BinderUpdate m = S.Binder -> m ()

type DeclScopeCtx = Map Name Identifier

type LocalScopeCtx = [Maybe Name]

traverseExpr ::
  (MonadTraverse m) =>
  VarUpdate m ->
  BinderUpdate m ->
  S.Expr ->
  m (Expr Builtin)
traverseExpr f g e = do
  result <- case e of
    S.Var p v -> f p v
    S.Universe p -> return $ Universe p (UniverseLevel 0)
    S.Hole p n -> return $ Hole p n
    S.Builtin p op -> return $ Builtin p op
    S.App fun args -> App <$> traverseExpr f g fun <*> traverse (traverse (traverseExpr f g)) args
    S.Pi p binder res ->
      traverseBinder f g binder $ \binder' ->
        Pi p binder' <$> traverseExpr f g res
    S.Lam p binder body -> do
      traverseBinder f g binder $ \binder' ->
        Lam p binder' <$> traverseExpr f g body
    S.Let p bound binder body -> do
      bound' <- traverseExpr f g bound
      traverseBinder f g binder $ \binder' ->
        Let p bound' binder' <$> traverseExpr f g body

  return result

traverseBinder ::
  (MonadTraverse m) =>
  VarUpdate m ->
  BinderUpdate m ->
  S.Binder ->
  (Binder Builtin -> m (Expr Builtin)) ->
  m (Expr Builtin)
traverseBinder f g binder update = do
  g binder
  binder' <- traverse (traverseExpr f g) binder
  let updateCtx ctx = nameOf binder : ctx
  local (second updateCtx) (update binder')

{-
logScopeEntry :: MonadTraverse m => Expr UnscopedBinding -> m ()
logScopeEntry e = do
  incrCallDepth
  logDebug MaxDetail $ "scope-entry" <+> prettyVerbose e -- <+> "in" <+> pretty ctx

logScopeExit :: MonadTraverse m => Expr -> m ()
logScopeExit e = do
  logDebug MaxDetail $ "scope-exit " <+> prettyVerbose e
  decrCallDepth
-}
getImportCtx :: Imports -> DeclScopeCtx
getImportCtx imports =
  Map.fromList $
    [getEntry d | imp <- imports, let Main ds = imp, d <- ds]
  where
    getEntry :: Decl builtin -> (Name, Identifier)
    getEntry d = do
      let ident = identifierOf d
      (nameOf ident, ident)
