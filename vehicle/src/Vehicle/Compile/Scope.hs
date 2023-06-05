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
import Data.List (elemIndex)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Expr.DeBruijn

scopeCheck ::
  (MonadCompile m) =>
  ImportedModules ->
  Prog Name Builtin ->
  m (Prog Ix Builtin)
scopeCheck imports e = logCompilerPass MinDetail "scope checking" $ do
  let importCtx = getImportCtx imports
  prog <- runReaderT (scopeProg e) importCtx
  return prog

scopeCheckClosedExpr :: (MonadCompile m) => Expr Name Builtin -> m (Expr Ix Builtin)
scopeCheckClosedExpr e = runReaderT (scopeExpr e) (mempty, mempty)

--------------------------------------------------------------------------------
-- Scope checking monad and context

type DeclScopeCtx = Map Name Identifier

type LocalScopeCtx = [Maybe Name]

type MonadScope m =
  ( MonadCompile m,
    MonadReader DeclScopeCtx m
  )

--------------------------------------------------------------------------------
-- Algorithm

scopeProg :: (MonadScope m) => Prog Name Builtin -> m (Prog Ix Builtin)
scopeProg (Main ds) = Main <$> scopeDecls ds

scopeDecls :: (MonadScope m) => [Decl Name Builtin] -> m [Decl Ix Builtin]
scopeDecls = \case
  [] -> return []
  (d : ds) -> do
    let ident = identifierOf d
    let identName = nameOf ident
    d' <- scopeDecl d

    existingEntry <- asks (Map.lookup identName)
    case existingEntry of
      Just existingIdent -> throwError $ DuplicateName (provenanceOf d) identName existingIdent
      Nothing -> do
        ds' <- bindDecl ident (scopeDecls ds)
        return (d' : ds')

scopeDecl :: (MonadScope m) => Decl Name Builtin -> m (Decl Ix Builtin)
scopeDecl decl = logCompilerPass MidDetail ("scoping" <+> quotePretty (identifierOf decl)) $ do
  result <- case decl of
    DefAbstract p ident r t ->
      DefAbstract p ident r <$> scopeDeclExpr False t
    DefFunction p ident anns t e ->
      DefFunction p ident anns <$> scopeDeclExpr True t <*> scopeDeclExpr False e
  logCompilerPassOutput (prettyFriendly result)
  return result

scopeDeclExpr ::
  (MonadScope m) =>
  Bool ->
  Expr Name Builtin ->
  m (Expr Ix Builtin)
scopeDeclExpr generalise expr = do
  declCtx <- ask

  exprToScope <-
    if generalise
      then generaliseExpr declCtx expr
      else return expr

  runReaderT (scopeExpr exprToScope) (declCtx, mempty)

bindDecl :: (MonadScope m) => Identifier -> m a -> m a
bindDecl ident = local (Map.insert (nameOf ident) ident)

--------------------------------------------------------------------------------
-- Expr generalisation

type GeneralisableVariable = (Provenance, Name)

generaliseExpr :: (MonadCompile m) => DeclScopeCtx -> Expr Name Builtin -> m (Expr Name Builtin)
generaliseExpr declContext expr = do
  candidates <- findGeneralisableVariables declContext expr
  generaliseOverVariables (reverse candidates) expr

findGeneralisableVariables ::
  (MonadCompile m) =>
  DeclScopeCtx ->
  Expr Name Builtin ->
  m [GeneralisableVariable]
findGeneralisableVariables declContext expr =
  execWriterT (runReaderT (traverseVars traverseVar expr) (declContext, mempty))
  where
    traverseVar ::
      (MonadTraverse m, MonadWriter [GeneralisableVariable] m) =>
      VarUpdate m Name Name
    traverseVar p symbol = do
      (declCtx, boundCtx) <- ask

      when (Map.notMember symbol declCtx && notElem (Just symbol) boundCtx) $ do
        tell [(p, symbol)]

      return $ BoundVar p symbol

generaliseOverVariables ::
  (MonadCompile m) =>
  [GeneralisableVariable] ->
  Expr Name Builtin ->
  m (Expr Name Builtin)
generaliseOverVariables vars e = fst <$> foldM generalise (e, mempty) vars
  where
    generalise ::
      (MonadCompile m) =>
      (Expr Name Builtin, Set Name) ->
      GeneralisableVariable ->
      m (Expr Name Builtin, Set Name)
    generalise (expr, seenNames) (p, name)
      | name `Set.member` seenNames = return (expr, seenNames)
      | otherwise = do
          logDebug MaxDetail $
            "Generalising over unbound variable" <+> quotePretty name
          let binderType = mkHole p ("typeOf[" <> name <> "]")
          let binderDisplayForm = BinderDisplayForm (OnlyName name) True
          let binder = Binder p binderDisplayForm (Implicit True) Relevant binderType
          let newExpr = Pi p binder expr
          return (newExpr, Set.insert name seenNames)

--------------------------------------------------------------------------------
-- Expr scoping

type MonadScopeExpr m =
  ( MonadCompile m,
    MonadReader (DeclScopeCtx, LocalScopeCtx) m
  )

scopeExpr :: (MonadScopeExpr m) => Expr Name Builtin -> m (Expr Ix Builtin)
scopeExpr = traverseVars scopeVar

-- | Find the index for a given name of a given sort.
scopeVar :: (MonadScopeExpr m) => VarUpdate m Name Ix
scopeVar p symbol = do
  (declCtx, boundCtx) <- ask

  case elemIndex (Just symbol) boundCtx of
    Just i -> return $ BoundVar p $ Ix i
    Nothing -> case Map.lookup symbol declCtx of
      Just ident -> return $ FreeVar p ident
      Nothing -> do
        throwError $ UnboundName p symbol

--------------------------------------------------------------------------------
-- Utility functions

type MonadTraverse m =
  ( MonadCompile m,
    MonadReader (DeclScopeCtx, LocalScopeCtx) m
  )

type VarUpdate m var1 var2 =
  forall builtin. Provenance -> var1 -> m (Expr var2 builtin)

traverseVars ::
  (MonadTraverse m) =>
  VarUpdate m var1 var2 ->
  Expr var1 builtin ->
  m (Expr var2 builtin)
traverseVars f e = do
  result <- case e of
    BoundVar p v -> f p v
    FreeVar p v -> return $ FreeVar p v
    Universe p l -> return $ Universe p l
    Meta p i -> return $ Meta p i
    Hole p n -> return $ Hole p n
    Builtin p op -> return $ Builtin p op
    Ann p ex t -> Ann p <$> traverseVars f ex <*> traverseVars f t
    App p fun args -> App p <$> traverseVars f fun <*> traverse (traverse (traverseVars f)) args
    Pi p binder res ->
      traverseBinder f binder $ \binder' ->
        Pi p binder' <$> traverseVars f res
    Lam p binder body -> do
      traverseBinder f binder $ \binder' ->
        Lam p binder' <$> traverseVars f body
    Let p bound binder body -> do
      bound' <- traverseVars f bound
      traverseBinder f binder $ \binder' ->
        Let p bound' binder' <$> traverseVars f body

  return result

traverseBinder ::
  (MonadTraverse m) =>
  VarUpdate m var1 var2 ->
  Binder var1 builtin ->
  (Binder var2 builtin -> m (Expr var2 builtin)) ->
  m (Expr var2 builtin)
traverseBinder f binder update = do
  binder' <- traverse (traverseVars f) binder
  let updateCtx ctx = nameOf binder : ctx
  local (second updateCtx) (update binder')

{-
logScopeEntry :: MonadTraverse m => Expr UnscopedBinding -> m ()
logScopeEntry e = do
  incrCallDepth
  logDebug MaxDetail $ "scope-entry" <+> prettyVerbose e -- <+> "in" <+> pretty ctx

logScopeExit :: MonadTraverse m => NormalisableExpr -> m ()
logScopeExit e = do
  logDebug MaxDetail $ "scope-exit " <+> prettyVerbose e
  decrCallDepth
-}
getImportCtx :: ImportedModules -> DeclScopeCtx
getImportCtx imports =
  Map.fromList $
    [getEntry d | imp <- imports, let Main ds = imp, d <- ds]
  where
    getEntry :: StandardGluedDecl -> (Name, Identifier)
    getEntry d = do
      let ident = identifierOf d
      (nameOf ident, ident)
