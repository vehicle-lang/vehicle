
module Vehicle.Compile.Scope
  ( scopeCheck
  , scopeCheckClosedExpr
  ) where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), asks, runReaderT)
import Control.Monad.State
import Control.Monad.Writer (MonadWriter (..), execWriterT, runWriterT)
import Data.Bifunctor (Bifunctor (..))
import Data.List (elemIndex)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendlyDBClosed)
import Vehicle.Expr.DeBruijn

scopeCheck :: MonadCompile m
           => ImportedModules
           -> InputProg
           -> m (UncheckedProg, DependencyGraph)
scopeCheck imports e = logCompilerPass MinDetail "scope checking" $ do
  let importCtx = getImportCtx imports
  (prog, dependencies) <- runReaderT (scopeProg e) importCtx
  return (prog, fromEdges dependencies)

scopeCheckClosedExpr :: MonadCompile m => InputExpr -> m UncheckedExpr
scopeCheckClosedExpr e = fst <$>
  runWriterT (runReaderT (scopeExpr e) (mempty, mempty))

--------------------------------------------------------------------------------
-- Scope checking monad and context

type DeclScopeCtx  = Map Name Identifier
type LocalScopeCtx = [Maybe Name]

type MonadScope m =
  ( MonadCompile m
  , MonadReader DeclScopeCtx m
  )

--------------------------------------------------------------------------------
-- Algorithm

scopeProg :: MonadScope m => InputProg -> m (UncheckedProg, DependencyList)
scopeProg (Main ds) = first Main <$> scopeDecls ds

scopeDecls :: MonadScope m => [InputDecl] -> m ([UncheckedDecl], DependencyList)
scopeDecls = \case
  []       -> return ([], [])
  (d : ds) -> do
    let ident = identifierOf d
    let identName = nameOf ident
    (d', dep) <- logCompilerPass MidDetail ("scoping" <+> quotePretty identName) $ do
      (d', dep) <- runWriterT $ scopeDecl d
      logCompilerPassOutput (prettyFriendlyDBClosed d')
      return (d', dep)

    existingEntry <- asks (Map.lookup identName)
    case existingEntry of
      Just existingIdent -> throwError $ DuplicateName (provenanceOf d) identName existingIdent
      Nothing -> do
        (ds', deps) <- bindDecl ident (scopeDecls ds)
        return (d' : ds', (ident, dep) : deps)

scopeDecl :: (MonadWriter Dependencies m, MonadScope m) => InputDecl -> m UncheckedDecl
scopeDecl = \case
  DefResource p ident r t ->
    DefResource p ident r <$> scopeDeclExpr False t

  DefFunction p ident isProperty t e ->
    DefFunction p ident isProperty <$> scopeDeclExpr True t <*> scopeDeclExpr False e

  DefPostulate p ident t ->
    DefPostulate p ident <$> scopeDeclExpr False t

scopeDeclExpr :: (MonadWriter Dependencies m, MonadScope m)
              => Bool
              -> InputExpr
              -> m UncheckedExpr
scopeDeclExpr generalise expr = do
  declCtx <- ask

  exprToScope <- if generalise
    then generaliseExpr declCtx expr
    else return expr

  runReaderT (scopeExpr exprToScope) (declCtx, mempty)

bindDecl :: MonadScope m => Identifier -> m a -> m a
bindDecl ident = local (Map.insert (nameOf ident) ident)

--------------------------------------------------------------------------------
-- Expr generalisation

type GeneralisableVariable = (Provenance, Name)

generaliseExpr :: MonadCompile m => DeclScopeCtx -> InputExpr -> m InputExpr
generaliseExpr declContext expr = do
  candidates <- findGeneralisableVariables declContext expr
  generaliseOverVariables (reverse candidates) expr

findGeneralisableVariables :: MonadCompile m
                           => DeclScopeCtx
                           -> InputExpr
                           -> m [GeneralisableVariable]
findGeneralisableVariables declContext expr =
  execWriterT (runReaderT (traverseVars traverseVar expr) (declContext, mempty))
  where
    traverseVar :: (MonadTraverse m, MonadWriter [GeneralisableVariable] m)
                => Provenance
                -> InputVar
                -> m InputVar
    traverseVar p symbol = do
      (declCtx, boundCtx) <- ask

      when (Map.notMember symbol declCtx && notElem (Just symbol) boundCtx) $ do
        tell [(p, symbol)]

      return symbol

generaliseOverVariables :: MonadCompile m
                        => [GeneralisableVariable]
                        -> InputExpr
                        -> m InputExpr
generaliseOverVariables vars e = fst <$> foldM generalise (e, mempty) vars
  where
  generalise :: MonadCompile m
             => (InputExpr, Set Name)
             -> GeneralisableVariable
             -> m (InputExpr, Set Name)
  generalise (expr, seenNames) (p, name)
    | name `Set.member` seenNames = return (expr, seenNames)
    | otherwise = do
      logDebug MaxDetail $
        "Generalising over unbound variable" <+> quotePretty name
      let binderType = mkHole p ("typeOf[" <> name <> "]")
      let newExpr = Pi p (ImplicitBinder p (Just name) binderType) expr
      return (newExpr, Set.insert name seenNames)

--------------------------------------------------------------------------------
-- Expr scoping

type MonadScopeExpr m =
  ( MonadCompile m
  , MonadReader (DeclScopeCtx, LocalScopeCtx) m
  , MonadWriter Dependencies m
  )

scopeExpr :: MonadScopeExpr m => InputExpr -> m UncheckedExpr
scopeExpr = traverseVars scopeVar

-- |Find the index for a given name of a given sort.
scopeVar :: MonadScopeExpr m => Provenance -> Name -> m DBVar
scopeVar p symbol = do
  (declCtx, boundCtx) <- ask

  case elemIndex (Just symbol) boundCtx of
    Just i -> return $ Bound i
    Nothing -> case Map.lookup symbol declCtx of
      Just ident -> do
        tell [ident]
        return $ Free ident
      Nothing -> throwError $ UnboundName p  symbol

--------------------------------------------------------------------------------
-- Utility functions

type MonadTraverse m =
  ( MonadCompile m
  , MonadReader (DeclScopeCtx, LocalScopeCtx) m
  )

traverseVars :: MonadTraverse m
             => (Provenance -> var1 -> m var2)
             -> Expr InputBinding var1
             -> m (Expr InputBinding var2)
traverseVars f e = do
  result <- case e of
    Var p v -> Var p <$> f p v

    Universe p l  -> return $ Universe p l
    Meta     p i  -> return $ Meta     p i
    Hole     p n  -> return $ Hole     p n
    Builtin  p op -> return $ Builtin  p op
    Literal  p l  -> return $ Literal  p l

    Ann      p ex t     -> Ann  p <$> traverseVars f ex <*> traverseVars f t
    App      p fun args -> App  p <$> traverseVars f fun <*> traverse (traverse (traverseVars f)) args
    LVec     p es       -> LVec p <$> traverse (traverseVars f) es

    Pi  p binder res ->
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

traverseBinder :: MonadTraverse m
               => (Provenance -> var1 -> m var2)
               -> Binder InputBinding var1
               -> (Binder InputBinding var2 -> m (Expr InputBinding var2))
               -> m (Expr InputBinding var2)
traverseBinder f binder update = do
  binder' <- traverse (traverseVars f) binder
  let updateCtx ctx = nameOf binder : ctx
  local (second updateCtx) (update binder')
{-
logScopeEntry :: MonadTraverse m => Expr InputBinding -> m ()
logScopeEntry e = do
  incrCallDepth
  logDebug MaxDetail $ "scope-entry" <+> prettyVerbose e -- <+> "in" <+> pretty ctx

logScopeExit :: MonadTraverse m => UncheckedExpr -> m ()
logScopeExit e = do
  logDebug MaxDetail $ "scope-exit " <+> prettyVerbose e
  decrCallDepth
-}
getImportCtx :: ImportedModules -> DeclScopeCtx
getImportCtx imports = Map.fromList $
  [ getEntry d | imp <- imports, let Main ds = imp, d <- ds]
  where
    getEntry :: TypedDecl -> (Name, Identifier)
    getEntry d = do
      let ident = identifierOf d
      (nameOf ident, ident)
