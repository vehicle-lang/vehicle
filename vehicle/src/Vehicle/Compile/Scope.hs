
module Vehicle.Compile.Scope
  ( scopeCheck
  , scopeCheckClosedExpr
  ) where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), asks, runReaderT)
import Control.Monad.State
import Control.Monad.Writer (MonadWriter (..), runWriterT)
import Data.Bifunctor (Bifunctor (..))
import Data.List (elemIndex)
import Data.Map (Map)
import Data.Map qualified as Map

import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Expr.DeBruijn

scopeCheck :: MonadCompile m => InputProg -> m (UncheckedProg, DependencyGraph)
scopeCheck e = logCompilerPass MinDetail "scope checking" $ do
  (prog, dependencies) <- runReaderT (scopeProg e) mempty
  return (prog, fromEdges dependencies)

scopeCheckClosedExpr :: MonadCompile m => InputExpr -> m UncheckedExpr
scopeCheckClosedExpr e = fst <$> runWriterT (evalStateT (runReaderT (scopeExpr e) (mempty, False)) mempty)

--------------------------------------------------------------------------------
-- Scope checking monad and context

type MonadScope m =
  ( MonadCompile m
  , MonadReader (Map Name Identifier) m
  )

type MonadScopeExpr m =
  ( MonadCompile m
  , MonadReader (Map Name Identifier, Bool) m
  , MonadState (BoundCtx DBBinding, [(Provenance, Name)]) m
  , MonadWriter Dependencies m
  )

--------------------------------------------------------------------------------
-- Debug functions

logScopeEntry :: MonadScopeExpr m => InputExpr -> m ()
logScopeEntry e = do
  incrCallDepth
  logDebug MaxDetail $ "scope-entry" <+> prettyVerbose e -- <+> "in" <+> pretty ctx

logScopeExit :: MonadScopeExpr m => UncheckedExpr -> m ()
logScopeExit e = do
  logDebug MaxDetail $ "scope-exit " <+> prettyVerbose e
  decrCallDepth

--------------------------------------------------------------------------------
-- Algorithm

scopeProg :: MonadScope m => InputProg -> m (UncheckedProg, DependencyList)
scopeProg (Main ds) = first Main <$> scopeDecls ds

scopeDecls :: MonadScope m => [InputDecl] -> m ([UncheckedDecl], DependencyList)
scopeDecls = \case
  []       -> return ([], [])
  (d : ds) -> do
    (d', dep) <- runWriterT $ scopeDecl d

    let identName = nameOf $ identifierOf d'
    exists <- asks (Map.member identName)
    if exists
      then throwError $ DuplicateName (provenanceOf d) identName
      else do
        (ds', deps) <- bindDecl (identifierOf d') (scopeDecls ds)
        let dependencies = (identifierOf d, dep) : deps
        return (d' : ds', dependencies)

scopeDecl :: (MonadWriter Dependencies m, MonadScope m) => InputDecl -> m UncheckedDecl
scopeDecl = \case
  DefResource p r ident t ->
    DefResource p r ident <$> scopeDeclExpr False t

  DefFunction p ident t e ->
    DefFunction p ident <$> scopeDeclExpr True t <*> scopeDeclExpr False e

  DefPostulate p ident t ->
    DefPostulate p ident <$> scopeDeclExpr False t

scopeDeclExpr :: (MonadWriter Dependencies m, MonadScope m) => Bool -> InputExpr -> m UncheckedExpr
scopeDeclExpr generalise expr = do
  declCtx <- ask

  (scopedExpr, (_, varsToGeneralise)) <-
    runStateT (runReaderT (scopeExpr expr) (declCtx, generalise)) (mempty, mempty)

  if not generalise || null varsToGeneralise then
    return scopedExpr
  else do
    logDebug MaxDetail $
      "Generalising over the following variables" <+> pretty (map snd varsToGeneralise)
    return $ foldr generaliseOverVar scopedExpr varsToGeneralise

generaliseOverVar :: (Provenance, Name) -> UncheckedExpr -> UncheckedExpr
generaliseOverVar (p, n) = Pi p (ImplicitBinder p (Just n) binderType)
  where binderType = mkHole p ("typeOf[" <> n <> "]")

scopeExpr :: MonadScopeExpr m => InputExpr -> m UncheckedExpr
scopeExpr e = do
  logScopeEntry e
  result <- case e of
    Universe  ann l       -> return $ Universe ann l
    Meta     ann i        -> return $ Meta ann i
    Hole     ann n        -> return $ Hole ann n
    Ann      ann ex t     -> Ann ann <$> scopeExpr ex <*> scopeExpr t
    App      ann fun args -> App ann <$> scopeExpr fun <*> traverse scopeArg args
    Builtin  ann op       -> return $ Builtin ann op
    Var      ann v        -> Var ann <$> getVar ann v
    Literal  ann l        -> return $ Literal ann l
    LVec     ann es       -> LVec ann <$> traverse scopeExpr es

    Pi  ann binder res -> do
      bindVar binder $ \binder' -> Pi ann binder' <$> scopeExpr res

    Lam ann binder body -> do
      bindVar binder $ \binder' -> Lam ann binder' <$> scopeExpr body

    Let ann bound binder body -> do
      bound' <- scopeExpr bound
      bindVar binder $ \binder' -> Let ann bound' binder' <$> scopeExpr body

  logScopeExit result
  return result

scopeArg :: MonadScopeExpr m => InputArg -> m UncheckedArg
scopeArg = traverse scopeExpr

scopeBinder :: MonadScopeExpr m => InputBinder -> m UncheckedBinder
scopeBinder = traverse scopeExpr

bindDecl :: MonadScope m => Identifier -> m a -> m a
bindDecl ident = local (Map.insert (nameOf ident) ident)

bindVar :: MonadScopeExpr m
        => InputBinder
        -> (UncheckedBinder -> m UncheckedExpr)
        -> m UncheckedExpr
bindVar binder update = do
  binder' <- scopeBinder binder
  modify (first (nameOf binder :))
  result <- update binder'
  modify (first tail)
  return result

-- |Find the index for a given name of a given sort.
getVar :: MonadScopeExpr m => Provenance -> Name -> m DBVar
getVar p symbol = do
  (declCtx, generaliseOverMissingVariables) <- ask
  (boundCtx, varsToGeneralise) <- get

  case elemIndex (Just symbol) boundCtx of
    Just i -> return $ Bound i
    Nothing -> case Map.lookup symbol declCtx of
      Just ident -> do
        tell [ident]
        return $ Free ident
      Nothing
        | not generaliseOverMissingVariables -> throwError $ UnboundName p  symbol
        | otherwise -> do
          -- Assumes that we're generalising in reverse order to the occurrences
          -- of unbound variables:
          -- e.g.
          -- test :: a -> b -> c
          -- will become
          -- test :: forall c b a . a -> b -> c
          logDebug MaxDetail $
            "Variable" <+> squotes (pretty symbol) <+> "should be generalised over"

          let newVarsToGeneralise = (p, symbol) : varsToGeneralise
          let newBoundCtx = boundCtx <> [Just symbol]
          put (newBoundCtx, newVarsToGeneralise)
          let dbIndex = length boundCtx
          return $ Bound dbIndex
