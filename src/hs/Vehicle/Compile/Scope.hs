
module Vehicle.Compile.Scope
  ( scopeCheck
  , scopeCheckClosedExpr
  ) where

import Control.Monad.Except (MonadError(..))
import Control.Monad.Writer (MonadWriter(..), runWriterT)
import Control.Monad.Reader (MonadReader(..), runReaderT, asks)
import Control.Monad.State
import Data.Bifunctor (Bifunctor(..))
import Data.List (elemIndex)
import Data.Map qualified as Map

import Vehicle.Language.Print (prettyVerbose)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Error

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
  , MonadReader (DeclCtx ()) m
  )

type MonadScopeExpr m =
  ( MonadCompile m
  , MonadReader (DeclCtx (), Bool) m
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

    let ident = identifierOf d'
    exists <- asks (Map.member ident)
    if exists
      then throwError $ DuplicateName (provenanceOf d) (nameOf ident)
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
    Universe    p l  -> return $ Universe p l
    Meta        p i  -> return $ Meta p i
    Hole        p n  -> return $ Hole p n
    Constructor p c  -> return $ Constructor p c
    Builtin     p op -> return $ Builtin p op

    Ann      p ex t     -> Ann p <$> scopeExpr ex <*> scopeExpr t
    App      p fun args -> App p <$> scopeExpr fun <*> traverse scopeArg args
    Var      p v        -> Var p <$> getVar p v
    Literal  p l        -> return $ Literal p l
    LVec     p es       -> LVec p <$> traverse scopeExpr es

    Pi  p binder res -> do
      bindVar binder $ \binder' -> Pi p binder' <$> scopeExpr res

    Lam p binder body -> do
      bindVar binder $ \binder' -> Lam p binder' <$> scopeExpr body

    Let p bound binder body -> do
      bound' <- scopeExpr bound
      bindVar binder $ \binder' -> Let p bound' binder' <$> scopeExpr body

  logScopeExit result
  return result

scopeArg :: MonadScopeExpr m => InputArg -> m UncheckedArg
scopeArg = traverse scopeExpr

scopeBinder :: MonadScopeExpr m => InputBinder -> m UncheckedBinder
scopeBinder = traverse scopeExpr

bindDecl :: MonadScope m => Identifier -> m a -> m a
bindDecl ident = local (Map.insert ident ())

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
    Nothing -> do
      let ident = Identifier symbol
      if Map.member ident declCtx then do
        tell [ident]
        return $ Free ident
      else if not generaliseOverMissingVariables
        then throwError $ UnboundName p  symbol
      else do
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