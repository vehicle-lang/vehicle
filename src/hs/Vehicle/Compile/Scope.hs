
module Vehicle.Compile.Scope
  ( scopeCheck
  , scopeCheckClosedExpr
  ) where

import Control.Monad.Except ( MonadError(..) )
import Control.Monad.Reader (MonadReader(..), runReaderT)
import Control.Monad.State
import Data.Bifunctor (Bifunctor(..))
import Data.List (elemIndex)
import Data.Map qualified as Map

import Vehicle.Language.Print (prettyVerbose)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Error

scopeCheck :: (MonadLogger m, MonadError CompileError m)
           => InputProg -> m UncheckedProg
scopeCheck e = logCompilerPass MinDetail "scope checking" $
  runReaderT (scopeProg e) mempty

scopeCheckClosedExpr :: (MonadLogger m, MonadError CompileError m)
                     => InputExpr -> m UncheckedExpr
scopeCheckClosedExpr e = evalStateT (runReaderT (scopeExpr e) (mempty, False)) mempty

--------------------------------------------------------------------------------
-- Scope checking monad and context

-- |Type of scope checking contexts.
type BoundCtx = [DBBinding]

type MonadScope m =
  ( MonadCompile m
  , MonadReader (DeclCtx ()) m
  )

type MonadScopeExpr m =
  ( MonadCompile m
  , MonadReader (DeclCtx (), Bool) m
  , MonadState (BoundCtx, [(Provenance, Symbol)]) m
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

scopeProg :: MonadScope m => InputProg -> m UncheckedProg
scopeProg (Main ds) = Main <$> scopeDecls ds

scopeDecls :: MonadScope m => [InputDecl] -> m [UncheckedDecl]
scopeDecls = \case
  []       -> return []
  (d : ds) -> do
    d' <- scopeDecl d
    ds' <- bindDecl (identifierOf d') (scopeDecls ds)
    return (d' : ds')

scopeDecl :: MonadScope m => InputDecl -> m UncheckedDecl
scopeDecl = \case
  DefResource p r ident t ->
    DefResource p r ident <$> scopeDeclExpr False t

  DefFunction p u ident t e ->
    DefFunction p u ident <$> scopeDeclExpr True t <*> scopeDeclExpr False e

  DefPostulate p ident t ->
    DefPostulate p ident <$> scopeDeclExpr False t

scopeDeclExpr :: forall m . MonadScope m => Bool -> InputExpr -> m UncheckedExpr
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

generaliseOverVar :: (Provenance, Symbol) -> UncheckedExpr -> UncheckedExpr
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
    LSeq     ann es       -> LSeq ann <$> traverse scopeExpr es

    Pi  ann binder res -> do
      bindVar binder $ \binder' -> Pi ann binder' <$> scopeExpr res

    Lam ann binder body -> do
      bindVar binder $ \binder' -> Lam ann binder' <$> scopeExpr body

    Let ann bound binder body -> do
      bound' <- scopeExpr bound
      bindVar binder $ \binder' -> Let ann bound' binder' <$> scopeExpr body

    PrimDict _ _tc -> compilerDeveloperError "Found PrimDict during scope checking."

  logScopeExit result
  return result

scopeArg :: MonadScopeExpr m => InputArg -> m UncheckedArg
scopeArg = traverseArgExpr scopeExpr

scopeBinder :: MonadScopeExpr m => InputBinder -> m UncheckedBinder
scopeBinder = traverseBinderType scopeExpr

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
getVar :: MonadScopeExpr m => Provenance -> NamedVar -> m DBVar
getVar ann symbol = do
  (declCtx, generaliseOverMissingVariables) <- ask
  (boundCtx, varsToGeneralise) <- get

  case elemIndex (Just symbol) boundCtx of
    Just i -> return $ Bound i
    Nothing ->
      if Map.member (Identifier symbol) declCtx
        then return $ Free (Identifier symbol)
      else if generaliseOverMissingVariables
        then do
          -- Assumes that we're generalising in reverse order to the occurrences
          -- of unbound variables:
          -- e.g.
          -- test :: a -> b -> c
          -- will become
          -- test :: forall c b a . a -> b -> c
          logDebug MaxDetail $
            "Variable" <+> squotes (pretty symbol) <+> "should be generalised over"

          let newVarsToGeneralise = (ann, symbol) : varsToGeneralise
          let newBoundCtx = boundCtx <> [Just symbol]
          put (newBoundCtx, newVarsToGeneralise)
          let dbIndex = length boundCtx
          return $ Bound dbIndex
      else
        throwError $ UnboundName symbol (provenanceOf ann)