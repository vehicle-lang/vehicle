{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module Vehicle.Compile.Scope
  ( scopeModuleDecls,
  )
where

import Control.Monad (forM)
import Data.Foldable (traverse_)
import Data.List.NonEmpty qualified as NonEmpty
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyExternal)
import Vehicle.Compile.Scope.Core
import Vehicle.Compile.Scope.Generalise
import Vehicle.Compile.Scope.RecordInstances (createAuxilliaryRecordDeclarations)
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.ModuleInterface
import Vehicle.Data.Universe (UniverseLevel (..))
import Vehicle.Syntax.AST.Expr qualified as S

--------------------------------------------------------------------------------
-- Decl scoping

scopeModuleDecls ::
  (MonadCompile m) =>
  ModulePath ->
  ImportedModuleContext Builtin ->
  [S.Decl] ->
  m ([Decl Builtin], ModuleScopingInterface)
scopeModuleDecls modulePath initialState decls = do
  logCompilerPass Scoping $ do
    runMonadScopeT modulePath initialState $ do
      concat <$> traverse scopeDecl decls

scopeDecl :: (MonadScope m) => S.Decl -> m [Decl Builtin]
scopeDecl decl =
  logCompileDecl "scoping" decl $ do
    scopedDecls <- case decl of
      DefAbstract p ident r t -> do
        t' <- runMonadScopeExprT $ scopeExpr t
        return [DefAbstract p ident r t']
      DefFunction p ident anns t e -> do
        t' <- runMonadScopeExprT $ scopeExpr =<< generaliseType t
        e' <- runMonadScopeExprT $ scopeExpr e
        return [DefFunction p ident anns t' e']
      DefRecord p ident sort telescope fields -> do
        (telescope', fields') <- runMonadScopeExprT $ scopeRecordDefinition ident telescope fields
        auxiliaryDeclarations <- createAuxilliaryRecordDeclarations p ident sort telescope' fields'
        let defFun = DefRecord p ident sort telescope' fields'
        return $ defFun : auxiliaryDeclarations

    traverse_ addNewDecl scopedDecls
    traverse_ (logCompilerPassOutput . prettyExternal) scopedDecls
    return scopedDecls

--------------------------------------------------------------------------------
-- Expr scoping

scopeRecordDefinition ::
  forall m.
  (MonadScopeExpr m) =>
  Identifier ->
  S.Telescope ->
  S.RecordFields ->
  m (Telescope Builtin, RecordFields Builtin)
scopeRecordDefinition ident telescope fields = go [] telescope
  where
    go :: Telescope Builtin -> S.Telescope -> m (Telescope Builtin, RecordFields Builtin)
    go revScopedTelescope = \case
      binder : binders -> do
        scopeBinder binder $ \binder' ->
          go (binder' : revScopedTelescope) binders
      [] -> do
        let scopedTelescope = reverse revScopedTelescope
        scopedFields <- forM fields $ \(field, fieldType) -> do
          fieldType' <- scopeExpr =<< generaliseType fieldType
          addNewRecordDefField ident scopedTelescope field
          return (field, fieldType')
        addNewRecordDef ident scopedTelescope scopedFields
        return (scopedTelescope, scopedFields)

scopeExpr ::
  (MonadScopeExpr m) =>
  S.Expr ->
  m (Expr Builtin)
scopeExpr e = case e of
  S.Var p v -> scopeVar p v
  S.Universe p -> return $ Universe p (UniverseLevel 0)
  S.Hole p n -> return $ Hole p n
  S.Builtin p op -> scopeBuiltin p op mempty
  S.App fun args -> case fun of
    S.Builtin p op -> scopeBuiltin p op $ NonEmpty.toList args
    _ -> App <$> scopeExpr fun <*> traverse (traverse scopeExpr) args
  S.Pi p binder res ->
    scopeBinder binder $ \binder' ->
      Pi p binder' <$> scopeExpr res
  S.Lam p binder body -> do
    scopeBinder binder $ \binder' ->
      Lam p binder' <$> scopeExpr body
  S.Let p bound binder body -> do
    bound' <- scopeExpr bound
    scopeBinder binder $ \binder' ->
      Let p bound' binder' <$> scopeExpr body
  S.Record p fields -> do
    fields' <- traverseRecordFields scopeExpr fields
    identAndTelescope <- lookupRecordDefinitionByFields p (fmap fst fields')
    let recordType = calculateConcreteRecordType p identAndTelescope
    return $ Record p recordType fields'
  S.RecordAcc p record field -> do
    record' <- scopeExpr record
    (ident, _) <- lookupRecordDefinitionByField field
    let projFn = FreeVar p (Identifier (modulePath ident) (nameOf field))
    return $ normAppList projFn [explicit record']

scopeBuiltin ::
  (MonadScopeExpr m) =>
  Provenance ->
  Builtin ->
  [S.Arg] ->
  m (Expr Builtin)
scopeBuiltin p builtin args = do
  args' <- traverse (traverse scopeExpr) args
  let defaultResult = normAppList (Builtin p builtin) args'
  return defaultResult

-- If we are not scoping a builtin module then insert coercions.
-- builtinModule <- isScopingBuiltinModule
-- if builtinModule
--   then return defaultResult
--   else case insertCoercions p builtin args' of
--     Nothing -> return defaultResult
--     Just coercedResult -> return coercedResult

scopeBinder ::
  (MonadScopeExpr m) =>
  S.Binder ->
  (Binder Builtin -> m a) ->
  m a
scopeBinder binder update = do
  binder' <- traverse scopeExpr binder
  addBinder binder (update binder')

-- | Find the index for a given name of a given sort.
scopeVar :: (MonadScopeExpr m) => Provenance -> Name -> m (Expr builtin)
scopeVar p symbol = do
  maybeVariable <- lookupVariable p symbol
  case maybeVariable of
    Left ident -> return $ FreeVar p ident
    Right ix -> return $ BoundVar p ix

calculateConcreteRecordType ::
  Provenance ->
  (Identifier, Telescope builtin) ->
  Type builtin
calculateConcreteRecordType p (ident, telescope) = do
  let mkHoleArg binder = argFromBinder binder (Hole (provenanceOf binder) (fst $ getNamedBinderInfo binder))
  normAppList (FreeVar p ident) $ fmap mkHoleArg telescope

{-
logScopeEntry :: MonadTraverse m => S.Expr -> m ()
logScopeEntry e = do
  incrCallDepth
  logDebug MaxDetail $ "scope-entry" <+> prettyVerbose e -- <+> "in" <+> pretty ctx

logScopeExit :: MonadTraverse m => S.Expr -> m ()
logScopeExit e = do
  logDebug MaxDetail $ "scope-exit " <+> prettyVerbose e
  decrCallDepth
-}
