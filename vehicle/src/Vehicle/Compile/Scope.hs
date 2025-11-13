{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module Vehicle.Compile.Scope
  ( scopeCheck,
    scopeCheckClosedExpr,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Foldable (traverse_)
import Data.List.NonEmpty qualified as NonEmpty
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Compile.Scope.Coercions (insertCoercions)
import Vehicle.Compile.Scope.Core
import Vehicle.Compile.Scope.Generalise
import Vehicle.Compile.Scope.RecordInstances (createTensorRecordConversionFunctions)
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Universe (UniverseLevel (..))
import Vehicle.Syntax.AST.Expr qualified as S

--------------------------------------------------------------------------------
-- Public interface

scopeCheck :: (MonadCompile m) => Imports -> S.Prog -> m (Prog Builtin)
scopeCheck imports prog = logCompilerPass Scoping $
  runMonadScopeT $ do
    scopeImports imports
    scopeProg prog

scopeCheckClosedExpr :: (MonadCompile m) => S.Expr -> m (Expr Builtin)
scopeCheckClosedExpr e = runMonadScopeT $ do
  runMonadScopeExprT (scopeExpr e)

--------------------------------------------------------------------------------
-- Algorithm

scopeImports :: (MonadScope m) => Imports -> m ()
scopeImports = traverse_ scopeModule
  where
    scopeModule :: (MonadScope m) => Prog Builtin -> m ()
    scopeModule = traverseDecls_ scopeImportDecl

    scopeImportDecl :: (MonadScope m) => Decl Builtin -> m ()
    scopeImportDecl decl = do
      case decl of
        DefAbstract {} -> return ()
        DefFunction {} -> return ()
        DefRecord _ ident _ _ fs -> do
          traverse_ (\(f, _) -> addNewRecordDefField ident f) fs
          addNewRecordDef ident (fmap fst fs)
          return ()
      addNewDecl decl

scopeProg :: (MonadScope m) => S.Prog -> m (Prog Builtin)
scopeProg (Main ds) = do
  scopedDecls <- traverse scopeDecl ds
  return (Main (concat scopedDecls))

scopeDecl :: (MonadScope m) => S.Decl -> m [Decl Builtin]
scopeDecl decl =
  logCompilerSection2 MidDetail ("scoping" <+> quotePretty (identifierOf decl)) $ do
    scopedDecl <- case decl of
      DefAbstract p ident r t -> do
        t' <- scopeTopLevelExpr False t
        return [DefAbstract p ident r t']
      DefFunction p ident anns t e -> do
        t' <- scopeTopLevelExpr True t
        e' <- scopeTopLevelExpr False e
        return [DefFunction p ident anns t' e']
      DefRecord p ident b t fs -> do
        t' <- scopeTopLevelExpr False t
        fs' <- traverse (scopeDefRecordField ident) fs
        addNewRecordDef ident (fmap fst fs')

        conversionFunctions <-
          if isAnnotatedAsTensor b
            then createTensorRecordConversionFunctions t' p ident fs'
            else return []

        let defRecord = DefRecord p ident b t' fs'
        return (defRecord : conversionFunctions)

    traverse_ addNewDecl scopedDecl
    traverse_ (logCompilerPassOutput . prettyFriendly) scopedDecl
    return scopedDecl

scopeDefRecordField ::
  (MonadScope m) =>
  Identifier ->
  RecordField S.Expr ->
  m (RecordField (Expr Builtin))
scopeDefRecordField ident (field, fieldType) = do
  fieldType' <- scopeTopLevelExpr True fieldType
  addNewRecordDefField ident field
  return (field, fieldType')

scopeTopLevelExpr :: (MonadScope m) => Bool -> S.Expr -> m (Expr Builtin)
scopeTopLevelExpr generalise expr = do
  exprToScope <- if generalise then generaliseType expr else return expr
  runMonadScopeExprT (scopeExpr exprToScope)

--------------------------------------------------------------------------------
-- Expr scoping

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
    recordDefinitionIdent <- lookupRecordDefinitionByFields p (fmap fst fields')
    return $ Record p recordDefinitionIdent fields'
  S.RecordAcc p record field -> do
    record' <- scopeExpr record
    recordDefinitionIdent <- lookupRecordDefinitionByField field
    return $ RecordAcc p record' (recordDefinitionIdent, field)

scopeBuiltin ::
  (MonadScopeExpr m) =>
  Provenance ->
  Builtin ->
  [S.Arg] ->
  m (Expr Builtin)
scopeBuiltin p builtin args = do
  args' <- traverse (traverse scopeExpr) args
  case insertCoercions p builtin args' of
    Nothing -> return $ normAppList (Builtin p builtin) args'
    Just coercedResult -> return coercedResult

scopeBinder ::
  (MonadScopeExpr m) =>
  S.Binder ->
  (Binder Builtin -> m (Expr Builtin)) ->
  m (Expr Builtin)
scopeBinder binder update = do
  binder' <- traverse scopeExpr binder
  addBinder binder (update binder')

-- | Find the index for a given name of a given sort.
scopeVar :: (MonadScopeExpr m) => Provenance -> Name -> m (Expr builtin)
scopeVar p symbol = do
  maybeVariable <- lookupVariable symbol
  case maybeVariable of
    Just (Left ident) -> return $ FreeVar p ident
    Just (Right ix) -> return $ BoundVar p ix
    Nothing -> do
      namesInScope <- getAllNamesInScope
      let closestMatches = mispellingsSortedByLikelihood symbol namesInScope
      throwError $ UnboundName p symbol closestMatches

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
