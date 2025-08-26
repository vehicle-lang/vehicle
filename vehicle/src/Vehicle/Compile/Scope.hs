{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module Vehicle.Compile.Scope
  ( scopeCheck,
    scopeCheckClosedExpr,
  )
where

import Control.Monad (foldM, void, when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), ReaderT, runReaderT)
import Control.Monad.State (MonadState (..), StateT, evalStateT, gets)
import Control.Monad.Writer (MonadWriter (..), execWriterT)
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (maximumBy, traverse_)
import Data.List (elemIndex, sortOn)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, isNothing, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Text.EditDistance
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Universe (UniverseLevel (..))
import Vehicle.Syntax.AST.Expr qualified as S

scopeCheck :: (MonadCompile m) => Imports -> S.Prog -> m (Prog Builtin)
scopeCheck imports prog = logCompilerPass Scoping $
  runMonadScopeT $ do
    scopeImports imports
    scopeProg prog

scopeCheckClosedExpr :: (MonadCompile m) => S.Expr -> m (Expr Builtin)
scopeCheckClosedExpr e = runMonadScopeT (runMonadScopeExprT (scopeExpr e))

--------------------------------------------------------------------------------
-- Scope checking over declarations

data DeclCtx = DeclCtx
  { recordIdentifiersByField :: Map FieldName Identifier,
    recordIdentifiersByFields :: Map (Set FieldName) Identifier,
    fieldsByRecordIdentifier :: Map Identifier (Set FieldName),
    declsIdentifiersByName :: Map Name Identifier
  }

emptyDeclCtx :: DeclCtx
emptyDeclCtx = DeclCtx mempty mempty mempty mempty

type MonadScope m =
  ( MonadCompile m,
    MonadState DeclCtx m
  )

runMonadScopeT :: (MonadCompile m) => StateT DeclCtx m a -> m a
runMonadScopeT = flip evalStateT emptyDeclCtx

-- | Called when parsing a record definition field-by-field so that
-- earlier fields are in scope for later fields.
addNewRecordDefField :: (MonadScope m) => Identifier -> FieldName -> m ()
addNewRecordDefField ident newField = do
  DeclCtx {..} <- get

  case Map.lookup newField recordIdentifiersByField of
    Nothing -> return ()
    Just existingIdentifier ->
      throwError $ DeclarationDeclarationShadowing (provenanceOf newField) (Left newField) existingIdentifier

  put $
    DeclCtx
      { recordIdentifiersByField = Map.insert newField ident recordIdentifiersByField,
        ..
      }

-- | Called when finishing parsing a record definition so that we can add
-- the information necessary to do efficient parsing of instances of that
-- record.
addNewRecordDef :: (MonadScope m) => Identifier -> [FieldName] -> m ()
addNewRecordDef ident fields = do
  DeclCtx {..} <- get
  let fieldSet = Set.fromList fields
  put $
    DeclCtx
      { recordIdentifiersByFields = Map.insert fieldSet ident recordIdentifiersByFields,
        fieldsByRecordIdentifier = Map.insert ident fieldSet fieldsByRecordIdentifier,
        ..
      }

addNewDecl :: (MonadScope m, HasProvenance decl, HasIdentifier decl) => decl -> m ()
addNewDecl decl = do
  DeclCtx {..} <- get
  let ident = identifierOf decl
  let name = nameOf ident

  case Map.lookup name declsIdentifiersByName of
    Nothing -> return ()
    Just existingIdent ->
      throwError $ DeclarationDeclarationShadowing (provenanceOf decl) (Right name) existingIdent

  put $
    DeclCtx
      { declsIdentifiersByName = Map.insert name ident declsIdentifiersByName,
        ..
      }

lookupFreeVariable :: (MonadScope m) => Name -> m (Maybe Identifier)
lookupFreeVariable name = do
  DeclCtx {..} <- get
  return $ Map.lookup name declsIdentifiersByName

lookupRecordDefinitionByField :: (MonadScopeExpr m) => FieldName -> m Identifier
lookupRecordDefinitionByField field = do
  DeclCtx {..} <- get
  case Map.lookup field recordIdentifiersByField of
    Just definitionIdent -> return definitionIdent
    Nothing -> do
      let fieldName = nameOf field
      fields <- getFieldsInScope
      let suggestions = mispellingsSortedByLikelihood fieldName (fmap nameOf fields)
      throwError $ UnboundRecordAccessor (provenanceOf field) fieldName suggestions

lookupRecordDefinitionByFields :: (MonadScopeExpr m) => Provenance -> [FieldName] -> m Identifier
lookupRecordDefinitionByFields p fields = do
  DeclCtx {..} <- get
  case Map.lookup (Set.fromList fields) recordIdentifiersByFields of
    Just ident -> return ident
    Nothing -> do
      let bestMatch = findBestRecordMatch fields (Map.toList fieldsByRecordIdentifier)
      throwError $ UnmatchedRecord p fields bestMatch

getFieldsInScope :: (MonadScope m) => m [FieldName]
getFieldsInScope = gets $ Map.keys . recordIdentifiersByField

--------------------------------------------------------------------------------
-- Scope checking over expressions

type LocalCtx = [Maybe Name]

emptyLocalCtx :: LocalCtx
emptyLocalCtx = mempty

type MonadScopeExpr m =
  ( MonadScope m,
    MonadReader LocalCtx m
  )

runMonadScopeExprT :: (MonadScope m) => ReaderT LocalCtx m a -> m a
runMonadScopeExprT = flip runReaderT emptyLocalCtx

addBinder :: (MonadScopeExpr m, HasProvenance binder, HasName binder (Maybe Name)) => binder -> m a -> m a
addBinder binder continuation = do
  let maybeName = nameOf binder
  case maybeName of
    Nothing -> return ()
    Just name -> do
      maybeFreeVar <- lookupFreeVariable name
      case maybeFreeVar of
        Just {} ->
          -- This restriction is needed so that
          -- `Vehicle.Compile.ResourceFunctionalisation`
          -- doesn't accidentally capture variables.
          throwError $ DeclarationBoundShadowing (provenanceOf binder) name
        Nothing -> return ()

  local (maybeName :) continuation

lookupVariable :: (MonadScopeExpr m) => Name -> m (Maybe (Either Identifier Ix))
lookupVariable name = do
  maybeFreeVar <- lookupFreeVariable name
  case maybeFreeVar of
    Just ident -> return $ Just $ Left ident
    Nothing -> do
      boundCtx <- ask
      case elemIndex (Just name) boundCtx of
        Just i -> return $ Just $ Right $ Ix i
        Nothing -> return Nothing

getAllNamesInScope :: (MonadScopeExpr m) => m [Name]
getAllNamesInScope = do
  DeclCtx {..} <- get
  localCtx <- ask
  return $ catMaybes localCtx <> Map.keys declsIdentifiersByName

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
scopeProg = traverseDecls scopeDecl

scopeDecl :: (MonadScope m) => S.Decl -> m (Decl Builtin)
scopeDecl decl =
  logCompilerSection2 MidDetail ("scoping" <+> quotePretty (identifierOf decl)) $ do
    scopedDecl <- case decl of
      DefAbstract p ident r t -> do
        t' <- scopeTopLevelExpr False t
        return (DefAbstract p ident r t')
      DefFunction p ident anns t e -> do
        t' <- scopeTopLevelExpr True t
        e' <- scopeTopLevelExpr False e
        return (DefFunction p ident anns t' e')
      DefRecord p ident b t fs -> do
        t' <- scopeTopLevelExpr False t
        fs' <- traverse (scopeDefRecordField ident) fs
        addNewRecordDef ident (fmap fst fs')
        return (DefRecord p ident b t' fs')
    addNewDecl scopedDecl

    logCompilerPassOutput (prettyFriendly scopedDecl)
    return scopedDecl

scopeDefRecordField ::
  (MonadScope m) =>
  Identifier ->
  RecordField S.Expr ->
  m (RecordField (Expr Builtin))
scopeDefRecordField ident (field, fieldType) = do
  fieldType' <- scopeTopLevelExpr False fieldType
  addNewRecordDefField ident field
  return (field, fieldType')

scopeTopLevelExpr :: (MonadScope m) => Bool -> S.Expr -> m (Expr Builtin)
scopeTopLevelExpr generalise expr = do
  exprToScope <- if generalise then generaliseExpr expr else return expr
  runMonadScopeExprT (scopeExpr exprToScope)

--------------------------------------------------------------------------------
-- Expr generalisation

type GeneralisableVariable = (Provenance, Name)

generaliseExpr :: (MonadScope m) => S.Expr -> m S.Expr
generaliseExpr expr = do
  candidates <- execWriterT (runMonadScopeExprT (findGeneralisableVariables expr))
  generaliseOverVariables (reverse candidates) expr

findGeneralisableVariables :: (MonadScopeExpr m, MonadWriter [GeneralisableVariable] m) => S.Expr -> m ()
findGeneralisableVariables = \case
  S.Var p v -> registerVar p v
  S.Universe {} -> return ()
  S.Hole {} -> return ()
  S.Builtin {} -> return ()
  S.App fun args -> do
    findGeneralisableVariables fun
    traverse_ (traverse_ findGeneralisableVariables) args
  S.Pi _ binder res ->
    findGeneralisableVariablesBinder binder $ findGeneralisableVariables res
  S.Lam _ binder body -> do
    findGeneralisableVariablesBinder binder $ findGeneralisableVariables body
  S.Let _ bound binder body -> do
    findGeneralisableVariables bound
    findGeneralisableVariablesBinder binder $ findGeneralisableVariables body
  S.Record _ fields ->
    void $ traverseRecordFields findGeneralisableVariables fields
  S.RecordAcc _ record _field ->
    findGeneralisableVariables record

findGeneralisableVariablesBinder :: (MonadScopeExpr m, MonadWriter [GeneralisableVariable] m) => S.Binder -> m () -> m ()
findGeneralisableVariablesBinder binder update = do
  traverse_ findGeneralisableVariables binder
  addBinder binder update

registerVar :: (MonadScopeExpr m, MonadWriter [GeneralisableVariable] m) => Provenance -> Name -> m ()
registerVar p symbol = do
  maybeVar <- lookupVariable symbol
  when (isNothing maybeVar) $ do
    tell [(p, symbol)]

generaliseOverVariables ::
  (MonadCompile m) =>
  [GeneralisableVariable] ->
  S.Expr ->
  m S.Expr
generaliseOverVariables vars e = fst <$> foldM generaliseOverVariable (e, mempty) vars

generaliseOverVariable ::
  (MonadCompile m) =>
  (S.Expr, Set Name) ->
  GeneralisableVariable ->
  m (S.Expr, Set Name)
generaliseOverVariable (expr, seenNames) (p, name)
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

scopeExpr ::
  (MonadScopeExpr m) =>
  S.Expr ->
  m (Expr Builtin)
scopeExpr e = do
  result <- case e of
    S.Var p v -> scopeVar p v
    S.Universe p -> return $ Universe p (UniverseLevel 0)
    S.Hole p n -> return $ Hole p n
    S.Builtin p op -> return $ Builtin p op
    S.App fun args -> App <$> scopeExpr fun <*> traverse (traverse scopeExpr) args
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

  return result

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

--------------------------------------------------------------------------------
-- Utility functions

findBestRecordMatch :: [FieldName] -> [(Identifier, Set FieldName)] -> Maybe (Identifier, RecordMatch)
findBestRecordMatch givenFields possibleFields
  | null possibleFields = Nothing
  | otherwise = do
      let givenFieldsSet = Set.fromList givenFields
      let matches = fmap (second (calculateMatch givenFieldsSet)) possibleFields
      let (ident, bestMatch) = maximumBy (\(_, m1) (_, m2) -> compare (matchScore m1) (matchScore m2)) matches
      if not (null (sharedFields bestMatch) && null (mispellings bestMatch))
        then Just (ident, bestMatch)
        else Nothing

matchScore :: RecordMatch -> (Int, Int, Int)
matchScore RecordMatch {..} =
  ( length sharedFields,
    -length mispellings,
    -(length missingFields + length extraFields)
  )

calculateMatch :: Set FieldName -> Set FieldName -> RecordMatch
calculateMatch recordFields actualFields = do
  let match =
        RecordMatch
          { sharedFields = Set.toList $ Set.intersection recordFields actualFields,
            missingFields = Set.toList $ Set.difference actualFields recordFields,
            extraFields = mempty,
            mispellings = mempty
          }
  let extraNames = Set.toList $ Set.difference recordFields actualFields
  foldr matchMispellings match extraNames
  where
    matchMispellings :: FieldName -> RecordMatch -> RecordMatch
    matchMispellings field RecordMatch {..} = do
      case mispellingsSortedByLikelihood field missingFields of
        [] ->
          RecordMatch
            { missingFields = missingFields,
              mispellings = mispellings,
              extraFields = field : extraFields,
              ..
            }
        matchedField : _ ->
          RecordMatch
            { missingFields = List.delete matchedField missingFields,
              mispellings = (field, matchedField) : mispellings,
              extraFields = extraFields,
              ..
            }

mispellingsSortedByLikelihood :: (HasName object Name) => object -> [object] -> [object]
mispellingsSortedByLikelihood symbol possibilities = do
  let scoredPossibilities = mapMaybe (symbol `isMispellingOf`) possibilities
  let finalPossibilities = sortOn snd scoredPossibilities
  fmap fst finalPossibilities

isMispellingOf :: (HasName object Name) => object -> object -> Maybe (object, Int)
isMispellingOf symbol possibility = do
  let fieldName = Text.unpack $ nameOf symbol
  let distance = levenshteinDistance defaultEditCosts fieldName (Text.unpack $ nameOf possibility)
  if distance <= length fieldName `div` 2
    then Just (possibility, distance)
    else Nothing

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
