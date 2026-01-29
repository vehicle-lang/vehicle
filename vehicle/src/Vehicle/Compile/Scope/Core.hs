module Vehicle.Compile.Scope.Core
  ( ScopableBuiltin (..),
    MonadScope,
    ModuleInterface,
    runMonadScopeT,
    addNewDecl,
    addNewRecordDef,
    addNewRecordDefField,
    isScopingBuiltinModule,
    MonadScopeExpr,
    runMonadScopeExprT,
    addBinder,
    lookupRecordDefinitionByField,
    lookupRecordDefinitionByFields,
    lookupVariable,
    lookupMaybeVariable,
    mispellingsSortedByLikelihood,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), ReaderT, asks, runReaderT)
import Control.Monad.State (MonadState (..), StateT (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (maximumBy)
import Data.List (elemIndex)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as Set
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Interface.Print (PrintableBuiltin)
import Vehicle.Data.Code.ModuleInterface
import Vehicle.Libraries.StandardLibrary (isBuiltinModule)
import Vehicle.Syntax.Builtin

--------------------------------------------------------------------------------
-- Builtin interface

class (PrintableBuiltin builtin, Ord builtin) => ScopableBuiltin builtin where
  generateAuxiliaryRecordDefinitions ::
    (MonadCompile m) => Provenance -> Identifier -> Maybe DefRecordSort -> Telescope builtin -> RecordFields builtin -> m [Decl builtin]

  convertScopeBuiltin :: Builtin -> builtin

--------------------------------------------------------------------------------
-- Scope checking over declarations

type MonadScope builtin m =
  ( MonadCompile m,
    ScopableBuiltin builtin,
    MonadReader (ModulePath, ImportedModuleContext builtin) m,
    MonadState (ModuleScopingInterface builtin) m
  )

runMonadScopeT ::
  (MonadCompile m) =>
  ModulePath ->
  ImportedModuleContext builtin ->
  ReaderT (ModulePath, ImportedModuleContext builtin) (StateT (ModuleScopingInterface builtin) m) a ->
  m (a, ModuleScopingInterface builtin)
runMonadScopeT modulePath importedCtx action = do
  runStateT (runReaderT action (modulePath, importedCtx)) emptyModuleScopingInterface

-- | Called when parsing a record definition field-by-field so that
-- earlier fields are in scope for later fields.
addNewRecordDefField ::
  (MonadState (ModuleScopingInterface builtin) m, MonadCompile m) =>
  Identifier ->
  Telescope builtin ->
  FieldName ->
  m ()
addNewRecordDefField ident telescope newField = do
  ModuleScopingInterface {..} <- get
  case Map.lookup newField recordIdentifiersByField of
    Nothing -> return ()
    Just (existingIdentifier, _) ->
      throwError $ DeclarationDeclarationShadowing (provenanceOf newField) (Left newField) existingIdentifier

  put $
    ModuleScopingInterface
      { recordIdentifiersByField = Map.insert newField (ident, telescope) recordIdentifiersByField,
        ..
      }

-- | Called when finishing parsing a record definition so that we can add
-- the information necessary to do efficient parsing of instances of that
-- record.
addNewRecordDef ::
  (MonadState (ModuleScopingInterface builtin) m) =>
  Identifier ->
  Telescope builtin ->
  RecordFields builtin ->
  m ()
addNewRecordDef ident telescope fields = do
  ModuleScopingInterface {..} <- get
  let fieldSet = Set.fromList $ fmap fst fields
  put $
    ModuleScopingInterface
      { recordIdentifiersByFields = Map.insert fieldSet (ident, telescope) recordIdentifiersByFields,
        fieldsByRecordIdentifier = Map.insert ident fieldSet fieldsByRecordIdentifier,
        ..
      }

addNewDecl :: (MonadScope builtin m) => Decl builtin -> m ()
addNewDecl decl = do
  ModuleScopingInterface {..} <- get
  let ident = identifierOf decl
  let name = nameOf ident
  case Map.lookup name declsIdentifiersByName of
    Nothing -> return ()
    Just existingIdent ->
      throwError $ DeclarationDeclarationShadowing (provenanceOf decl) (Right name) existingIdent

  put $
    ModuleScopingInterface
      { declsIdentifiersByName = Map.insert name ident declsIdentifiersByName,
        ..
      }

--------------------------------------------------------------------------------
-- Scope checking over expressions

data LocalCtx builtin = LocalCtx
  { currentModulePath :: ModulePath,
    importedCtx :: ImportedModuleContext builtin,
    currentModuleCtx :: ModuleScopingInterface builtin,
    boundCtx :: [Maybe Name]
  }

type MonadScopeExpr builtin m =
  ( MonadCompile m,
    ScopableBuiltin builtin,
    MonadState (ModuleScopingInterface builtin) m,
    MonadReader (LocalCtx builtin) m
  )

lookupInNonLocalCtx :: (MonadScopeExpr builtin m) => (ModuleScopingInterface builtin -> Maybe a) -> m (Maybe a)
lookupInNonLocalCtx lookupValue = do
  LocalCtx {..} <- ask
  return $ lookupInCombinedContext scopingInterface lookupValue currentModuleCtx importedCtx

concatInNonLocalCtx :: (MonadScopeExpr builtin m, Monoid a) => (ModuleScopingInterface builtin -> a) -> m a
concatInNonLocalCtx lookupValue = do
  LocalCtx {..} <- ask
  return $ concatInCombinedContext scopingInterface lookupValue currentModuleCtx importedCtx

runMonadScopeExprT :: (MonadScope builtin m) => ReaderT (LocalCtx builtin) m a -> m a
runMonadScopeExprT action = do
  (modulePath, importedCtx) <- ask
  currentCtx <- get
  runReaderT action $
    LocalCtx
      { currentModulePath = modulePath,
        importedCtx = importedCtx,
        currentModuleCtx = currentCtx,
        boundCtx = mempty
      }

addBinder :: (MonadScopeExpr builtin m) => GenericBinder expr -> m a -> m a
addBinder binder continuation = do
  case getMaybeNamedBinderInfo binder of
    Nothing -> return ()
    Just (name, p) -> do
      maybeFreeVar <- lookupFreeVariable name
      case maybeFreeVar of
        Just {} ->
          -- This restriction is needed so that
          -- `Vehicle.Compile.ResourceFunctionalisation`
          -- doesn't accidentally capture variables.
          throwError $ DeclarationBoundShadowing p name
        Nothing -> return ()

  flip local continuation $ \LocalCtx {..} ->
    LocalCtx
      { boundCtx = nameOf binder : boundCtx,
        ..
      }

lookupVariable :: (MonadScopeExpr builtin m) => Provenance -> Name -> m (Either Identifier Ix)
lookupVariable p name = do
  maybeResult <- lookupMaybeVariable name
  case maybeResult of
    Just result -> return result
    Nothing -> do
      nonLocalNames <- concatInNonLocalCtx (Map.keys . declsIdentifiersByName)
      localNames <- asks (catMaybes . boundCtx)
      let closestMatches = mispellingsSortedByLikelihood name (localNames <> nonLocalNames)
      throwError $ UnboundName p name closestMatches

lookupMaybeVariable :: (MonadScopeExpr builtin m) => Name -> m (Maybe (Either Identifier Ix))
lookupMaybeVariable name = do
  maybeFreeVar <- lookupFreeVariable name
  case maybeFreeVar of
    Just ident -> return $ Just $ Left ident
    Nothing -> do
      maybeIx <- lookupBoundVariable name
      case maybeIx of
        Just ix -> return $ Just $ Right ix
        Nothing -> return Nothing

lookupFreeVariable :: (MonadScopeExpr builtin m) => Name -> m (Maybe Identifier)
lookupFreeVariable name = do
  LocalCtx {..} <- ask
  return $ lookupInCombinedContext scopingInterface (Map.lookup name . declsIdentifiersByName) currentModuleCtx importedCtx

lookupBoundVariable :: (MonadScopeExpr builtin m) => Name -> m (Maybe Ix)
lookupBoundVariable name = do
  boundCtx <- asks boundCtx
  return (Ix <$> elemIndex (Just name) boundCtx)

lookupRecordDefinitionByField :: (MonadScopeExpr builtin m) => FieldName -> m (Identifier, Telescope builtin)
lookupRecordDefinitionByField field = do
  maybeResult <- lookupInNonLocalCtx (Map.lookup field . recordIdentifiersByField)
  case maybeResult of
    Just result -> return result
    Nothing -> do
      allFieldsInScope <- concatInNonLocalCtx (Map.keys . recordIdentifiersByField)
      let fieldName = nameOf field
      let suggestions = mispellingsSortedByLikelihood fieldName (fmap nameOf allFieldsInScope)
      throwError $ UnboundRecordAccessor (provenanceOf field) fieldName suggestions

lookupRecordDefinitionByFields ::
  (MonadScopeExpr builtin m) =>
  Provenance ->
  [FieldName] ->
  m (Identifier, Telescope builtin)
lookupRecordDefinitionByFields p fields = do
  let fieldSet = Set.fromList fields
  maybeResult <- lookupInNonLocalCtx (Map.lookup fieldSet . recordIdentifiersByFields)
  case maybeResult of
    Just result -> return result
    Nothing -> do
      allFieldsByIdentifier <- concatInNonLocalCtx (Map.toList . fieldsByRecordIdentifier)
      let bestMatch = findBestRecordMatch fields allFieldsByIdentifier
      throwError $ UnmatchedRecord p fields bestMatch

isScopingBuiltinModule :: (MonadScopeExpr builtin m) => m Bool
isScopingBuiltinModule = asks $ isBuiltinModule . currentModulePath

--------------------------------------------------------------------------------
-- Utility functions

findBestRecordMatch ::
  [FieldName] ->
  [(Identifier, Set FieldName)] ->
  Maybe (Identifier, RecordMatch)
findBestRecordMatch givenFields possibleFields
  | null possibleFields = Nothing
  | otherwise = do
      let givenFieldsSet = Set.fromList givenFields
      let matches = fmap (second (calculateMatch givenFieldsSet)) possibleFields
      let (ident, bestMatch) = maximumBy (\(_, m1) (_, m2) -> compareMatches m1 m2) matches
      if not (null (sharedFields bestMatch) && null (mispellings bestMatch))
        then Just (ident, bestMatch)
        else Nothing

-- Orders by best match (i.e. GT == better ordering)
compareMatches :: RecordMatch -> RecordMatch -> Ordering
compareMatches m1 m2 =
  -- Prioritise matches with the highest number of shared fields
  comparing (length . sharedFields) m1 m2
    <>
    -- Otherwise prioritise matches with the highest number of potential misspellings
    comparing (length . mispellings) m1 m2
    <>
    -- Otherwise prioritise matches with the lowest number of total and missing fields
    comparing (\m -> -length (missingFields m) - length (extraFields m)) m1 m2

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
    matchMispellings field RecordMatch {..} = case mispellingsSortedByLikelihood field missingFields of
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
