module Vehicle.Compile.Scope.Core where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), ReaderT, runReaderT)
import Control.Monad.State (MonadState (..), StateT, evalStateT, gets)
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (maximumBy)
import Data.List (elemIndex, sortOn)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Text.EditDistance
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude

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
