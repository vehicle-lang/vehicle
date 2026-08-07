module Vehicle.List
  ( ListOptions (..),
    ListEntitiesOptions (..),
    ListRecordsOptions (..),
    list,
  )
where

import Control.Monad.State (MonadState, execStateT, modify)
import Control.Monad.Writer.Strict (MonadWriter (tell), execWriterT)
import Data.Aeson (ToJSON (..), genericToJSON)
import Data.Foldable (traverse_)
import Data.Proxy (Proxy (..))
import Data.Text (Text, pack)
import GHC.Generics
import Vehicle.Compile.Error (CompileError (MultiPropertyTraveralError), MultiPropertyTraveralError (..))
import Vehicle.Compile.ExpandResources (expandResources)
import Vehicle.Compile.Normalise.Force
import Vehicle.Compile.Prelude hiding (Dataset, Network, Parameter, datasets, name, networks, parameters)
import Vehicle.Compile.Print
import Vehicle.Compile.Print.Error (prettyCompileError)
import Vehicle.Compile.Property (traverseMultiProperty)
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Builtin.Standard (Builtin (..), Quantifier)
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Code.Interface (QuantifyRatTensorArgs (..), accessQuantifyRatTensor)
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Data.Variable.Free.Context (MonadFreeContext, addDeclEntryToContext, runFreshFreeContextT)
import Vehicle.List.RecordSchema (listRecordSchemas)
import Vehicle.Prelude.Logging.Instance
import Vehicle.TypeCheck (TypeCheckOptions (..), runCompileMonad, typeCheckUserProg)
import Vehicle.Verify.Core (PropertyAddress)
import Vehicle.Verify.Specification (MultiProperty)

--------------------------------------------------------------------------------
-- List mode

data ListOptions
  = ListEntitiesTarget ListEntitiesOptions
  | ListRecordsTarget ListRecordsOptions
  deriving (Eq, Show)

data ListEntitiesOptions = ListEntitiesOptions
  { specification :: FilePath,
    networkLocations :: NetworkLocations,
    datasetLocations :: DatasetLocations,
    parameterValues :: ParameterValues
  }
  deriving (Eq, Show)

data ListRecordsOptions = ListRecordsOptions
  { specification :: FilePath,
    outputFile :: Maybe FilePath
  }
  deriving (Eq, Show)

list :: (MonadStdIO IO) => LoggingSettings -> OutputAsJSON -> ListOptions -> IO ()
list loggingSettings outputAsJSON = \case
  ListEntitiesTarget opts -> listEntities loggingSettings outputAsJSON opts
  ListRecordsTarget opts -> listRecords loggingSettings outputAsJSON opts

listEntities :: (MonadStdIO IO) => LoggingSettings -> OutputAsJSON -> ListEntitiesOptions -> IO ()
listEntities loggingSettings outputAsJSON ListEntitiesOptions {..} =
  runCompileMonad loggingSettings outputAsJSON $ do
    -- Type check the program
    typedProg <-
      typeCheckUserProg $
        TypeCheckOptions
          { specification = specification,
            secondaryTypeSystem = Nothing,
            declarationsToCompile = mempty
          }

    -- Expand any of the provided resources as the set of properties may be
    -- dependent on e.g. dataset size. Note that not all resources have
    -- to be provided, so we don't check the list of missing resources.
    let resources = Resources specification networkLocations datasetLocations parameterValues
    (expandedProg, _, _, _, _) <- expandResources resources typedProg

    -- Search for entities
    entities <- searchProg expandedProg

    -- Produce the output (at the moment only support JSON)
    programOutput $ prettyAsJSON entities

listRecords :: (MonadStdIO IO) => LoggingSettings -> OutputAsJSON -> ListRecordsOptions -> IO ()
listRecords loggingSettings outputAsJSON ListRecordsOptions {..} =
  runCompileMonad loggingSettings outputAsJSON $ do
    typedProg <-
      typeCheckUserProg $
        TypeCheckOptions
          { specification = specification,
            secondaryTypeSystem = Nothing,
            declarationsToCompile = mempty
          }
    listRecordSchemas outputFile typedProg

--------------------------------------------------------------------------------
-- Output format
--------------------------------------------------------------------------------

data SpecificationSummary = SpecificationSummary
  { networks :: [NetworkSummary],
    datasets :: [DatasetSummary],
    parameters :: [ParameterSummary],
    properties :: [PropertySummary]
  }
  deriving (Generic)

data NetworkSummary = NetworkSummary
  { provenance :: Provenance,
    name :: Text,
    typeText :: Text
  }
  deriving (Generic)

data DatasetSummary = DatasetSummary
  { provenance :: Provenance,
    name :: Text,
    typeText :: Text
  }
  deriving (Generic)

data ParameterSummary = ParameterSummary
  { provenance :: Provenance,
    name :: Text,
    typeText :: Text,
    inferable :: Bool
  }
  deriving (Generic)

data PropertySummary = PropertySummary
  { provenance :: Provenance,
    name :: Text,
    typeText :: Text,
    quantifiedVariables :: Maybe (MultiProperty [QuantifiedVariableSummary])
  }
  deriving (Generic)

data QuantifiedVariableSummary = QuantifiedVariableSummary
  { provenance :: Provenance,
    name :: Text,
    typeText :: Text,
    quantifier :: Quantifier
  }
  deriving (Generic)

instance ToJSON SpecificationSummary where
  toJSON = genericToJSON jsonOptions

instance ToJSON NetworkSummary where
  toJSON = genericToJSON jsonOptions

instance ToJSON DatasetSummary where
  toJSON = genericToJSON jsonOptions

instance ToJSON ParameterSummary where
  toJSON = genericToJSON jsonOptions

instance ToJSON PropertySummary where
  toJSON = genericToJSON jsonOptions

instance ToJSON QuantifiedVariableSummary where
  toJSON = genericToJSON jsonOptions

--------------------------------------------------------------------------------
-- Utility methods

type MonadList m =
  ( MonadLogger m,
    MonadStdIO m,
    MonadState SpecificationSummary m,
    MonadFreeContext Builtin m
  )

-- | Print all the listable entities in the program
searchProg :: (MonadLogger m, MonadStdIO m) => Prog Builtin -> m SpecificationSummary
searchProg (Main decls) = do
  let initialSummary = SpecificationSummary mempty mempty mempty mempty
  summary <- runFreshFreeContextT (Proxy @Builtin) $ execStateT (searchDecls decls) initialSummary
  return $
    SpecificationSummary
      { networks = reverse $ networks summary,
        datasets = reverse $ datasets summary,
        parameters = reverse $ parameters summary,
        properties = reverse $ properties summary
      }

searchDecls :: (MonadList m) => [Decl Builtin] -> m ()
searchDecls = \case
  [] -> return ()
  d : ds -> do
    searchDecl d
    addDeclEntryToContext d $ searchDecls ds

searchDecl :: (MonadList m) => Decl Builtin -> m ()
searchDecl decl = do
  case decl of
    DefAbstract p ident sort typ -> do
      let normType = Unforced emptyBoundEnv typ
      case sort of
        NetworkDef -> addNetwork p ident normType
        DatasetDef -> addDataset p ident normType
        ParameterDef s -> addParameter p ident normType s
        BuiltinDef -> return ()
    DefFunction p ident sort typ body
      | not $ isAnnotatedAsProperty sort -> return ()
      | otherwise -> do
          let normType = Unforced emptyBoundEnv typ
          let normBody = Unforced emptyBoundEnv body
          addProperty p ident normType normBody
    DefRecord {} -> return ()

addNetwork :: (MonadList m) => Provenance -> Identifier -> UnforcedType Builtin -> m ()
addNetwork p ident typ = do
  let summary =
        NetworkSummary
          { provenance = p,
            name = nameOf ident,
            typeText = entityTypeText typ
          }
  modify $ \s -> s {networks = summary : networks s}

addDataset :: (MonadList m) => Provenance -> Identifier -> UnforcedType Builtin -> m ()
addDataset p ident typ = do
  let summary =
        DatasetSummary
          { provenance = p,
            name = nameOf ident,
            typeText = entityTypeText typ
          }
  modify $ \s -> s {datasets = summary : datasets s}

addParameter :: (MonadList m) => Provenance -> Identifier -> UnforcedType Builtin -> ParameterSort -> m ()
addParameter p ident typ sort = do
  let summary =
        ParameterSummary
          { provenance = p,
            name = nameOf ident,
            typeText = entityTypeText typ,
            inferable = isInferable sort
          }
  modify $ \s -> s {parameters = summary : parameters s}

addProperty :: (MonadList m) => Provenance -> Identifier -> UnforcedType Builtin -> Thunk Builtin -> m ()
addProperty p ident typ body = do
  quantifiedVariables <- searchPropertyDecl (ident, p) typ body
  let summary =
        PropertySummary
          { provenance = p,
            name = nameOf ident,
            typeText = entityTypeText typ,
            quantifiedVariables = quantifiedVariables
          }
  modify $ \s -> s {properties = summary : properties s}

type MonadListProperty m =
  ( MonadNameContext m,
    MonadWriter [QuantifiedVariableSummary] m,
    MonadFreeContext Builtin m
  )

searchPropertyDecl :: (MonadList m) => DeclProvenance -> UnforcedType Builtin -> Thunk Builtin -> m (Maybe (MultiProperty [QuantifiedVariableSummary]))
searchPropertyDecl prov@(ident, _) declType declBody = do
  traversalErrorOrResult <- runFreshNameBoundContextT $ traverseMultiProperty searchProperty (nameOf ident) declType declBody

  case traversalErrorOrResult of
    Right result -> return $ Just result
    Left err -> do
      let mkActualError = fatalError $ prettyCompileError True $ MultiPropertyTraveralError prov err
      case err of
        UnsupportedVectorDimension {} -> return Nothing
        UnsupportedTensorDimensions {} -> return Nothing
        UnsupportedVectorValue {} -> mkActualError
        UnreducableTensorValue {} -> mkActualError
        UnreducableType {} -> mkActualError

searchProperty :: (MonadList m, MonadNameContext m) => PropertyAddress -> Thunk Builtin -> m [QuantifiedVariableSummary]
searchProperty _address value = execWriterT (searchValue value)

-- | Traverse a value to find all quantified variables
searchValue :: (MonadListProperty m) => Thunk Builtin -> m ()
searchValue value = do
  forcedValue <- forceThunk value
  case forcedValue of
    VBoundVar _ spine -> searchSpine spine
    VFreeVar _ spine -> searchSpine spine
    VBuiltin _ spine -> do
      searchBuiltinForQuantifier value
      searchSpine spine
    VLam binder closure -> do
      lv <- getBinderDepth
      let body = extendClosureWithBound closure binder lv
      addNameToContext binder $ searchValue body
    VRecord _ fields -> traverse_ searchValue fields
    VRecordAcc _ record _ spine -> do searchValue record; searchSpine spine
    -- Never traverse into types so the following cases shouldn't happen!
    VUniverse {} -> unexpectedExprError "list" "VUniverse"
    VPi {} -> unexpectedExprError "list" "VPi"

searchSpine :: (MonadListProperty m) => UnforcedSpine Builtin -> m ()
searchSpine = traverse_ (traverse_ searchValue)

searchBuiltinForQuantifier :: (MonadListProperty m) => Thunk Builtin -> m ()
searchBuiltinForQuantifier value = do
  forcedValue <- forceThunk value
  case getExpr (accessQuantifyRatTensor @ForcedValue @Thunk @Builtin @Closure) forcedValue of
    Just (q, args) -> do
      let (name, p) = getNamedBinderInfo (quantifyBinder args)
      let typeText = entityTypeText (typeOf $ quantifyBinder args)
      tell [QuantifiedVariableSummary p name typeText q]
    _ -> return ()

entityTypeText :: UnforcedType Builtin -> Text
entityTypeText entityType = pack $ show $ prettyFriendlyEmptyCtx entityType
