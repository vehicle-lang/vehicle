module Vehicle.List
  ( ListOptions (..),
    list,
  )
where

import Control.Monad.Writer (MonadWriter (tell), execWriterT)
import Data.Aeson (ToJSON (..))
import Data.Foldable (traverse_)
import Data.Proxy (Proxy (..))
import Data.Text (Text, pack)
import GHC.Generics
import Vehicle.Compile.Error (CompileError (MultiPropertyTraveralError), MultiPropertyTraveralError (..))
import Vehicle.Compile.ExpandResources (expandResources)
import Vehicle.Compile.Normalise.NBE (evalDecl, normaliseClosure)
import Vehicle.Compile.Prelude hiding (Dataset, Network, Parameter, name)
import Vehicle.Compile.Print
import Vehicle.Compile.Print.Error (prettyCompileError)
import Vehicle.Compile.Property (traverseMultiProperty)
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Builtin.Standard (Builtin (..), Quantifier)
import Vehicle.Data.Code.Interface (QuantifyRatTensorArgs (..), accessQuantifyRatTensor)
import Vehicle.Data.Code.Value (Closure, Spine, VDecl, VType, Value (..))
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Data.Variable.Free.Context (MonadFreeContext, addDeclEntryToContext, runFreshFreeContextT)
import Vehicle.Prelude.Logging.Instance
import Vehicle.TypeCheck (TypeCheckOptions (..), runCompileMonad, typeCheckUserProg)
import Vehicle.Verify.Core (PropertyAddress, PropertyID)
import Vehicle.Verify.Specification (MultiProperty)

--------------------------------------------------------------------------------
-- List mode

data ListOptions = ListOptions
  { specification :: FilePath,
    networkLocations :: NetworkLocations,
    datasetLocations :: DatasetLocations,
    parameterValues :: ParameterValues
  }
  deriving (Eq, Show)

list :: (MonadStdIO IO) => LoggingSettings -> OutputAsJSON -> ListOptions -> IO ()
list loggingSettings outputAsJSON ListOptions {..} =
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

--------------------------------------------------------------------------------
-- Program traversal

-- | Print all the listable entities in the program
searchProg :: (MonadLogger m, MonadStdIO m) => Prog Builtin -> m [ListableEntity]
searchProg (Main decls) =
  runFreshFreeContextT (Proxy @Builtin) $
    runSupplyT [(0 :: PropertyID) ..] $
      execWriterT $
        searchDecls decls

type MonadList m =
  ( MonadLogger m,
    MonadWriter [ListableEntity] m,
    MonadFreeContext Builtin m,
    MonadStdIO m
  )

searchDecls :: (MonadList m, MonadSupply PropertyID m) => [Decl Builtin] -> m ()
searchDecls = \case
  [] -> return ()
  d : ds -> do
    normDecl <- evalDecl d
    searchDecl normDecl
    addDeclEntryToContext normDecl $ searchDecls ds

searchDecl :: (MonadList m, MonadSupply PropertyID m) => VDecl Builtin -> m ()
searchDecl decl = do
  let sharedData = mkSharedData (provenanceOf decl) (nameOf decl)
  case decl of
    DefAbstract _ _ sort t -> case sort of
      NetworkDef -> tell [Network $ NetworkSummary (sharedData t)]
      DynamicsDef -> tell [Dynamics $ DynamicsSummary (sharedData t)]
      DatasetDef -> tell [Dataset $ DatasetSummary (sharedData t)]
      ParameterDef s -> tell [Parameter $ ParameterSummary (sharedData t) (isInferable s)]
      BuiltinDef -> return ()
    DefFunction _ _ sort typ body
      | not $ isAnnotatedAsProperty sort -> return ()
      | otherwise -> do
          entity <- searchPropertyDecl (identifierOf decl, provenanceOf decl) (sharedData typ) typ body
          tell [entity]
    DefRecord {} -> return ()

searchPropertyDecl :: (MonadList m, MonadSupply PropertyID m) => DeclProvenance -> SharedData -> VType Builtin -> Value Builtin -> m ListableEntity
searchPropertyDecl prov sharedData declType declBody = do
  propertyID <- demand
  traversalErrorOrResult <- traverseMultiProperty searchProperty propertyID (name sharedData) declType declBody

  case traversalErrorOrResult of
    Right result -> return $ Property $ PropertySummary sharedData (Just result)
    Left err -> do
      let evaluationBlockedByUnprovidedResource = do
            return $ Property $ PropertySummary sharedData Nothing
      let mkActualError = fatalError $ prettyCompileError True $ MultiPropertyTraveralError prov err
      case err of
        UnsupportedVectorDimension {} -> evaluationBlockedByUnprovidedResource
        UnsupportedTensorDimensions {} -> evaluationBlockedByUnprovidedResource
        UnsupportedVectorValue {} -> mkActualError
        UnreducableTensorValue {} -> mkActualError
        UnreducableType {} -> mkActualError

searchProperty :: (MonadList m) => PropertyAddress -> Value Builtin -> m [QuantifiedVariableSummary]
searchProperty _address value = runFreshNameBoundContextT $ execWriterT (searchValue value)

type MonadListProperty m =
  ( MonadNameContext m,
    MonadWriter [QuantifiedVariableSummary] m,
    MonadFreeContext Builtin m
  )

-- | Traverse a value to find all quantified variables
searchValue :: (MonadListProperty m) => Value Builtin -> m ()
searchValue value = case value of
  VBoundVar _ spine -> searchSpine spine
  VFreeVar _ spine -> searchSpine spine
  VBuiltin _ spine -> do
    searchBuiltinForQuantifier value
    searchSpine spine
  VLam binder closure -> do
    body <- normaliseClosure binder closure
    searchValue body
  VRecord _ fields -> traverse_ searchValue fields
  VRecordAcc _ record _ spine -> do searchValue record; searchSpine spine
  -- Never traverse into types so the following cases shouldn't happen!
  VUniverse {} -> unexpectedExprError pass "VUniverse"
  VPi {} -> unexpectedExprError pass "VUniverse"
  VMeta {} -> unexpectedExprError pass "VMeta"
  where
    pass = "list"

searchSpine :: (MonadListProperty m) => Spine Builtin -> m ()
searchSpine = traverse_ (traverse_ searchValue)

searchBuiltinForQuantifier :: (MonadListProperty m) => Value Builtin -> m ()
searchBuiltinForQuantifier value = case getExpr (accessQuantifyRatTensor @Value @Builtin @Closure) value of
  Just (q, args) -> do
    let (name, p) = getNamedBinderInfo (quantifyBinder args)
    let sharedData = mkSharedData p name (typeOf $ quantifyBinder args)
    tell [QuantifiedVariableSummary sharedData q]
  _ -> return ()

--------------------------------------------------------------------------------
-- JSON output format
--------------------------------------------------------------------------------

data ListableEntity
  = Network NetworkSummary
  | Dynamics DynamicsSummary
  | Dataset DatasetSummary
  | Parameter ParameterSummary
  | Property PropertySummary
  deriving (Generic)

instance ToJSON ListableEntity

--------------------------------------------------------------------------------
-- Shared data

data SharedData = SharedData
  { provenance :: Provenance,
    name :: Text,
    typeText :: Text
  }
  deriving (Generic)

instance ToJSON SharedData

mkSharedData ::
  Provenance ->
  Name ->
  VType Builtin ->
  SharedData
mkSharedData p name entityType =
  SharedData
    { name = name,
      typeText = pack $ show $ prettyFriendlyEmptyCtx entityType,
      provenance = p
    }

--------------------------------------------------------------------------------
-- Network

newtype NetworkSummary = NetworkSummary
  { sharedData :: SharedData
  }
  deriving (Generic)

instance ToJSON NetworkSummary

--------------------------------------------------------------------------------
-- Dynamics

newtype DynamicsSummary = DynamicsSummary
  { sharedData :: SharedData
  }
  deriving (Generic)

instance ToJSON DynamicsSummary

--------------------------------------------------------------------------------
-- Data

newtype DatasetSummary = DatasetSummary
  { sharedData :: SharedData
  }
  deriving (Generic)

instance ToJSON DatasetSummary

--------------------------------------------------------------------------------
-- Parameter

data ParameterSummary = ParameterSummary
  { sharedData :: SharedData,
    inferable :: Bool
  }
  deriving (Generic)

instance ToJSON ParameterSummary

--------------------------------------------------------------------------------
-- Property

data PropertySummary = PropertySummary
  { sharedData :: SharedData,
    subcomponents :: Maybe (MultiProperty [QuantifiedVariableSummary])
  }
  deriving (Generic)

instance ToJSON PropertySummary

--------------------------------------------------------------------------------
-- Quantified variable

data QuantifiedVariableSummary = QuantifiedVariableSummary
  { sharedData :: SharedData,
    quantifier :: Quantifier
  }
  deriving (Generic)

instance ToJSON QuantifiedVariableSummary
