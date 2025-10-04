module Vehicle.List
  ( ListOptions (..),
    list,
  )
where

import Control.Monad.Writer (MonadWriter (tell), execWriterT)
import Data.Aeson (ToJSON (..))
import Data.Aeson.Encode.Pretty (encodePretty')
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Foldable (traverse_)
import Data.Proxy (Proxy (..))
import Data.Text (Text, pack)
import GHC.Generics
import Vehicle.Compile.Normalise.NBE (normaliseClosure, normaliseInEmptyEnv)
import Vehicle.Compile.Prelude hiding (Dataset, Network, Parameter)
import Vehicle.Compile.Print
import Vehicle.Data.Builtin.Core (BuiltinFunction (..), Quantifier)
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Code.Interface (IsArgs (..), QuantifyRatTensorArgs (QuantifyRatTensorArgs))
import Vehicle.Data.Code.Value (Spine, VDecl, Value (..))
import Vehicle.Data.Variable.Bound.Context.Name (MonadNameContext, getNameContext, runFreshNameContextT)
import Vehicle.Data.Variable.Free.Context (MonadFreeContext, addDeclEntryToContext, runFreshFreeContextT)
import Vehicle.Prelude.Logging.Instance
import Vehicle.Syntax.Builtin (Builtin (..))
import Vehicle.TypeCheck (TypeCheckOptions (..), runCompileMonad, typeCheckUserProg)

--------------------------------------------------------------------------------
-- List mode

newtype ListOptions = ListOptions
  { specification :: FilePath
  }
  deriving (Eq, Show)

list :: (MonadStdIO IO) => LoggingSettings -> OutputAsJSON -> ListOptions -> IO ()
list loggingSettings outputAsJSON ListOptions {..} =
  runCompileMonad loggingSettings outputAsJSON $ do
    -- Type check the program
    Main decls <-
      typeCheckUserProg $
        TypeCheckOptions
          { specification = specification,
            secondaryTypeSystem = Nothing,
            declarationsToCompile = mempty
          }

    -- Search for entities
    entities <- runFreshFreeContextT (Proxy @Builtin) $ execWriterT $ searchDecls decls

    -- Produce the output (at the moment only support JSON)
    programOutput $ pretty $ unpack $ encodePretty' prettyJSONConfig $ toJSON entities

--------------------------------------------------------------------------------
-- Program traversal

type MonadList m =
  ( MonadLogger m,
    MonadWriter [ListableEntity] m,
    MonadFreeContext Builtin m
  )

-- | Print all the listable entities in the program
searchDecls :: (MonadList m) => [Decl Builtin] -> m ()
searchDecls = \case
  [] -> return ()
  d : ds -> do
    normDecl <- traverse normaliseInEmptyEnv d
    searchDecl normDecl
    addDeclEntryToContext (d, normDecl) $ searchDecls ds

searchDecl :: (MonadList m) => VDecl Builtin -> m ()
searchDecl decl = do
  let sharedData = mkSharedData nameOf decl
  case decl of
    DefRecord {} -> return ()
    DefAbstract _ _ sort _ -> case sort of
      NetworkDef -> tell [Network $ NetworkSummary sharedData]
      DatasetDef -> tell [Dataset $ DatasetSummary sharedData]
      ParameterDef s -> tell [Parameter $ ParameterSummary sharedData (isInferable s)]
      PostulateDef -> return ()
    DefFunction _ _ anns _ body
      | AnnProperty `notElem` anns -> return ()
      | otherwise -> do
          quantifiers <- runFreshNameContextT $ execWriterT $ searchValue body
          tell [Property $ PropertySummary sharedData quantifiers]

type MonadListBody m =
  ( MonadNameContext m,
    MonadWriter [QuantifiedVariableSummary] m,
    MonadFreeContext Builtin m
  )

-- | Traverse the body to find all quantified variables
searchValue :: (MonadListBody m) => Value Builtin -> m ()
searchValue = \case
  VBoundVar _ spine -> searchSpine spine
  VFreeVar _ spine -> searchSpine spine
  VBuiltin b spine -> do
    case (b, getExpr accessSpine spine) of
      (BuiltinFunction (QuantifyRatTensor q), Just (QuantifyRatTensorArgs _dims (VLam binder _))) -> do
        tell [QuantifiedVariableSummary (mkSharedData getBinderName binder) q]
      _ -> return ()
    searchSpine spine
  VLam binder closure -> do
    nameCtx <- getNameContext
    value <- normaliseClosure nameCtx binder closure
    searchValue value
  VRecord _ fields -> traverse_ searchValue fields
  VRecordAcc record _ -> searchValue record
  -- Never traverse into types so the following cases shouldn't happen!
  VUniverse {} -> unexpectedExprError pass "VUniverse"
  VPi {} -> unexpectedExprError pass "VUniverse"
  VMeta {} -> unexpectedExprError pass "VMeta"
  where
    pass = "list"

searchSpine :: (MonadListBody m) => Spine Builtin -> m ()
searchSpine = traverse_ (traverse_ searchValue)

--------------------------------------------------------------------------------
-- JSON output format
--------------------------------------------------------------------------------

data ListableEntity
  = Network NetworkSummary
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
  ( HasProvenance entity,
    HasType entity (Value Builtin)
  ) =>
  (entity -> Name) ->
  entity ->
  SharedData
mkSharedData getName entity =
  SharedData
    { name = getName entity,
      typeText = pack $ show $ prettyFriendlyEmptyCtx (typeOf entity),
      provenance = provenanceOf entity
    }

--------------------------------------------------------------------------------
-- Network

newtype NetworkSummary = NetworkSummary
  { sharedData :: SharedData
  }
  deriving (Generic)

instance ToJSON NetworkSummary

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
    quantifiedVariables :: [QuantifiedVariableSummary]
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
