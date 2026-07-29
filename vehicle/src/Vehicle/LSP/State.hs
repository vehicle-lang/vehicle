module Vehicle.LSP.State
  ( Server (..),
    newServer,
    initialiseServer,
    fileUpdated,
  )
where

import Control.Concurrent.STM
import Control.Monad (forever, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text, unpack)
import GHC.Conc (forkIO)
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Vehicle.Compile.Error (CompileError (..), ParseError (..), VehicleError, provenance)
import Vehicle.Compile.Prelude (ModulePath, MonadStdIO, Prog, userModulePath)
import Vehicle.Compile.Print.Error
import Vehicle.Data.AST.Provenance qualified as P
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.LSP.Config (Config)
import Vehicle.Prelude.Error
import Vehicle.Prelude.Logging (runSilentLoggerT)
import Vehicle.Prelude.Prettyprinter
import Vehicle.TypeCheck (TypeCheckOptions (..), typeCheckUserProg)

data Server = Server
  { stateRef :: TVar ServerState,
    jobQueue :: JobQueue,
    resultQueue :: ResultQueue
  }

-----------------
-- ServerState --
-----------------

data ServerState = ServerState
  { sourceFiles :: Map NormalizedUri FileState,
    availableModules :: Map ModulePath FilePath
  }

initialServerState :: (MonadIO m) => m ServerState
initialServerState = do
  return $
    ServerState
      { sourceFiles = mempty,
        availableModules = mempty
      }

newServer :: (MonadIO m) => m Server
newServer = do
  initialState <- initialServerState
  stateRef <- liftIO $ newTVarIO initialState
  jobQueue <- liftIO newTQueueIO
  resultQueue <- liftIO newTQueueIO
  return $ Server stateRef jobQueue resultQueue

initialiseServer :: (MonadStdIO IO) => Server -> LanguageContextEnv Config -> IO ()
initialiseServer server@Server {..} env = do
  _ <- liftIO $ forkIO $ jobWorker server
  _ <- liftIO $ forkIO $ runLspT env $ resultWorker resultQueue
  return ()

type FileVersion = Int32

data FileState = FileState
  { fileVersion :: FileVersion,
    fileSource :: Text,
    fileResult :: Maybe (Either CompileError (Prog Builtin))
  }

fileUpdated ::
  (MonadIO m) =>
  Server ->
  FileVersion ->
  NormalizedUri ->
  Text ->
  m ()
fileUpdated Server {..} ver uri txt =
  liftIO $ atomically $ do
    modifyTVar' stateRef $ \serverState@ServerState {..} -> do
      let newFileState =
            FileState
              { fileVersion = ver,
                fileSource = txt,
                fileResult = Nothing
              }
      serverState
        { sourceFiles = Map.insert uri newFileState sourceFiles
        }

    writeTQueue jobQueue (Job ver uri txt)

----------
-- Jobs --
----------

data Job = Job FileVersion NormalizedUri Text

type JobQueue = TQueue Job

lspTypeCheck :: (MonadStdIO IO) => NormalizedUri -> Text -> IO (Either CompileError (Prog Builtin))
lspTypeCheck uri _txt = do
  case maybeFilePath of
    Nothing ->
      pure $
        Left $
          ParseError
            (userModulePath, "") -- TODO: what can we do if the LSP is handed a bad file path?
            (RawParseError ("invalid file path! " <> unpack (getUri rawUri)))
    Just filePath -> do
      (result, _warnings) <-
        runSilentLoggerT
          ( logCompileError
              ( typeCheckUserProg
                  TypeCheckOptions
                    { specification = filePath,
                      secondaryTypeSystem = Nothing, -- TODO: run all type systems
                      declarationsToCompile = []
                    }
              )
          )
      pure result
  where
    rawUri = fromNormalizedUri uri
    maybeFilePath = uriToFilePath rawUri

jobWorker :: (MonadStdIO IO) => Server -> IO ()
jobWorker (Server stateVar jobQueue resultQueue) = forever $ do
  Job version uri txt <- atomically $ readTQueue jobQueue

  result <- lspTypeCheck uri txt

  atomically $ do
    oldState@ServerState {..} <- readTVar stateVar
    oldFileState <- case Map.lookup uri sourceFiles of
      Nothing -> developerError "Missing file"
      Just oldFileState -> return oldFileState

    when (fileVersion oldFileState == version) $ do
      let newFileState =
            oldFileState
              { fileResult = Just result
              }
      writeTVar stateVar $
        oldState
          { sourceFiles = Map.insert uri newFileState sourceFiles
          }

      case result of
        Right {} -> return ()
        Left err ->
          writeTQueue resultQueue $
            Result
              { version = version,
                uri = uri,
                typeError = formatCompileError err
              }

-------------
-- Results --
-------------

data Result = Result
  { version :: Int32,
    uri :: NormalizedUri,
    typeError :: VehicleError
  }

type ResultQueue = TQueue Result

resultWorker :: (MonadLsp Config m) => ResultQueue -> m ()
resultWorker resultQueue = forever $ do
  Result {..} <- liftIO $ atomically $ readTQueue resultQueue

  publishDiagnostics
    100
    uri
    (Just version)
    [(Nothing, [errorDiagnostic typeError])]

errorDiagnostic :: VehicleError -> Diagnostic
errorDiagnostic err =
  Diagnostic
    { _range = maybe defaultRange v2lRange (P.range <$> provenance err),
      _severity = Just DiagnosticSeverity_Error,
      _code = Nothing,
      _codeDescription = Nothing,
      _source = Nothing,
      _message = layoutAsText $ pretty err,
      _tags = Nothing,
      _relatedInformation = Nothing,
      _data_ = Nothing
    }
  where
    defaultRange = Range (Position 0 0) (Position 0 0)

    v2lRange :: P.Range -> Range
    v2lRange r = Range (v2lPosition $ P.start r) (v2lPosition $ P.end r)

    v2lPosition :: P.Position -> Position
    v2lPosition p = Position (v2lInt $ P.posLine p) (v2lInt $ P.posColumn p)

    -- LSP uses 0-based indexing for lines and columns while our Position uses 1-based indexing.
    v2lInt :: Int -> UInt
    v2lInt = subtract 1 . fromIntegral
