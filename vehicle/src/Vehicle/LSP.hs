{-# LANGUAGE BangPatterns #-}

module Vehicle.LSP
  ( LSPOptions (..),
    runLSP,
  )
where

import Colog qualified
import Colog.Core (LogAction, Severity (..), WithSeverity (..), (<&))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, newTVarIO)
import Control.Concurrent.STM.TChan
  ( TChan,
    newTChanIO,
    readTChan,
    writeTChan,
  )
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Version (showVersion)
import Language.LSP.Logging (defaultClientLogger)
import Language.LSP.Protocol.Message
  ( MessageDirection (..),
    MessageKind (..),
    Method (Method_Initialize),
    TMessage,
    TResponseError,
  )
import Language.LSP.Protocol.Types (ClientCapabilities, SaveOptions (..), ServerInfo (..), TextDocumentSyncKind (..), TextDocumentSyncOptions (..), type (|?) (InR))
import Language.LSP.Server
  ( Handler,
    Handlers,
    LanguageContextEnv,
    LspM,
    MonadLsp (..),
    Options (optServerInfo, optTextDocumentSync),
    ServerDefinition (..),
    defaultOptions,
    mapHandlers,
    runServerWithHandles,
    type (<~>) (..),
  )
import Paths_vehicle (version)
import Prettyprinter (Pretty (..))
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)
import System.IO (BufferMode (..), Handle, IOMode (..), hSetBuffering, stderr, stdin, stdout, withFile)
import Vehicle.LSP.Config (Config)
import Vehicle.LSP.Config qualified as Config
import Vehicle.LSP.Handlers (handlers)
import Vehicle.LSP.Monad as LSPMonad
import Vehicle.LSP.State (ServerState, ServerStateRef, initialServerState, initialiseServerState)

--------------------------------------------------------------------------------

runLSP :: LSPOptions -> IO ()
runLSP LSPOptions {..} = do
  result <- withLogHandle maybeLogFile $ \logHandle -> do
    let -- Setup loggers:
        -- logs to 'logHandle' (either 'stderr' or 'logFile')
        handleLogger :: (Pretty a) => LogAction IO (WithSeverity a)
        handleLogger = Colog.cmap (addSeverity . fmap prettyText) (Colog.logTextHandle logHandle)
        -- logs to LSP client
        clientLogger :: (MonadLsp Config m, Pretty a) => LogAction m (WithSeverity a)
        clientLogger = Colog.cmap (fmap prettyText) defaultClientLogger
        -- logs to both
        dualLogger :: (MonadLsp Config m, Pretty a) => LogAction m (WithSeverity a)
        dualLogger = clientLogger <> Colog.hoistLogAction liftIO handleLogger

    -- Setup global queue with LSP message reactions:
    reactorInputChan <- newTChanIO

    -- Initialise the global state
    stateVar <- liftIO $ newTVarIO initialServerState

    -- Start the LSP server:
    runServerWithHandles handleLogger (dualLogger @(LspM Config)) stdin stdout $
      lspDefinition handleLogger (dualLogger @LspTc) stateVar reactorInputChan

  case result of
    0 -> exitSuccess
    c -> exitWith (ExitFailure c)

--------------------------------------------------------------------------------
-- Logger Helpers
--------------------------------------------------------------------------------

prettyText :: (Pretty t) => t -> Text
prettyText = T.pack . show . pretty

addSeverity :: WithSeverity Text -> Text
addSeverity l = mconcat ["[", T.pack . show $ Colog.getSeverity l, "] ", Colog.getMsg l]

withLogHandle :: Maybe FilePath -> (Handle -> IO a) -> IO a
withLogHandle maybeLogFile action = case maybeLogFile of
  Nothing -> action stderr
  Just logFile -> withFile logFile AppendMode $ \handle ->
    hSetBuffering handle NoBuffering >> action handle

--------------------------------------------------------------------------------
-- Command-Line Options
--------------------------------------------------------------------------------

newtype LSPOptions = LSPOptions
  { maybeLogFile :: Maybe FilePath
  }
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Reactor-Style Language Server
--------------------------------------------------------------------------------

-- NOTE: LSP follows Reactor-Style. See the following for details:
--       https://github.com/haskell/lsp/blob/master/lsp/example/Reactor.hs

newtype ReactorInput
  = ReactorAction {runReactorAction :: IO ()}

lspDefinition ::
  LogAction IO (WithSeverity Text) ->
  LogAction LspTc (WithSeverity Text) ->
  ServerStateRef ->
  TChan ReactorInput ->
  ServerDefinition Config
lspDefinition handleLogger dualLogger serverState reactorInputChan = do
  ServerDefinition
    { defaultConfig = Config.defaultConfig,
      configSection = "vehicle",
      parseConfig = Config.parseConfig,
      onConfigChange = LSPMonad.onConfigChange,
      doInitialize = lspInitialise handleLogger serverState reactorInputChan,
      staticHandlers = lspHandlers dualLogger serverState reactorInputChan,
      interpretHandler = lspInterpretHandler,
      options = lspOptions
    }

-- | The single point that all events flow through, allowing management of
--  state to stitch replies and requests together from the two asynchronous
--  sides: lsp server and backend compiler.
reactor ::
  LogAction IO (WithSeverity Text) ->
  TChan ReactorInput ->
  IO ()
reactor logger reactorInputChan = do
  logger <& "Reactor started" `WithSeverity` Info
  forever (runReactorAction =<< atomically (readTChan reactorInputChan))

lspInitialise ::
  LogAction IO (WithSeverity Text) ->
  ServerStateRef ->
  TChan ReactorInput ->
  LanguageContextEnv Config ->
  TMessage 'Method_Initialize ->
  IO (Either (TResponseError 'Method_Initialize) (LanguageContextEnv Config))
lspInitialise logger serverState reactorInputChan languageContextEnv _request = do
  _reactorId <- forkIO (reactor logger reactorInputChan)
  maybeError <- initialiseServerState serverState
  pure $ Right languageContextEnv

lspHandlers ::
  LogAction LspTc (WithSeverity Text) ->
  ServerStateRef ->
  TChan ReactorInput ->
  ClientCapabilities ->
  Handlers LspTc
lspHandlers logger serverState reactorInputChan =
  mapHandlers pushRequest pushNotification . handlers logger serverState
  where
    pushRequest :: forall (a :: Method 'ClientToServer 'Request). Handler LspTc a -> Handler LspTc a
    pushRequest handler message responder = do
      lspEnv <- getLspEnv
      let !action = runLspTc lspEnv (handler message responder)
      liftIO . atomically . writeTChan reactorInputChan $ ReactorAction action

    pushNotification :: forall (a :: Method 'ClientToServer 'Notification). Handler LspTc a -> Handler LspTc a
    pushNotification handler message = do
      lspEnv <- getLspEnv
      let !action = runLspTc lspEnv (handler message)
      liftIO . atomically . writeTChan reactorInputChan $ ReactorAction action

lspInterpretHandler ::
  LanguageContextEnv Config ->
  LspTc <~> IO
lspInterpretHandler languageContextEnv =
  Iso (runLspTc languageContextEnv) liftIO

lspOptions :: Options
lspOptions =
  defaultOptions
    { optServerInfo =
        Just
          ServerInfo
            { _name = Config.packageName,
              _version = Just . T.pack $ showVersion version
            },
      optTextDocumentSync = Just textDocumentSyncOptions
    }

textDocumentSyncOptions :: TextDocumentSyncOptions
textDocumentSyncOptions =
  TextDocumentSyncOptions
    { _openClose = Just True,
      _change = Just TextDocumentSyncKind_Full,
      _willSave = Just False,
      _willSaveWaitUntil = Just False,
      _save = Just . InR . SaveOptions $ Just False
    }
