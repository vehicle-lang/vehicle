{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Vehicle.LSP (
    main,
) where

import Colog qualified
import Colog.Core (LogAction, Severity (..), WithSeverity (..), (<&))
import Control.Applicative ((<**>))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (
    TChan,
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
import Language.LSP.Protocol.Message (
    MessageDirection (..),
    MessageKind (..),
    Method (Method_Initialize),
    TMessage,
    TResponseError,
 )
import Language.LSP.Protocol.Types (ClientCapabilities, SaveOptions (..), ServerInfo (..), TextDocumentSyncKind (..), TextDocumentSyncOptions (..), type (|?) (InR))
import Language.LSP.Server (
    Handler,
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
import Options.Applicative (Parser, ParserInfo, execParser, help, helper, info, infoOption, long, metavar, optional, progDesc, short, strOption)
import Paths_vehicle_lsp (version)
import Prettyprinter (Pretty (..))
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)
import System.IO (BufferMode (..), Handle, IOMode (..), hSetBuffering, stderr, stdin, stdout, withFile)
import Vehicle.LSP.Internal.Config (Config, LspTc)
import Vehicle.LSP.Internal.Config qualified as Config
import Vehicle.LSP.Internal.Handlers (handlers)

--------------------------------------------------------------------------------

{-# ANN module ("HLint: ignore Redundant lambda" :: String) #-}

--------------------------------------------------------------------------------

main :: IO ()
main =
    run >>= \case
        0 -> exitSuccess
        c -> exitWith (ExitFailure c)

run :: IO Int
run = do
    CliOptions{..} <- execParser cliOptionInfo
    withLogHandle maybeLogFile $ \logHandle -> do
        let
            -- Setup loggers:
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
        -- Start the LSP server:
        runServerWithHandles handleLogger (dualLogger @(LspM Config)) stdin stdout $
            lspDefinition handleLogger (dualLogger @LspTc) reactorInputChan

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

newtype CliOptions = CliOptions
    { maybeLogFile :: Maybe FilePath
    }

cliOptionInfo :: ParserInfo CliOptions
cliOptionInfo =
    info
        cliOptionParser
        (progDesc "The Vehicle Language Server")

cliOptionParser :: Parser CliOptions
cliOptionParser =
    CliOptions
        <$> logFileOption
        <**> versionOption
        <**> numericVersionOption
        <**> helper
  where
    logFileOption =
        optional . strOption . mconcat $
            [ long "log-file"
            , metavar "FILE"
            , help "Log file to use while LSP logging is unavailable."
            ]
    versionString = "vehicle-lsp version " <> numericVersionString
    versionOption =
        infoOption versionString . mconcat $
            [ long "version"
            , short 'V'
            , help "Print the version information and exit."
            ]
    numericVersionString = showVersion version
    numericVersionOption =
        infoOption numericVersionString . mconcat $
            [ long "numeric-version"
            , help "Print the numeric version only and exit."
            ]

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
    TChan ReactorInput ->
    ServerDefinition Config
lspDefinition handleLogger dualLogger reactorInputChan =
    ServerDefinition
        { defaultConfig = Config.defaultConfig
        , configSection = "vehicle"
        , parseConfig = Config.parseConfig
        , onConfigChange = Config.onConfigChange
        , doInitialize = lspInitialise handleLogger reactorInputChan
        , staticHandlers = lspHandlers dualLogger reactorInputChan
        , interpretHandler = lspInterpretHandler
        , options = lspOptions
        }

{- | The single point that all events flow through, allowing management of
  state to stitch replies and requests together from the two asynchronous
  sides: lsp server and backend compiler.
-}
reactor ::
    LogAction IO (WithSeverity Text) ->
    TChan ReactorInput ->
    IO ()
reactor logger reactorInputChan = do
    logger <& "Reactor started" `WithSeverity` Info
    forever (runReactorAction =<< atomically (readTChan reactorInputChan))

lspInitialise ::
    LogAction IO (WithSeverity Text) ->
    TChan ReactorInput ->
    LanguageContextEnv Config ->
    TMessage 'Method_Initialize ->
    IO (Either (TResponseError 'Method_Initialize) (LanguageContextEnv Config))
lspInitialise logger reactorInputChan languageContextEnv _request = do
    _reactorId <- forkIO (reactor logger reactorInputChan)
    pure $ Right languageContextEnv

lspHandlers ::
    LogAction LspTc (WithSeverity Text) ->
    TChan ReactorInput ->
    ClientCapabilities ->
    Handlers LspTc
lspHandlers logger reactorInputChan =
    mapHandlers pushRequest pushNotification . handlers logger
  where
    pushRequest :: forall (a :: Method 'ClientToServer 'Request). Handler LspTc a -> Handler LspTc a
    pushRequest handler = \message responder -> do
        lspEnv <- getLspEnv
        let !action = Config.runLspTc lspEnv (handler message responder)
        liftIO . atomically . writeTChan reactorInputChan $ ReactorAction action

    pushNotification :: forall (a :: Method 'ClientToServer 'Notification). Handler LspTc a -> Handler LspTc a
    pushNotification handler = \message -> do
        lspEnv <- getLspEnv
        let !action = Config.runLspTc lspEnv (handler message)
        liftIO . atomically . writeTChan reactorInputChan $ ReactorAction action

lspInterpretHandler ::
    LanguageContextEnv Config ->
    LspTc <~> IO
lspInterpretHandler languageContextEnv =
    Iso (Config.runLspTc languageContextEnv) liftIO

lspOptions :: Options
lspOptions =
    defaultOptions
        { optServerInfo =
            Just
                ServerInfo
                    { _name = Config.packageName
                    , _version = Just . T.pack $ showVersion version
                    }
        , optTextDocumentSync = Just textDocumentSyncOptions
        }

textDocumentSyncOptions :: TextDocumentSyncOptions
textDocumentSyncOptions =
    TextDocumentSyncOptions
        { _openClose = Just False
        , _change = Just TextDocumentSyncKind_None
        , _willSave = Just False
        , _willSaveWaitUntil = Just False
        , _save = Just . InR . SaveOptions $ Just True
        }
