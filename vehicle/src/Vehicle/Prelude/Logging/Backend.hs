{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Vehicle.Prelude.Logging.Backend
  ( MonadLoggingBackend (..),
    ImmediateBackendT,
    runImmediateBackendT,
    SilentBackendT,
    runSilentBackendT,
    DelayedBackendT,
    runDelayedBackendT,
    showMessages,
  )
where

import Control.Monad (unless)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (ReaderT (..), ask)
import Control.Monad.State (MonadState (..), StateT (..), evalStateT)
import Control.Monad.Trans (MonadIO (..), MonadTrans (..))
import Control.Monad.Writer (MonadWriter (..), WriterT (..))
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Text (Text)
import Data.Text qualified as Text (pack, unpack)
import Prettyprinter (Pretty (..), line, (<+>))
import System.Console.ANSI
import Vehicle.Compile.Print.Warning ()
import Vehicle.Prelude.IO as VIO (MonadStdIO (..))
import Vehicle.Prelude.Logging.Class
import Vehicle.Prelude.Misc (setTextColour)
import Vehicle.Prelude.Warning
import Vehicle.Syntax.Prelude

--------------------------------------------------------------------------------
-- Class for logging backends

class (Monad m) => MonadLoggingBackend m where
  output :: Message -> m ()

instance (MonadLoggingBackend m) => MonadLoggingBackend (StateT s m) where
  output = lift . output

instance (MonadLoggingBackend m) => MonadLoggingBackend (ReaderT s m) where
  output = lift . output

--------------------------------------------------------------------------------
-- Immediate backend

type LogAction = Text -> IO ()

newtype ImmediateBackendT m a = ImmediateBackendT
  { unImmediateBackendT :: StateT IntSet (ReaderT LogAction m) a
  }
  deriving (Functor, Applicative, Monad)

instance (MonadIO m) => MonadLoggingBackend (ImmediateBackendT m) where
  output message = ImmediateBackendT $ do
    logAction <- ask
    isDuplicateWarning <- isAlreadySeenWarning message
    unless isDuplicateWarning $ do
      lift $ liftIO $ logAction (Text.pack $ showMessage message)

instance MonadTrans ImmediateBackendT where
  lift = ImmediateBackendT . lift . lift

instance (MonadIO m) => MonadIO (ImmediateBackendT m) where
  liftIO = lift . liftIO

instance (MonadStdIO m) => MonadStdIO (ImmediateBackendT m) where
  writeStdout = lift . VIO.writeStdout
  writeStderr = lift . VIO.writeStderr
  writeStdoutLn = lift . VIO.writeStdoutLn
  writeStderrLn = lift . VIO.writeStderrLn

runImmediateBackendT :: (MonadIO m) => (Text -> IO ()) -> ImmediateBackendT m a -> m a
runImmediateBackendT putLogLn v =
  runReaderT (evalStateT (unImmediateBackendT v) mempty) putLogLn

--------------------------------------------------------------------------------
-- Silent backend

newtype SilentBackendT m a = SilentBackendT
  { unSilentBackendT :: m a
  }
  deriving (Functor, Applicative, Monad)

instance (Monad m) => MonadLoggingBackend (SilentBackendT m) where
  output _message = return ()

instance MonadTrans SilentBackendT where
  lift = SilentBackendT

instance (MonadIO m) => MonadIO (SilentBackendT m) where
  liftIO = lift . liftIO

instance (MonadStdIO m) => MonadStdIO (SilentBackendT m) where
  writeStdout = lift . VIO.writeStdout
  writeStderr = lift . VIO.writeStderr
  writeStdoutLn = lift . VIO.writeStdoutLn
  writeStderrLn = lift . VIO.writeStderrLn

instance (MonadError e m) => MonadError e (SilentBackendT m) where
  throwError = lift . throwError
  catchError m f = SilentBackendT (catchError (unSilentBackendT m) (unSilentBackendT . f))

runSilentBackendT :: SilentBackendT m a -> m a
runSilentBackendT = unSilentBackendT

--------------------------------------------------------------------------------
-- Delayed backend

newtype DelayedBackendT m a = DelayedBackendT
  { unDelayedBackendT :: StateT IntSet (WriterT [Message] m) a
  }
  deriving (Functor, Applicative, Monad)

instance (Monad m) => MonadLoggingBackend (DelayedBackendT m) where
  output message = DelayedBackendT $ do
    isDuplicateWarning <- isAlreadySeenWarning message
    unless isDuplicateWarning $ do
      tell [message]

instance MonadTrans DelayedBackendT where
  lift = DelayedBackendT . lift . lift

instance (MonadIO m) => MonadIO (DelayedBackendT m) where
  liftIO = lift . liftIO

instance (MonadStdIO m) => MonadStdIO (DelayedBackendT m) where
  writeStdout = lift . VIO.writeStdout
  writeStderr = lift . VIO.writeStderr
  writeStdoutLn = lift . VIO.writeStdoutLn
  writeStderrLn = lift . VIO.writeStderrLn

runDelayedBackendT :: (Monad m) => DelayedBackendT m a -> m (a, [Message])
runDelayedBackendT v = runWriterT (evalStateT (unDelayedBackendT v) mempty)

--------------------------------------------------------------------------------
-- Duplicate detection

isAlreadySeenWarning :: (MonadState IntSet m) => Message -> m Bool
isAlreadySeenWarning = \case
  DebugMessage {} -> return False
  WarningMessage w -> do
    case warningDuplicateDetectionHash w of
      Nothing -> return False
      Just value -> do
        seenWarnings <- get
        if value `IntSet.member` seenWarnings
          then return True
          else do
            put (IntSet.insert value seenWarnings)
            return False

--------------------------------------------------------------------------------
-- Formatting

showMessage :: Message -> String
showMessage = \case
  DebugMessage t ->
    setTextColour Green $ Text.unpack t
  WarningMessage w ->
    setTextColour Yellow $ layoutAsString (line <> "Warning: " <+> pretty w <> line)

showMessages :: [Message] -> String
showMessages logs = unlines $ map showMessage logs
