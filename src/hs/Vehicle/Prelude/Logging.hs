{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Vehicle.Prelude.Logging
  ( LoggingOptions(..)
  , Severity
  , Message(..)
  , MonadLogger(incrCallDepth, decrCallDepth, logMessage)
  , LoggerT
  , Logger
  , runLoggerT
  , runLogger
  , discardLoggerT
  , discardLogger
  , logWarning
  , logDebug
  , liftExceptWithLogging
  , showMessages
  , setTextColour
  , setBackgroundColour
  , fromLoggedIO
  , fromLoggerTIO
  ) where

import Control.Monad.State (StateT(..), get, modify, evalStateT)
import Control.Monad.Writer (WriterT (WriterT), tell, runWriterT)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Except (MonadError(..), Except, ExceptT, mapExceptT)
import Control.Monad.Reader (ReaderT)
import Data.Text (Text)
import Data.Text qualified as T
import System.Console.ANSI
import System.IO

import Vehicle.Prelude.Prettyprinter
import Vehicle.Prelude.Supply (SupplyT)
import Control.Monad (forM_)
import Control.Monad.Trans

data LoggingOptions = LoggingOptions
  { errorHandle  :: Handle
  , outputHandle :: Handle
  , logHandle    :: Maybe Handle
  }

data Severity
  = Debug
  | Warning
  deriving (Eq, Ord)

setTextColour :: Color -> String -> String
setTextColour c s =
  setSGRCode [SetColor Foreground Vivid c] <>
  s <>
  setSGRCode [SetColor Foreground Vivid White]

setBackgroundColour :: Color -> String -> String
setBackgroundColour c s =
  setSGRCode [SetColor Background Vivid c] <>
  s <>
  setSGRCode [SetColor Background Vivid Black]

severityColour :: Severity -> Maybe Color
severityColour = \case
  Warning        -> Just Yellow
  Debug          -> Just Green

severityPrefix :: Severity -> Text
severityPrefix Warning        = "Warning: "
severityPrefix Debug          = ""

type CallDepth = Int

data Message = Message
  { severityOf :: Severity
  , textOf     :: Text
  }

class Monad m => MonadLogger m where
  getCallDepth  :: m CallDepth
  incrCallDepth :: m ()
  decrCallDepth :: m ()
  logMessage    :: Message -> m ()

newtype LoggerT m a = LoggerT
  { unloggerT :: WriterT [Message] (StateT Int m) a
  } deriving (Functor, Applicative, Monad)

type Logger = LoggerT Identity

instance Monad m => MonadLogger (LoggerT m) where
  getCallDepth  = LoggerT get
  incrCallDepth = LoggerT $ modify (+1)
  decrCallDepth = LoggerT $ modify (\x -> x-1)
  logMessage m  = LoggerT $ tell [m]

instance MonadLogger m => MonadLogger (StateT s m) where
  getCallDepth  = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  logMessage    = lift . logMessage

instance MonadLogger m => MonadLogger (ReaderT s m) where
  getCallDepth  = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  logMessage    = lift . logMessage

instance (Monoid w, MonadLogger m) => MonadLogger (WriterT w m) where
  getCallDepth  = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  logMessage    = lift . logMessage

instance (MonadLogger m) => MonadLogger (ExceptT e m) where
  getCallDepth  = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  logMessage    = lift . logMessage

instance MonadLogger m => MonadLogger (SupplyT s m) where
  getCallDepth  = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  logMessage    = lift . logMessage

instance MonadTrans LoggerT where
  lift = LoggerT . lift . lift

instance MonadError e m => MonadError e (LoggerT m) where
  throwError     = lift . throwError
  catchError m f = LoggerT (catchError (unloggerT m) (unloggerT . f))

instance MonadIO m => MonadIO (LoggerT m) where
  liftIO = lift . liftIO

runLoggerT :: Monad m => LoggerT m a -> m (a, [Message])
runLoggerT (LoggerT logger) = evalStateT (runWriterT logger) 0

runLogger :: Logger a -> (a, [Message])
runLogger = runIdentity . runLoggerT

logWarning :: MonadLogger m => Doc a -> m ()
logWarning text = logMessage $ Message Warning (layoutAsText text)

logDebug :: MonadLogger m => Doc a -> m ()
logDebug text = do
  depth <- getCallDepth
  logMessage $ Message Debug (layoutAsText (indent depth text))

instance Show Message where
  show (Message s t) =
    let txt = T.unpack (severityPrefix s <> t) in
    maybe txt (`setTextColour` txt) (severityColour s)

showMessages :: [Message] -> String
showMessages logs = unlines $ map show logs

liftExceptWithLogging :: Except e v -> ExceptT e Logger v
liftExceptWithLogging = mapExceptT (pure . runIdentity)

discardLoggerT :: Monad m => LoggerT m a -> m a
discardLoggerT m = fst <$> runLoggerT m

discardLogger :: Logger a -> a
discardLogger m = fst $ runLogger m

flushLogger :: MonadIO m => Handle -> LoggerT m a -> m a
flushLogger logHandle l = do
  (v, messages) <- runLoggerT l
  flushLogs logHandle messages
  return v

flushLogs :: MonadIO m => Handle -> [Message] -> m ()
flushLogs logHandle messages = liftIO $ mapM_ (hPrint logHandle) messages

fromLoggedIO :: MonadIO m => LoggingOptions -> LoggerT m a -> m a
fromLoggedIO LoggingOptions{..} logger = case logHandle of
  Nothing       -> discardLoggerT logger
  (Just handle) -> flushLogger handle logger

fromLoggerTIO :: LoggingOptions -> LoggerT IO a -> IO a
fromLoggerTIO loggingOptions logger = do
  (v, messages) <- runLoggerT logger
  fromLoggedIO loggingOptions $ do
    forM_ messages logMessage
    return v