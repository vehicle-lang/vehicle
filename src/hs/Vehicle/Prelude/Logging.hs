{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Vehicle.Prelude.Logging
  ( LoggingOptions(..)
  , Severity
  , DebugLevel(..)
  , Message(..)
  , MonadLogger(incrCallDepth, decrCallDepth, logMessage)
  , LoggerT
  , Logger
  , runLogger
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

import Control.Monad (when, forM_)
import Control.Monad.State (StateT(..), get, modify, evalStateT)
import Control.Monad.Writer (WriterT(..), tell, runWriterT)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Except (MonadError(..), Except, ExceptT, mapExceptT)
import Control.Monad.Reader (ReaderT(..), ask)
import Control.Monad.Trans ( MonadIO(..), MonadTrans(..) )
import Data.Text (Text)
import Data.Text qualified as T
import System.Console.ANSI
import System.IO ( Handle, hPrint )

import Vehicle.Prelude.Prettyprinter
import Vehicle.Prelude.Supply (SupplyT)

data Severity
  = Debug
  | Warning
  deriving (Eq, Ord)

data DebugLevel
  = MinDetail
  | MaxDetail
  deriving (Eq, Ord, Show)

data LoggingOptions = LoggingOptions
  { errorHandle  :: Handle
  , outputHandle :: Handle
  , logHandle    :: Maybe Handle
  , debugLevel   :: DebugLevel
  }

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
  getDebugLevel :: m DebugLevel
  logMessage    :: Message -> m ()

newtype LoggerT m a = LoggerT
  { unloggerT :: ReaderT DebugLevel (WriterT [Message] (StateT Int m)) a
  } deriving (Functor, Applicative, Monad)

type Logger = LoggerT Identity

instance Monad m => MonadLogger (LoggerT m) where
  getCallDepth  = LoggerT get
  incrCallDepth = LoggerT $ modify (+1)
  decrCallDepth = LoggerT $ modify (\x -> x-1)
  getDebugLevel = LoggerT ask
  logMessage m  = LoggerT $ tell [m]

instance MonadLogger m => MonadLogger (StateT s m) where
  getCallDepth  = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage    = lift . logMessage

instance MonadLogger m => MonadLogger (ReaderT s m) where
  getCallDepth  = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage    = lift . logMessage

instance (Monoid w, MonadLogger m) => MonadLogger (WriterT w m) where
  getCallDepth  = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage    = lift . logMessage

instance (MonadLogger m) => MonadLogger (ExceptT e m) where
  getCallDepth  = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage    = lift . logMessage

instance MonadLogger m => MonadLogger (SupplyT s m) where
  getCallDepth  = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage    = lift . logMessage

instance MonadTrans LoggerT where
  lift = LoggerT . lift . lift . lift

instance MonadError e m => MonadError e (LoggerT m) where
  throwError     = lift . throwError
  catchError m f = LoggerT (catchError (unloggerT m) (unloggerT . f))

instance MonadIO m => MonadIO (LoggerT m) where
  liftIO = lift . liftIO

runLoggerT :: Monad m => DebugLevel -> LoggerT m a -> m (a, [Message])
runLoggerT debugLevel (LoggerT logger) =
  evalStateT (runWriterT (runReaderT logger debugLevel)) 0

runLogger :: DebugLevel -> Logger a -> (a, [Message])
runLogger debugLevel = runIdentity . runLoggerT debugLevel

logWarning :: MonadLogger m => Doc a -> m ()
logWarning text = logMessage $ Message Warning (layoutAsText text)

logDebug :: MonadLogger m => DebugLevel -> Doc a -> m ()
logDebug level text = do
  depth <- getCallDepth
  debugLevel <- getDebugLevel
  when (level <= debugLevel) $
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
discardLoggerT m = fst <$> runLoggerT MinDetail m

discardLogger :: Logger a -> a
discardLogger m = fst $ runLogger MinDetail m

flushLogger :: MonadIO m => DebugLevel -> Handle -> LoggerT m a -> m a
flushLogger debugLevel logHandle logger = do
  (v, messages) <- runLoggerT debugLevel logger
  flushLogs logHandle messages
  return v

flushLogs :: MonadIO m => Handle -> [Message] -> m ()
flushLogs logHandle messages = liftIO $ mapM_ (hPrint logHandle) messages

fromLoggedIO :: MonadIO m => LoggingOptions -> LoggerT m a -> m a
fromLoggedIO LoggingOptions{..} logger = case logHandle of
  Nothing       -> discardLoggerT logger
  (Just handle) -> flushLogger debugLevel handle logger

fromLoggerTIO :: LoggingOptions -> LoggerT IO a -> IO a
fromLoggerTIO options@LoggingOptions{..} logger = do
  (v, messages) <- runLoggerT debugLevel logger
  fromLoggedIO options $ do
    forM_ messages logMessage
    return v