

module Vehicle.Prelude.Logging
  ( Severity(..)
  , Message(..)
  , MonadLogger(incrCallDepth, decrCallDepth, logMessage)
  , LoggerT
  , Logger
  , runLoggerT
  , runLogger
  , logError
  , logWarning
  , logInfo
  , logDebug
  , liftExceptWithLogging
  , flushLogs
  ) where

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.State (StateT(..), get, modify, evalStateT)
import Control.Monad.Writer (WriterT, tell, runWriterT)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Except (Except, ExceptT, mapExceptT)
import Control.Monad.Reader (ReaderT)
import Data.Text (Text)
import Data.Text qualified as T
import System.Console.ANSI

data Severity
  = Error
  | Warning
  | Info
  | Debug

setColor :: Severity -> String
setColor Error   = setSGRCode [SetColor Foreground Vivid Red]
setColor Warning = setSGRCode [SetColor Foreground Vivid Yellow]
setColor Info    = setSGRCode [SetColor Foreground Vivid Blue]
setColor Debug   = setSGRCode [SetColor Foreground Vivid Green]

type CallDepth = Int

data Message = Message Severity Text

class Monad m => MonadLogger m where
  getCallDepth  :: m CallDepth
  incrCallDepth :: m ()
  decrCallDepth :: m ()
  logMessage    :: Message -> m ()

newtype LoggerT m a = LoggerT
  { unloggerT :: WriterT [Message] (StateT Int m) a
  }

type Logger = LoggerT Identity

instance Functor m => Functor (LoggerT m) where
  fmap f = LoggerT . fmap f . unloggerT

-- TODO why do I need `Monad` here and not `Applicative`???
instance Monad m => Applicative (LoggerT m) where
  pure x = LoggerT (pure x)
  f <*> x = LoggerT (unloggerT f <*> unloggerT x)

instance Monad m => Monad (LoggerT m) where
  x >>= f = LoggerT (unloggerT x >>= unloggerT . f)

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

instance MonadTrans LoggerT where
  lift = LoggerT . lift . lift
{-
instance MonadError e m => MonadError e (LoggerT m) where
  throwError e   = lift (throwError e)
  catchError e f = _
-}

runLoggerT :: Monad m => LoggerT m a -> m (a, [Message])
runLoggerT (LoggerT logger) = evalStateT (runWriterT logger) 0

runLogger :: Logger a -> (a, [Message])
runLogger = runIdentity . runLoggerT

logError :: MonadLogger m => Text -> m ()
logError text = logMessage $ Message Error text

logWarning :: MonadLogger m => Text -> m ()
logWarning text = logMessage $ Message Warning text

logInfo :: MonadLogger m => Text -> m ()
logInfo text = logMessage $ Message Info text

logDebug :: MonadLogger m => Text -> m ()
logDebug text = do
  depth <- getCallDepth
  logMessage $ Message Debug (T.replicate depth " " <> text)

resetColor :: String
resetColor = setSGRCode []

instance Show Message where
  show (Message s t) = setColor s <> T.unpack t <> resetColor

liftExceptWithLogging :: Except e v -> ExceptT e Logger v
liftExceptWithLogging = mapExceptT (pure . runIdentity)

flushLogs :: Logger a -> IO a
flushLogs l = do
  let (v, logs) = runLogger l
  mapM_ print logs
  return v