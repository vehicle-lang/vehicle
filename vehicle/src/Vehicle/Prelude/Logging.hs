{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Vehicle.Prelude.Logging
  ( Severity
  , LoggingLevel(..)
  , Message(..)
  , MonadLogger(..)
  , LoggerT(..)
  , Logger
  , defaultLoggingLevel
  , mapLoggerT
  , discardLogger
  , discardLoggerT
  , logWarning
  , logDebug
  , liftExceptWithLogging
  , showMessages
  , setTextColour
  , setBackgroundColour
  , runLogger
  , runLoggerT
  , allLoggingLevels
  , loggingLevelAtLeast
  , loggingLevelHelp
  ) where

import Control.Monad (when)
import Control.Monad.Except (Except, ExceptT, MonadError (..), mapExceptT)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader (ReaderT (..), ask, mapReaderT)
import Control.Monad.State (StateT (..), evalStateT, get, mapStateT, modify)
import Control.Monad.Trans (MonadIO (..), MonadTrans (..))
import Control.Monad.Writer (WriterT (..), mapWriterT, runWriterT, tell)
import Data.Text (Text)
import Data.Text qualified as T
import System.Console.ANSI

import Vehicle.Prelude.Misc (enumerate, supportedOptions)
import Vehicle.Prelude.Prettyprinter
import Vehicle.Prelude.Supply (SupplyT)
import Vehicle.Syntax.Prelude (layoutAsText)

data Severity
  = Debug
  | Warning
  deriving (Eq, Ord)

data LoggingLevel
  = NoDetail
  | MinDetail
  | MidDetail
  | MaxDetail
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

allLoggingLevels :: [String]
allLoggingLevels = map show (enumerate @LoggingLevel)

loggingLevelHelp :: String
loggingLevelHelp =
  "Sets the level of detail in the logs if the --log argument has been passed. " <>
  supportedOptions allLoggingLevels

defaultLoggingLevel :: LoggingLevel
defaultLoggingLevel = NoDetail

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
  Warning -> Just Yellow
  Debug   -> Just Green

severityPrefix :: Severity -> Text
severityPrefix Warning = "Warning: "
severityPrefix Debug   = ""

type CallDepth = Int

data Message = Message
  { severityOf :: Severity
  , textOf     :: Text
  }

class Monad m => MonadLogger m where
  getCallDepth  :: m CallDepth
  incrCallDepth :: m ()
  decrCallDepth :: m ()
  getDebugLevel :: m LoggingLevel
  logMessage    :: Message -> m ()

newtype LoggerT m a = LoggerT
  { unloggerT :: ReaderT LoggingLevel (WriterT [Message] (StateT Int m)) a
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

runLoggerT :: Monad m => LoggingLevel -> LoggerT m a -> m (a, [Message])
runLoggerT debugLevel (LoggerT logger) =
  evalStateT (runWriterT (runReaderT logger debugLevel)) 0

mapLoggerT :: (m ((a, [Message]), Int) -> n ((b, [Message]), Int)) -> LoggerT m a -> LoggerT n b
mapLoggerT f l = LoggerT $ mapReaderT (mapWriterT (mapStateT f)) (unloggerT l)

runLogger :: LoggingLevel -> Logger a -> (a, [Message])
runLogger debugLevel = runIdentity . runLoggerT debugLevel

logWarning :: MonadLogger m => Doc a -> m ()
logWarning text = logMessage $ Message Warning (layoutAsText text)

logDebug :: MonadLogger m => LoggingLevel -> Doc a -> m ()
logDebug level text = do --traceShow text $ do
  debugLevel <- getDebugLevel
  when (level <= debugLevel) $ do
    depth <- getCallDepth
    logMessage $ Message Debug (layoutAsText (indent depth text))

loggingLevelAtLeast :: MonadLogger m => LoggingLevel -> m Bool
loggingLevelAtLeast level = do
  currentLevel <- getDebugLevel
  return $ currentLevel >= level

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
