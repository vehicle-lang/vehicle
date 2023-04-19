module Vehicle.Prelude.Logging.Class
  ( Severity,
    LoggingLevel (..),
    Message (..),
    MonadLogger (..),
    defaultLoggingLevel,
    logWarning,
    logDebug,
    logDebugM,
    showMessages,
    setTextColour,
    setBackgroundColour,
    allLoggingLevels,
    loggingLevelHelp,
    logCompilerPass,
    logCompilerPassOutput,
    logCompilerSection,
  )
where

import Control.Monad (join, when)
import Control.Monad.Except (ExceptT (..))
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (StateT (..))
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Writer (WriterT (..))
import Data.Text (Text)
import Data.Text qualified as T
import System.Console.ANSI
import Vehicle.Prelude.Misc (enumerate, supportedOptions)
import Vehicle.Prelude.Prettyprinter
import Vehicle.Prelude.Supply (SupplyT)
import Vehicle.Syntax.Prelude (layoutAsText)

--------------------------------------------------------------------------------
-- Logging settings

data Severity
  = Debug
  | Warning
  deriving (Eq, Ord)

severityPrefix :: Severity -> Text
severityPrefix Warning = "Warning: "
severityPrefix Debug = ""

severityColour :: Severity -> Maybe Color
severityColour = \case
  Warning -> Just Yellow
  Debug -> Just Green

setTextColour :: Color -> String -> String
setTextColour c s =
  join
    [setSGRCode [SetColor Foreground Vivid c], s, setSGRCode []]

setBackgroundColour :: Color -> String -> String
setBackgroundColour c s =
  join
    [setSGRCode [SetColor Background Vivid c], s, setSGRCode []]

--------------------------------------------------------------------------------
-- Logging levels

data LoggingLevel
  = NoDetail
  | MinDetail
  | MidDetail
  | MaxDetail
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

allLoggingLevels :: [String]
allLoggingLevels = map show (enumerate @LoggingLevel)

defaultLoggingLevel :: LoggingLevel
defaultLoggingLevel = NoDetail

loggingLevelHelp :: String
loggingLevelHelp =
  "Sets the level of detail in the logs if the --log argument has been passed. "
    <> supportedOptions allLoggingLevels

--------------------------------------------------------------------------------
-- Messages

data Message = Message
  { severityOf :: Severity,
    textOf :: Text
  }

instance Show Message where
  show (Message s t) =
    let txt = T.unpack (severityPrefix s <> t)
     in maybe txt (`setTextColour` txt) (severityColour s)

showMessages :: [Message] -> String
showMessages logs = unlines $ map show logs

type CallDepth = Int

--------------------------------------------------------------------------------
-- Logging monad

class (Monad m) => MonadLogger m where
  setCallDepth :: CallDepth -> m ()
  getCallDepth :: m CallDepth
  incrCallDepth :: m ()
  decrCallDepth :: m ()
  getDebugLevel :: m LoggingLevel
  logMessage :: Message -> m ()

instance (MonadLogger m) => MonadLogger (StateT s m) where
  setCallDepth = lift . setCallDepth
  getCallDepth = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage = lift . logMessage

instance (MonadLogger m) => MonadLogger (ReaderT s m) where
  setCallDepth = lift . setCallDepth
  getCallDepth = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage = lift . logMessage

instance (Monoid w, MonadLogger m) => MonadLogger (WriterT w m) where
  setCallDepth = lift . setCallDepth
  getCallDepth = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage = lift . logMessage

instance (MonadLogger m) => MonadLogger (ExceptT e m) where
  setCallDepth = lift . setCallDepth
  getCallDepth = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage = lift . logMessage

instance (MonadLogger m) => MonadLogger (SupplyT s m) where
  setCallDepth = lift . setCallDepth
  getCallDepth = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage = lift . logMessage

logWarning :: (MonadLogger m) => Doc a -> m ()
logWarning text = logMessage $ Message Warning (layoutAsText text)

logDebugM :: (MonadLogger m) => LoggingLevel -> m (Doc a) -> m ()
logDebugM level getText = do
  -- traceShow text $ do
  debugLevel <- getDebugLevel
  when (level <= debugLevel) $ do
    text <- getText
    depth <- getCallDepth
    logMessage $ Message Debug (layoutAsText (indent depth text))

-- TODO try implement via logDebugM but check performance first.
logDebug :: (MonadLogger m) => LoggingLevel -> Doc a -> m ()
logDebug level text = do
  -- traceShow text $ do
  debugLevel <- getDebugLevel
  when (level <= debugLevel) $ do
    depth <- getCallDepth
    logMessage $ Message Debug (layoutAsText (indent depth text))

logCompilerPass :: (MonadLogger m) => LoggingLevel -> Doc a -> m b -> m b
logCompilerPass level passName performPass = do
  logDebug level $ "Starting" <+> passName
  incrCallDepth
  result <- performPass
  decrCallDepth
  logDebug level $ "Finished" <+> passName <> line
  return result

logCompilerSection :: (MonadLogger m) => LoggingLevel -> Doc a -> m b -> m b
logCompilerSection level sectionName performPass = do
  logDebug level sectionName
  incrCallDepth
  result <- performPass
  decrCallDepth
  logDebug level ""
  return result

logCompilerPassOutput :: (MonadLogger m) => Doc a -> m ()
logCompilerPassOutput result = do
  logDebug MidDetail "Result:"
  incrCallDepth
  logDebug MidDetail result
  decrCallDepth
