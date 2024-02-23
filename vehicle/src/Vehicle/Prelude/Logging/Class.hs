module Vehicle.Prelude.Logging.Class
  ( CompilerPass,
    LoggingLevel (..),
    DebugMessage,
    MonadLogger (..),
    defaultLoggingLevel,
    logDebug,
    logDebugM,
    allLoggingLevels,
    loggingLevelHelp,
    logCompilerPass,
    logCompilerPassOutput,
    logCompilerSection,
  )
where

import Control.Monad (when)
import Control.Monad.Except (ExceptT (..))
import Control.Monad.Identity (IdentityT (..))
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (StateT (..))
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Writer (WriterT (..))
import Data.Text (Text)
import Data.Text qualified as Text (unpack)
import System.Console.ANSI (Color (..))
import Vehicle.Prelude.Misc (enumerate, setTextColour, supportedOptions)
import Vehicle.Prelude.Prettyprinter
import Vehicle.Prelude.Supply (SupplyT)
import Vehicle.Prelude.Warning
import Vehicle.Syntax.Prelude (layoutAsText)

--------------------------------------------------------------------------------
-- Settings

type CompilerPass = Doc ()

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

newtype DebugMessage = DebugMessage Text

instance Show DebugMessage where
  show (DebugMessage t) = setTextColour Green $ Text.unpack t

type CallDepth = Int

--------------------------------------------------------------------------------
-- Logging monad

class (Monad m) => MonadLogger m where
  setCallDepth :: CallDepth -> m ()
  getCallDepth :: m CallDepth
  incrCallDepth :: m ()
  decrCallDepth :: m ()
  getDebugLevel :: m LoggingLevel
  logMessage :: DebugMessage -> m ()
  logWarning :: CompileWarning -> m ()

instance (MonadLogger m) => MonadLogger (StateT s m) where
  setCallDepth = lift . setCallDepth
  getCallDepth = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage = lift . logMessage
  logWarning = lift . logWarning

instance (MonadLogger m) => MonadLogger (ReaderT s m) where
  setCallDepth = lift . setCallDepth
  getCallDepth = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage = lift . logMessage
  logWarning = lift . logWarning

instance (Monoid w, MonadLogger m) => MonadLogger (WriterT w m) where
  setCallDepth = lift . setCallDepth
  getCallDepth = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage = lift . logMessage
  logWarning = lift . logWarning

instance (MonadLogger m) => MonadLogger (ExceptT e m) where
  setCallDepth = lift . setCallDepth
  getCallDepth = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage = lift . logMessage
  logWarning = lift . logWarning

instance (MonadLogger m) => MonadLogger (SupplyT s m) where
  setCallDepth = lift . setCallDepth
  getCallDepth = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage = lift . logMessage
  logWarning = lift . logWarning

instance (MonadLogger m) => MonadLogger (IdentityT m) where
  setCallDepth = lift . setCallDepth
  getCallDepth = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage = lift . logMessage
  logWarning = lift . logWarning

logDebugM :: (MonadLogger m) => LoggingLevel -> m (Doc a) -> m ()
logDebugM level getText = do
  -- traceShow text $ do
  debugLevel <- getDebugLevel
  when (level <= debugLevel) $ do
    text <- getText
    depth <- getCallDepth
    logMessage $ DebugMessage (layoutAsText (indent depth text))

-- TODO try implement via logDebugM but check performance first.
logDebug :: (MonadLogger m) => LoggingLevel -> Doc a -> m ()
logDebug level text = logDebugM level (return text)

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
