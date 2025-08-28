module Vehicle.Prelude.Logging.Class
  ( CompilerPass (..),
    allCompilerPasses,
    loggingPassHelp,
    LoggingLevel (..),
    defaultLoggingLevel,
    allLoggingLevels,
    loggingLevelHelp,
    DebugMessage,
    MonadLogger (..),
    logDebug,
    logDebugM,
    logCompilerPass,
    logCompilerPassOutput,
    logCompilerSection,
    logCompilerSection2,
    logIndent,
  )
where

import Control.Monad (when)
import Control.Monad.Except (ExceptT (..))
import Control.Monad.Identity (IdentityT (..))
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (StateT (..))
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Maybe (MaybeT)
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

data CompilerPass
  = Scoping
  | TypeChecking
  | QueryBackend
  | ITPBackend
  | LossBackend
  | QueryError
  | WitnessReconstruction
  deriving (Eq, Show, Read, Bounded, Enum)

instance Pretty CompilerPass where
  pretty = \case
    Scoping -> "scope checking"
    TypeChecking -> "typing checking"
    QueryBackend -> "query compilation"
    ITPBackend -> "ITP compilation"
    LossBackend -> "loss compilation"
    QueryError -> "query error"
    WitnessReconstruction -> "witness reconstruction"

allCompilerPasses :: [String]
allCompilerPasses = map show (enumerate @CompilerPass)

loggingPassHelp :: String
loggingPassHelp =
  "Sets the which compiler pass logging is enabled for. "
    <> supportedOptions allCompilerPasses

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
  enterCompilerPass :: CompilerPass -> m ()
  exitCompilerPass :: m ()
  setCallDepth :: CallDepth -> m ()
  getCallDepth :: m CallDepth
  incrCallDepth :: m ()
  decrCallDepth :: m ()
  getDebugLevel :: m LoggingLevel
  logMessage :: DebugMessage -> m ()
  logWarning :: CompileWarning -> m ()

instance (MonadLogger m) => MonadLogger (StateT s m) where
  enterCompilerPass = lift . enterCompilerPass
  exitCompilerPass = lift exitCompilerPass
  setCallDepth = lift . setCallDepth
  getCallDepth = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage = lift . logMessage
  logWarning = lift . logWarning

instance (MonadLogger m) => MonadLogger (ReaderT s m) where
  enterCompilerPass = lift . enterCompilerPass
  exitCompilerPass = lift exitCompilerPass
  setCallDepth = lift . setCallDepth
  getCallDepth = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage = lift . logMessage
  logWarning = lift . logWarning

instance (Monoid w, MonadLogger m) => MonadLogger (WriterT w m) where
  enterCompilerPass = lift . enterCompilerPass
  exitCompilerPass = lift exitCompilerPass
  setCallDepth = lift . setCallDepth
  getCallDepth = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage = lift . logMessage
  logWarning = lift . logWarning

instance (MonadLogger m) => MonadLogger (ExceptT e m) where
  enterCompilerPass = lift . enterCompilerPass
  exitCompilerPass = lift exitCompilerPass
  setCallDepth = lift . setCallDepth
  getCallDepth = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage = lift . logMessage
  logWarning = lift . logWarning

instance (MonadLogger m) => MonadLogger (SupplyT s m) where
  enterCompilerPass = lift . enterCompilerPass
  exitCompilerPass = lift exitCompilerPass
  setCallDepth = lift . setCallDepth
  getCallDepth = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage = lift . logMessage
  logWarning = lift . logWarning

instance (MonadLogger m) => MonadLogger (IdentityT m) where
  enterCompilerPass = lift . enterCompilerPass
  exitCompilerPass = lift exitCompilerPass
  setCallDepth = lift . setCallDepth
  getCallDepth = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage = lift . logMessage
  logWarning = lift . logWarning

instance (MonadLogger m) => MonadLogger (MaybeT m) where
  enterCompilerPass = lift . enterCompilerPass
  exitCompilerPass = lift exitCompilerPass
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

logCompilerPass :: (MonadLogger m) => CompilerPass -> m b -> m b
logCompilerPass pass performPass = do
  enterCompilerPass pass
  let passName = pretty pass
  result <- logIndent MinDetail ("Starting" <+> passName) performPass
  logDebug MinDetail $ "Finished" <+> passName <> line
  exitCompilerPass
  return result

logCompilerSection2 :: (MonadLogger m) => LoggingLevel -> Doc a -> m b -> m b
logCompilerSection2 level passName performPass = do
  result <- logIndent level ("Starting" <+> passName) performPass
  logDebug level $ "Finished" <+> passName
  return result

logCompilerSection :: (MonadLogger m) => LoggingLevel -> Doc a -> m b -> m b
logCompilerSection level sectionName performPass = do
  result <- logIndent level sectionName performPass
  logDebug level ""
  return result

logIndent :: (MonadLogger m) => LoggingLevel -> Doc a -> m b -> m b
logIndent level sectionName performPass = do
  logDebug level sectionName
  incrCallDepth
  result <- performPass
  decrCallDepth
  return result

logCompilerPassOutput :: (MonadLogger m) => Doc a -> m ()
logCompilerPassOutput result = do
  logDebug MidDetail "Result:"
  incrCallDepth
  logDebug MidDetail result
  decrCallDepth
