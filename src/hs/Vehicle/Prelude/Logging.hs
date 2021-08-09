

module Vehicle.Prelude.Logging where

import Colog (Msg, WithLog)
import Colog qualified (log)
import Control.Monad.Reader (ask)
import Data.Text (Text)
import Data.Set (Set, member)

data Severity
  = Error
  | Warning
  | Debug DebugLevel

data DebugLevel
  = Light
  | Medium
  | Detailed
  deriving (Eq, Ord)

data CompilerPhase
  = Elaborate
  | ScopeCheck
  | TypeCheck
  | DescopeCheck
  | Delaborate
  deriving (Eq, Ord)

data LoggingSettings = LoggingSettings
  { phases     :: Set CompilerPhase
  , debugLevel :: DebugLevel
  }

type MonadLog m = WithLog LoggingSettings (Msg Severity) m

logError :: MonadLog m => Text -> m ()
logError = Colog.log Error

logWarning :: MonadLog m => Text -> m ()
logWarning = Colog.log Warning

logDebug :: MonadLog m => CompilerPhase -> DebugLevel -> Text -> m ()
logDebug phase level txt = do
  LoggingSettings{..} <- ask
  if debugLevel < level && member phase phases
    then return ()
    else Colog.log (Debug level) txt