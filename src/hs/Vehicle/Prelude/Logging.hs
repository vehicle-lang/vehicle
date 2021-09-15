

module Vehicle.Prelude.Logging where

import Control.Monad.Writer (MonadWriter(..), WriterT(..))
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

data Message = Message
  { severity :: Severity
  , text :: Text
  }

type MonadLog m = MonadWriter [Message] m
type LoggerT m a = WriterT [Message] m a

logError :: MonadLog m => Text -> m ()
logError text = tell [Message Error text]

logWarning :: MonadLog m => Text -> m ()
logWarning text = tell [Message Warning text]

logInfo :: MonadLog m => Text -> m ()
logInfo text = tell [Message Info text]

logDebug :: MonadLog m => Text -> m ()
logDebug text = tell [Message Debug text]

runLoggerT :: LoggerT m a -> m (a, [Message])
runLoggerT = runWriterT

resetColor :: String
resetColor = setSGRCode []

instance Show Message where
  show (Message s t) = setColor s <> T.unpack t <> resetColor