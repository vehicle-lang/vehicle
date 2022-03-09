module Vehicle
  ( run
  , Options(..)
  , Command(..)
  ) where

import Control.Monad (when,)
import System.Exit (exitSuccess)

import Vehicle.Prelude
import Vehicle.Compile (CompileOptions(..), compile)
import Vehicle.Check (CheckOptions(..), check)

--------------------------------------------------------------------------------
-- Main command

run :: Options -> IO ()
run Options{..} = do
  when version $ do
    print vehicleVersion
    exitSuccess

  let loggingSettings = LoggingOptions
        { errorLocation = errFile
        , logLocation   = logFile
        }

  case commandOption of
    Compile options -> compile loggingSettings options
    Check   options -> check   loggingSettings options

data Options = Options
  { version       :: Bool
  , logFile       :: LogFilePath
  , errFile       :: ErrorFilePath
  , commandOption :: Command
  } deriving (Show)

data Command
  = Compile CompileOptions
  | Check   CheckOptions
   deriving (Show)