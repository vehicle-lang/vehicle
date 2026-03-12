module Vehicle.LSP.State where

import Control.Concurrent.STM (TVar)
import Control.Monad.IO.Class (MonadIO)
import Data.Map (Map)
import Vehicle.Data.AST.Name (ModulePath)
import Vehicle.TypeCheck (ProgramContext, initialProgramContext)

data ServerState = ServerState
  { programContext :: ProgramContext,
    availableModules :: Map ModulePath FilePath
  }

type ServerStateRef = TVar ServerState

initialServerState :: ServerState
initialServerState =
  ServerState
    { programContext = initialProgramContext,
      availableModules = mempty
    }

initialiseServerState :: (MonadIO m) => ServerStateRef -> m ()
initialiseServerState = do
  _
