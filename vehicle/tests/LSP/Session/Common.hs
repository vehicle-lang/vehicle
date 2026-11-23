module Session.Common
  ( runSessionWithDiagnostics,
  )
where

import Control.Exception (catch, throwIO)
import Language.LSP.Protocol.Capabilities (fullLatestClientCaps)
import Language.LSP.Test
  ( Session,
    SessionConfig (..),
    SessionException,
    defaultConfig,
    runSessionWithConfig,
  )
import System.IO (hPutStrLn, stderr)

lspSessionConfig :: SessionConfig
lspSessionConfig =
  defaultConfig
    { messageTimeout = 60,
      logStdErr = False,
      logMessages = False,
      logColor = False
    }

runSessionWithDiagnostics ::
  String ->
  FilePath ->
  Session a ->
  IO a
runSessionWithDiagnostics command root session =
  runSessionWithConfig lspSessionConfig command fullLatestClientCaps root session
    `catch` \(exception :: SessionException) -> do
      hPutStrLn stderr $
        "LSP session failure: " <> show exception
      throwIO exception
