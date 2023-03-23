module Vehicle.Backend.LossFunction.Interact
  ( writeLossFunctionFiles,
  )
where

import Control.Monad.IO.Class
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8
import Vehicle.Backend.LossFunction.Compile
import Vehicle.Backend.Prelude
import Vehicle.Prelude

writeLossFunctionFiles ::
  MonadIO m =>
  Maybe FilePath ->
  DifferentiableLogic ->
  [LDecl] ->
  m ()
writeLossFunctionFiles filepath _t functions = do
  let json = encodePretty' prettyJSONConfig functions
  writeResultToFile lossFunctionOutputFormat filepath (pretty (unpack json))

lossFunctionOutputFormat :: Maybe ExternalOutputFormat
lossFunctionOutputFormat = Nothing
