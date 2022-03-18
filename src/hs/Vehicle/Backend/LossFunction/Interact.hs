module Vehicle.Backend.LossFunction.Interact
  ( writeLossFunctionFiles
  ) where

import Vehicle.Backend.Prelude
import Vehicle.Prelude
import Control.Monad (forM_)
import Vehicle.Backend.LossFunction.Compile
import Data.Aeson
import Data.ByteString.Lazy.Char8

writeLossFunctionFiles :: Maybe FilePath -> [LExpr] -> IO ()
writeLossFunctionFiles filepath functions = forM_ functions $ \function -> do
  writeResultToFile LossFunction filepath (pretty (unpack (encode function)))