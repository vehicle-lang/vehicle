module Vehicle.Backend.LossFunction.Interact
  ( writeLossFunctionFiles
  ) where

import Vehicle.Backend.Prelude
import Vehicle.Prelude
import Control.Monad (forM_)
import Vehicle.Backend.LossFunction.Compile
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8


encode :: LExpr -> String
encode e = unpack $ flip encodePretty' e $ Config
  { confIndent          = Spaces 2
  , confCompare         = \t1 t2 -> compare t2 t1
  , confNumFormat       = Generic
  , confTrailingNewline = False
  }

writeLossFunctionFiles :: Maybe FilePath -> [LExpr] -> IO ()
writeLossFunctionFiles filepath functions = forM_ functions $ \function -> do
  writeResultToFile LossFunction filepath (pretty (encode function))