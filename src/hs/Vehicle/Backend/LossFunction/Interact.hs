module Vehicle.Backend.LossFunction.Interact
  ( writeLossFunctionFiles
  ) where

import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8
import Vehicle.Backend.LossFunction.Compile
import Vehicle.Backend.Prelude
import Vehicle.Prelude


encode :: [LDecl] -> String
encode e = unpack $ flip encodePretty' e $ Config
  { confIndent          = Spaces 2
  , confCompare         = \t1 t2 -> compare t2 t1
  , confNumFormat       = Generic
  , confTrailingNewline = False
  }

writeLossFunctionFiles :: Maybe FilePath -> [LDecl] -> IO ()
writeLossFunctionFiles filepath functions =
  writeResultToFile LossFunction filepath (pretty (encode functions))
