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

encode :: [LDecl] -> String
encode e =
  unpack $
    flip encodePretty' e $
      Config
        { confIndent = Spaces 2,
          confCompare = \t1 t2 -> compare t2 t1,
          confNumFormat = Generic,
          confTrailingNewline = False
        }

writeLossFunctionFiles ::
  MonadIO m =>
  Maybe FilePath ->
  DifferentiableLogic ->
  [LDecl] ->
  m ()
writeLossFunctionFiles filepath t functions =
  writeResultToFile (LossFunction t) filepath (pretty (encode functions))
