
module Vehicle.Prelude.DeveloperError
  ( developerError
  ) where

import Data.Text
import Control.Exception (Exception, throw)
import GHC.Stack (HasCallStack)

import Vehicle.Prelude.Prettyprinter

--------------------------------------------------------------------------------
-- Developer errors

newtype DeveloperError = DeveloperError Text

instance Show DeveloperError where
  show (DeveloperError text) = unpack text

instance Exception DeveloperError

developerError :: HasCallStack => Doc a -> b
developerError message = throw $ DeveloperError $ layoutAsText $
  "Something went wrong internally. Please report the error" <+>
  "shown below to `https://github.com/wenkokke/vehicle/issues`." <> line <>
  "Error:" <+> message