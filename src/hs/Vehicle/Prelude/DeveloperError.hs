
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

-- | Immediately terminates execution. When in the `CompileMonad`, you should
-- prefer to use the method `compilerDeveloperError` instead of this, as
-- this method will prevent the logs from being displayed.
developerError :: HasCallStack => Doc a -> b
developerError message = throw $ DeveloperError $ layoutAsText $
  "Something went wrong internally. Please report the error" <+>
  "shown below to `https://github.com/vehicle-lang/vehicle/issues`." <> line <>
  "Error:" <+> message