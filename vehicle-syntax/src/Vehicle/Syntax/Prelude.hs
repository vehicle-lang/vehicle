module Vehicle.Syntax.Prelude where

import Control.Exception (Exception, throw)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Stack (HasCallStack)

import Prettyprinter ( Doc, (<+>), defaultLayoutOptions, layoutPretty, line )
import Prettyprinter.Render.String (renderString)
import Prettyprinter.Render.Text (renderStrict)
import Numeric (readFloat)

--------------------------------------------------------------------------------
-- Developer errors

newtype DeveloperError = DeveloperError Text

instance Show DeveloperError where
  show (DeveloperError text) = Text.unpack text

instance Exception DeveloperError

-- | Immediately terminates execution. When in the `CompileMonad`, you should
-- prefer to use the method `compilerDeveloperError` instead of this, as
-- this method will prevent the logs from being displayed.
developerError :: HasCallStack => Doc a -> b
developerError message = throw $ DeveloperError $ layoutAsText $
  "Something went wrong internally. Please report the error" <+>
  "shown below to `https://github.com/vehicle-lang/vehicle/issues`." <> line <>
  "Error:" <+> message

--------------------------------------------------------------------------------
-- Prettyprinting

layoutAsString :: Doc a -> String
layoutAsString = renderString . layoutPretty defaultLayoutOptions

layoutAsText :: Doc a -> Text
layoutAsText = renderStrict . layoutPretty defaultLayoutOptions

--------------------------------------------------------------------------------
-- Reading

readNat :: Text -> Int
readNat = read . Text.unpack

readRat :: Text -> Prelude.Rational
readRat str = case readFloat (Text.unpack str) of
  ((n, []) : _) -> n
  _             -> developerError "Invalid number"
