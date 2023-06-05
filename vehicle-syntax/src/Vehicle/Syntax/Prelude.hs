module Vehicle.Syntax.Prelude where

import Control.Exception (Exception, throw)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Serialize (Get, Putter, Serialize (..))
import Data.Serialize.Get (getListOf)
import Data.Serialize.Put (putListOf)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Stack (HasCallStack)
import Numeric (readFloat)
import Prettyprinter (Doc, defaultLayoutOptions, layoutPretty, line, (<+>))
import Prettyprinter.Render.String (renderString)
import Prettyprinter.Render.Text (renderStrict)

--------------------------------------------------------------------------------
-- Developer errors

newtype DeveloperError = DeveloperError Text

instance Show DeveloperError where
  show (DeveloperError text) = Text.unpack text

instance Exception DeveloperError

-- | Immediately terminates execution. When in the `CompileMonad`, you should
-- prefer to use the method `compilerDeveloperError` instead of this, as
-- this method will prevent the logs from being displayed.
developerError :: (HasCallStack) => Doc a -> b
developerError message =
  throw $
    DeveloperError $
      layoutAsText $
        "Something went wrong internally. Please report the error"
          <+> "shown below to `https://github.com/vehicle-lang/vehicle/issues`."
          <> line
          <> "Error:"
          <+> message

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
  _ -> developerError "Invalid number"

--------------------------------------------------------------------------------
-- Serialization instances missing from Cereal

instance (Serialize a) => Serialize (NonEmpty a) where
  put = putNonEmptyListOf put
  get = getNonEmptyListOf get

getNonEmptyListOf :: Get a -> Get (NonEmpty a)
getNonEmptyListOf m = do
  list <- getListOf m
  case NonEmpty.nonEmpty list of
    Nothing -> fail "getNonEmptyListOf: empty list"
    Just neList -> pure neList

putNonEmptyListOf :: Putter a -> Putter (NonEmpty a)
putNonEmptyListOf pa = putListOf pa . NonEmpty.toList
