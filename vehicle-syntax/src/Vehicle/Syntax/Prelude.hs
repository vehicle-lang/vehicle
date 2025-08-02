{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Syntax.Prelude where

import Control.Exception (Exception, throw)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Serialize (Get, Putter, Serialize (..))
import Data.Serialize.Get (getListOf)
import Data.Serialize.Put (putListOf)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Numeric (readFloat)
import Prettyprinter (Doc, Pretty (..), defaultLayoutOptions, indent, layoutPretty, line, list, squotes, (<+>))
import Prettyprinter.Render.String (renderString)
import Prettyprinter.Render.Text (renderStrict)

#if MIN_VERSION_base(4,19,0)
import qualified Data.Functor as F
#endif
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
          <> line
          <> "Stack:"
          <> line
          <> pretty (prettyCallStack callStack)

unexpectedExpr :: Doc a -> Doc a -> Doc a
unexpectedExpr pass name =
  "encountered unexpected expression:"
    <> line
    <> indent 2 name
    <> line
    <> "during"
    <+> pass
    <> "."

unexpectedExprError :: (HasCallStack) => Doc a -> Doc a -> b
unexpectedExprError pass name = developerError $ unexpectedExpr pass name

normalisationError :: (HasCallStack) => Doc a -> Doc a -> b
normalisationError pass name =
  developerError $
    unexpectedExpr pass name <+> "We should have normalised this out."

unexpectedTypeInExprError :: (HasCallStack) => Doc a -> Doc a -> b
unexpectedTypeInExprError pass name =
  developerError $
    unexpectedExpr pass name <+> "We should not be processing types."

illTypedError :: (HasCallStack) => Doc a -> Doc a -> b
illTypedError pass name =
  developerError $
    unexpectedExpr pass name <+> "This is ill-typed."

visibilityError :: (HasCallStack) => Doc a -> Doc a -> Doc a -> b
visibilityError pass fun args =
  developerError $
    unexpectedExpr pass args <+> "Does not match function's visibility:" <> line <> indent 2 fun

-- | Throw this when you encounter a case that should have been resolved during
-- type-checking, e.g. holes or metas.
resolutionError :: (HasCallStack) => Doc a -> Doc a -> b
resolutionError pass name =
  developerError $
    unexpectedExpr pass name <+> "We should have resolved this during type-checking."

caseError :: (HasCallStack) => Doc a -> Doc a -> [Doc a] -> b
caseError pass name cases =
  developerError $
    unexpectedExpr pass name
      <+> "This should already have been caught by the"
      <+> "following cases:"
      <+> list cases

internalScopingError :: (HasCallStack) => Doc a -> b
internalScopingError ident =
  developerError $
    "Internal scoping error"
      <> ":"
      <+> "declaration"
      <+> squotes ident
      <+> "not found in scope..."

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
  xs <- getListOf m
  case NonEmpty.nonEmpty xs of
    Nothing -> fail "getNonEmptyListOf: empty list"
    Just neList -> pure neList

putNonEmptyListOf :: Putter a -> Putter (NonEmpty a)
putNonEmptyListOf pa = putListOf pa . NonEmpty.toList

unzipF :: (Functor f) => f (a, b) -> (f a, f b)
#if MIN_VERSION_base(4,19,0)
unzipF = F.unzip
#else
unzipF = NonEmpty.unzip
#endif
