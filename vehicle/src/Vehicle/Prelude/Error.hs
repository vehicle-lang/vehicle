module Vehicle.Prelude.Error where

import Control.Exception (Exception, throw)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Prettyprinter (list)
import Vehicle.Prelude.Prettyprinter

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
