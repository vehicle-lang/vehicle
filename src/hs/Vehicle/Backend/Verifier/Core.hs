
module Vehicle.Backend.Verifier.Core where

import Vehicle.Prelude
import Vehicle.Language.AST

--------------------------------------------------------------------------------
-- Errors

unexpectedExprError :: Doc a -> Doc a
unexpectedExprError name =
  "encountered unexpected expression" <+> squotes name <+> "during compilation."

typeError :: Doc a -> b
typeError name = developerError $
  unexpectedExprError name <+> "We should not be compiling types."

visibilityError :: Doc a -> b
visibilityError name = developerError $
  unexpectedExprError name <+> "Should not be present as explicit arguments"

resolutionError :: Doc a -> b
resolutionError name = developerError $
  unexpectedExprError name <+> "We should have resolved this during type-checking."

normalisationError :: Doc a -> b
normalisationError name = developerError $
  unexpectedExprError name <+> "We should have normalised this out."

--------------------------------------------------------------------------------
-- Results

isProperty :: CheckedExpr -> Bool
isProperty (Builtin _ Prop) = True
isProperty _                = False