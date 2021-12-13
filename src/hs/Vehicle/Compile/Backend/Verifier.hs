
module Vehicle.Compile.Backend.Verifier where

import Vehicle.Prelude
import Vehicle.Compile.Error

--------------------------------------------------------------------------------
-- Errors

typeError :: Doc a -> b
typeError name = developerError $
  unexpectedExprError name <+> "We should not be compiling types."

visibilityError :: Doc a -> b
visibilityError name = developerError $
  unexpectedExprError name <+> "Should not be present as explicit arguments"

resolutionError :: Doc a -> b
resolutionError name = developerError $
  unexpectedExprError name <+> "We should have resolved this during type-checking."
