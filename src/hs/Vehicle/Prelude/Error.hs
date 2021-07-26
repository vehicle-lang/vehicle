
module Vehicle.Prelude.Error where

import Data.Text (Text)
import Data.Void (Void)
import GHC.Stack (HasCallStack)
import Prettyprinter (Doc, Pretty(..), (<+>), squotes, line, unAnnotate)

import Vehicle.Prelude.Provenance (Provenance)
import Vehicle.Prelude.Prettyprinter

--------------------------------------------------------------------------------
-- Developer errors

developerError :: HasCallStack => Doc a -> b
developerError message = error $ layoutAsString $
  "Something went wrong internally. Please report the error" <+>
  "shown below to `https://github.com/wenkokke/vehicle/issues`." <> line <>
  "Error:" <+> message

--------------------------------------------------------------------------------
-- User errors

-- |Errors that are the user's responsibility to fix.
data UserError = UserError
  { provenance :: Provenance
  , problem    :: Doc Void
  , fix        :: Doc Void
  }

-- |Errors from external code that we have no control over.
-- These may be either user or developer errors but in general we
-- can't distinguish between the two.
newtype ExternalError = ExternalError Text

data VehicleError
  = UError UserError
  | EError ExternalError

class MeaningfulError e where
  details :: e -> VehicleError

instance Pretty VehicleError where
  pretty (UError (UserError p prob probFix)) =
    unAnnotate $ "Error:" <+> appendProvenance prob p <> line <> fixText probFix

  pretty (EError (ExternalError text)) =
    pretty text

instance Show VehicleError where
  show = layoutAsString . pretty

appendProvenance :: Doc ann -> Provenance -> Doc ann
appendProvenance doc p = doc <+> "(" <> pretty p <> ")"

fixText :: Doc ann -> Doc ann
fixText t = "Fix:" <+> t

squotes :: Text -> Doc ann
squotes t = Prettyprinter.squotes (pretty t)