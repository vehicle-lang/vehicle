{-# LANGUAGE DuplicateRecordFields #-}

module Vehicle.Error where

import Data.Text (Text)
import Data.Void (Void)
import Prettyprinter (Doc, Pretty(..), (<+>), squotes, line, unAnnotate, layoutPretty, defaultLayoutOptions)
import Prettyprinter.Render.String (renderString)

import Vehicle.Prelude.Provenance (Provenance)

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
  show = renderString . layoutPretty defaultLayoutOptions . pretty

appendProvenance :: Doc ann -> Provenance -> Doc ann
appendProvenance doc p = doc <+> "(" <> pretty p <> ")"

fixText :: Doc ann -> Doc ann
fixText t = "Fix:" <+> t

squotes :: Text -> Doc ann
squotes t = Prettyprinter.squotes (pretty t)