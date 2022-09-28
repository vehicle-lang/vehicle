module Vehicle.Verify.Specification.Status where

import Data.Aeson
import Data.Text (Text, pack)
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Generics (Generic)
import System.Console.ANSI (Color(..))

import Vehicle.Prelude
import Vehicle.Verify.Specification

class IsVerified a where
  isVerified :: a -> Bool

--------------------------------------------------------------------------------
-- Verification status of a single property

data SatisfiabilityStatus
  = SAT (Maybe Text)
  | UnSAT
  deriving (Show, Generic)

instance FromJSON SatisfiabilityStatus
instance ToJSON SatisfiabilityStatus

instance IsVerified SatisfiabilityStatus where
  isVerified = \case
    SAT{} -> True
    UnSAT -> False

instance IsVerified (NegationStatus, Query SatisfiabilityStatus) where
  isVerified (negated, sat) = evaluateQuery negated isVerified sat

--------------------------------------------------------------------------------
-- Verification status of a single property

data PropertyStatus
  = MultiPropertyStatus [PropertyStatus]
  | SinglePropertyStatus (NegationStatus, Query SatisfiabilityStatus)
  deriving (Generic)

instance FromJSON PropertyStatus
instance ToJSON PropertyStatus

instance IsVerified PropertyStatus where
  isVerified = \case
    MultiPropertyStatus ps -> and (fmap isVerified ps)
    SinglePropertyStatus s -> isVerified s

prettyPropertyStatus :: Symbol -> PropertyStatus -> Doc a
prettyPropertyStatus name = \case
  MultiPropertyStatus ps -> do
    let numberedSubproperties =
          zipWith (\(i :: Int) p -> (name <> "!" <> pack (show i), p)) [0..] ps
    let numVerified = pretty (length (filter isVerified ps))
    let num = pretty $ length ps
    let summary = pretty name <> ":" <+> numVerified <> "/" <> num <+> "verified"
    let results = indent 2 $ vsep (fmap (uncurry prettyPropertyStatus) numberedSubproperties)
    summary <> line <> results

  SinglePropertyStatus (negated, s) -> do
    let (verified, evidenceText) = case s of
          Trivial    status -> (status `xor` negated, " (trivial)")
          NonTrivial status -> case status of
            UnSAT       -> (negated, "")
            SAT witness -> do
              let witnessText = if negated then "Counter-example" else "Witness"
              let formatWitness e = line <> indent 2 (witnessText <> ":" <+> pretty e)
              let witnessDoc = maybe "" formatWitness witness
              (not negated, witnessDoc)

    prettyNameAndStatus name verified <> evidenceText

prettyNameAndStatus :: Text -> Bool -> Doc a
prettyNameAndStatus name verified = do
  let (colour, symbol) = if verified then (Green, "🗸") else (Red, "✗")
  pretty (setTextColour colour symbol) <+> pretty name

--------------------------------------------------------------------------------
-- Verification status of the specification

newtype SpecificationStatus = SpecificationStatus (Map Text PropertyStatus)
  deriving (Generic)

instance FromJSON SpecificationStatus
instance ToJSON SpecificationStatus

instance IsVerified SpecificationStatus where
  isVerified (SpecificationStatus properties) =
    and (fmap isVerified (Map.elems properties))

instance Pretty SpecificationStatus where
  pretty spec@(SpecificationStatus properties) =
    (if isVerified spec
      then "Result: verified"
      else "Result: unverified")
    <> line
    <> indent 2 (vsep (fmap (uncurry prettyPropertyStatus) (Map.toList properties)))
