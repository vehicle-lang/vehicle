module Vehicle.Verify.Specification.Status where

import Data.Aeson
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import System.Console.ANSI (Color (..))
import Vehicle.Expr.Boolean (MaybeTrivial (..))
import Vehicle.Prelude
import Vehicle.Syntax.AST (Name)
import Vehicle.Verify.Core
import Vehicle.Verify.Specification

class IsVerified a where
  isVerified :: a -> Bool

--------------------------------------------------------------------------------
-- Verification status of a single property

instance IsVerified QueryResult where
  isVerified = \case
    SAT {} -> True
    UnSAT -> False

instance IsVerified (QueryNegationStatus, MaybeTrivial QueryResult) where
  isVerified (negated, sat) = evaluateQuery negated isVerified sat

--------------------------------------------------------------------------------
-- Verification status of a single property

data PropertyStatus
  = MultiPropertyStatus [PropertyStatus]
  | SinglePropertyStatus (QueryNegationStatus, MaybeTrivial QueryResult)
  deriving (Generic)

instance FromJSON PropertyStatus

instance ToJSON PropertyStatus

instance IsVerified PropertyStatus where
  isVerified = \case
    MultiPropertyStatus ps -> and (fmap isVerified ps)
    SinglePropertyStatus s -> isVerified s

prettyPropertyStatus :: Name -> PropertyStatus -> Doc a
prettyPropertyStatus name = \case
  MultiPropertyStatus ps -> do
    let numberedSubproperties =
          zipWith (\(i :: Int) p -> (name <> "!" <> pack (show i), p)) [0 ..] ps
    let numVerified = pretty (length (filter isVerified ps))
    let num = pretty $ length ps
    let summary = pretty name <> ":" <+> numVerified <> "/" <> num <+> "verified"
    let results = indent 2 $ vsep (fmap (uncurry prettyPropertyStatus) numberedSubproperties)
    summary <> line <> results
  SinglePropertyStatus (negated, s) -> do
    let (verified, evidenceText) = case s of
          Trivial status -> (status `xor` negated, " (trivial)")
          NonTrivial status -> case status of
            UnSAT -> (negated, "")
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

newtype SpecificationStatus = SpecificationStatus (Map Name PropertyStatus)
  deriving (Generic)

instance FromJSON SpecificationStatus

instance ToJSON SpecificationStatus

instance IsVerified SpecificationStatus where
  isVerified (SpecificationStatus properties) =
    and (fmap isVerified (Map.elems properties))

instance Pretty SpecificationStatus where
  pretty spec@(SpecificationStatus properties) = do
    let result = "Result:" <> (if isVerified spec then "verified" else "unverified")
    let propertiesByName = Map.toList properties
    result
      <> line
      <> indent 2 (vsep (fmap (uncurry prettyPropertyStatus) propertiesByName))
