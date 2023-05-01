module Vehicle.Verify.Specification.Status where

import Data.Aeson
import Data.List.Split (chunksOf)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text, pack)
import Data.Vector.Unboxed qualified as Vector
import GHC.Generics (Generic)
import System.Console.ANSI (Color (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Compile.Type.Subsystem.Standard (TypeCheckedExpr)
import Vehicle.Compile.Type.Subsystem.Standard.Patterns
import Vehicle.Expr.Boolean (MaybeTrivial (..))
import Vehicle.Expr.Normalisable (NormalisableBuiltin (..))
import Vehicle.Verify.Core

class IsVerified a where
  isVerified :: a -> Bool

instance IsVerified (QueryResult witness) where
  isVerified = \case
    SAT {} -> True
    UnSAT -> False

evaluateQuery :: QueryNegationStatus -> MaybeTrivial (QueryResult witness) -> Bool
evaluateQuery negated q =
  negated `xor` case q of
    Trivial b -> b
    NonTrivial a -> isVerified a

--------------------------------------------------------------------------------
-- Verification status of a single property

data PropertyStatus
  = PropertyStatus QueryNegationStatus (MaybeTrivial (QueryResult UserVariableCounterexample))
  deriving (Generic)

instance FromJSON PropertyStatus

instance ToJSON PropertyStatus

--------------------------------------------------------------------------------
-- Verification status of a multi property

data MultiPropertyStatus
  = MultiPropertyStatus [MultiPropertyStatus]
  | SinglePropertyStatus PropertyStatus
  deriving (Generic)

instance FromJSON MultiPropertyStatus

instance ToJSON MultiPropertyStatus

instance IsVerified MultiPropertyStatus where
  isVerified = \case
    MultiPropertyStatus ps -> and (fmap isVerified ps)
    SinglePropertyStatus (PropertyStatus negated result) -> evaluateQuery negated result

prettyPropertyStatus :: Name -> MultiPropertyStatus -> Doc a
prettyPropertyStatus name = \case
  MultiPropertyStatus ps -> do
    let numberedSubproperties =
          zipWith (\(i :: Int) p -> (name <> "!" <> pack (show i), p)) [0 ..] ps
    let numVerified = pretty (length (filter isVerified ps))
    let num = pretty $ length ps
    let summary = pretty name <> ":" <+> numVerified <> "/" <> num <+> "verified"
    let results = indent 2 $ vsep (fmap (uncurry prettyPropertyStatus) numberedSubproperties)
    summary <> line <> results
  SinglePropertyStatus (PropertyStatus negated s) -> do
    let witnessText = if negated then "Counter-example" else "Witness"
    let (verified, evidenceText) = case s of
          Trivial status -> (status `xor` negated, " (trivial)")
          NonTrivial status -> case status of
            UnSAT -> (negated, "")
            SAT Nothing -> (not negated, witnessText <> ": none")
            SAT (Just witness) -> do
              let assignments = vsep (fmap prettyUserVariableAssignment witness)
              let witnessDoc = witnessText <> line <> indent 2 assignments
              (not negated, witnessDoc)

    prettyNameAndStatus name verified <> line <> evidenceText

prettyNameAndStatus :: Text -> Bool -> Doc a
prettyNameAndStatus name verified = do
  let (colour, symbol) = if verified then (Green, "🗸") else (Red, "✗")
  pretty (setTextColour colour symbol) <+> pretty name

prettyUserVariableAssignment :: UserVariableAssignment -> Doc a
prettyUserVariableAssignment UserVariableAssignment {..} = do
  let name = pretty variableName
  let valueExpr = assignmentToExpr variableDimensions (Vector.toList variableValue)
  let value = prettyFriendly (WithContext valueExpr emptyDBCtx)
  name <> ":" <+> value

assignmentToExpr :: TensorDimensions -> [Double] -> TypeCheckedExpr
assignmentToExpr [] xs = RatLiteral mempty (toRational (head xs))
assignmentToExpr (dim : dims) xs = do
  let vecConstructor = Builtin mempty (CConstructor $ LVec dim)
  let inputVarIndicesChunks = chunksOf (product dims) xs
  let elems = fmap (ExplicitArg mempty . assignmentToExpr dims) inputVarIndicesChunks
  normAppList mempty vecConstructor elems

--------------------------------------------------------------------------------
-- Verification status of the specification

newtype SpecificationStatus = SpecificationStatus (Map Name MultiPropertyStatus)
  deriving (Generic)

instance FromJSON SpecificationStatus

instance ToJSON SpecificationStatus

instance IsVerified SpecificationStatus where
  isVerified (SpecificationStatus properties) =
    and (fmap isVerified (Map.elems properties))

instance Pretty SpecificationStatus where
  pretty spec@(SpecificationStatus properties) = do
    let result = "Result:" <+> (if isVerified spec then "true" else "false")
    let propertiesByName = Map.toList properties
    result
      <> line
      <> indent 2 (vsep (fmap (uncurry prettyPropertyStatus) propertiesByName))
