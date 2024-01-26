module Vehicle.Verify.Specification.Status where

import Data.Aeson
import Data.List.Split (chunksOf)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import System.Console.ANSI (Color (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Data.BooleanExpr (MaybeTrivial (..))
import Vehicle.Data.BuiltinInterface.Expr
import Vehicle.Data.LinearExpr (RationalTensor)
import Vehicle.Verify.Core
import Vehicle.Verify.Specification
import Vehicle.Verify.Variable

class IsVerified a where
  isVerified :: a -> Bool

instance IsVerified (QueryResult witness) where
  isVerified = \case
    SAT {} -> True
    UnSAT -> False

evaluateQuery :: QuerySetNegationStatus -> QueryResult witness -> Bool
evaluateQuery negated q = negated `xor` isVerified q

--------------------------------------------------------------------------------
-- Verification status of a single property

newtype PropertyStatus
  = PropertyStatus (MaybeTrivial (QuerySetNegationStatus, QueryResult UserVariableAssignment))
  deriving (Generic)

instance FromJSON PropertyStatus

instance ToJSON PropertyStatus

instance IsVerified PropertyStatus where
  isVerified (PropertyStatus maybeResult) = case maybeResult of
    Trivial b -> b
    NonTrivial (negated, result) -> evaluateQuery negated result

instance Pretty PropertyStatus where
  pretty (PropertyStatus maybeResult) = do
    let (verified, evidenceText) = case maybeResult of
          Trivial status -> (status, "(trivial)")
          NonTrivial (negated, status) -> do
            let witnessText = if negated then "counterexample" else "witness"
            case status of
              UnSAT -> (negated, "proved no" <+> witnessText <+> "exists")
              SAT Nothing -> (not negated, "no" <> witnessText <+> "found")
              SAT Just {} -> (not negated, witnessText <+> "found")
    pretty (statusSymbol verified) <+> "-" <+> evidenceText

--------------------------------------------------------------------------------
-- Verification status of a multi property

type MultiPropertyStatus = MultiProperty PropertyStatus

instance IsVerified MultiPropertyStatus where
  isVerified = \case
    MultiProperty ps -> all isVerified ps
    SingleProperty _ status -> isVerified status

nameSubProperties :: Name -> [MultiPropertyStatus] -> [(Name, MultiPropertyStatus)]
nameSubProperties name = zipWith (\(i :: Int) p -> (name <> "!" <> pack (show i), p)) [0 ..]

prettyMultiPropertyStatus :: Name -> MultiPropertyStatus -> Doc a
prettyMultiPropertyStatus name = \case
  MultiProperty ps -> do
    let namedSubproperties = nameSubProperties name ps
    let numVerified = pretty (length (filter isVerified ps))
    let num = pretty $ length ps
    let summary = "Property" <+> quotePretty name <> ":" <+> numVerified <> "/" <> num <+> "verified"
    let results = indent 2 $ vsep (fmap (uncurry prettyMultiPropertyStatus) namedSubproperties)
    summary <> line <> results
  SingleProperty _address status ->
    pretty name <+> pretty status

statusSymbol :: Bool -> String
statusSymbol verified = do
  let (colour, symbol) = if verified then (Green, "🗸") else (Red, "✗")
  setTextColour colour symbol

prettyNameAndStatus :: Text -> Bool -> Doc a
prettyNameAndStatus name verified = do
  pretty (statusSymbol verified) <+> pretty name

prettyUserVariableAssignment :: (OriginalUserVariable, RationalTensor) -> Doc a
prettyUserVariableAssignment (OriginalUserVariable {..}, variableValue) =
  pretty userTensorVarName <> ":" <+> pretty variableValue

assignmentToExpr :: TensorDimensions -> [Rational] -> Expr Ix Builtin
assignmentToExpr [] xs = RatLiteral mempty (toRational (head xs))
assignmentToExpr (dim : dims) xs = do
  let vecConstructor = Builtin mempty (BuiltinConstructor $ LVec dim)
  let inputVarIndicesChunks = chunksOf (product dims) xs
  let elems = fmap (Arg mempty Explicit Relevant . assignmentToExpr dims) inputVarIndicesChunks
  normAppList mempty vecConstructor elems

--------------------------------------------------------------------------------
-- Verification status of the specification

type SpecificationStatus = Specification PropertyStatus

instance IsVerified SpecificationStatus where
  isVerified (Specification properties) =
    all (isVerified . snd) properties

instance Pretty SpecificationStatus where
  pretty spec@(Specification properties) = do
    let result = "Specification summary:" <+> (if isVerified spec then "true" else "false")
    result
      <> line
      <> indent 2 (vsep (fmap (uncurry prettyMultiPropertyStatus) properties))
