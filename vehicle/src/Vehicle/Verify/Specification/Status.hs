module Vehicle.Verify.Specification.Status where

import Control.Monad (forM_)
import Data.Aeson
import Data.List.Split (chunksOf)
import Data.Text (Text, pack)
import Data.Vector.Unboxed qualified as Vector
import GHC.Generics (Generic)
import System.Console.ANSI (Color (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Compile.Queries.Variable
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Compile.Type.Subsystem.Standard.Patterns
import Vehicle.Expr.Boolean (MaybeTrivial (..))
import Vehicle.Expr.DeBruijn (Ix)
import Vehicle.Expr.Normalisable (NormalisableBuiltin (..))
import Vehicle.Verify.Core
import Vehicle.Verify.Specification

class IsVerified a where
  isVerified :: a -> Bool

instance IsVerified (QueryResult witness) where
  isVerified = \case
    SAT {} -> True
    UnSAT -> False

evaluateQuery :: QuerySetNegationStatus -> MaybeTrivial (QueryResult witness) -> Bool
evaluateQuery negated q =
  negated `xor` case q of
    Trivial b -> b
    NonTrivial a -> isVerified a

--------------------------------------------------------------------------------
-- Verification status of a single property

data PropertyStatus
  = PropertyStatus QuerySetNegationStatus (MaybeTrivial (QueryResult UserVariableAssignment))
  deriving (Generic)

instance FromJSON PropertyStatus

instance ToJSON PropertyStatus

instance IsVerified PropertyStatus where
  isVerified (PropertyStatus negated result) = evaluateQuery negated result

instance Pretty PropertyStatus where
  pretty (PropertyStatus negated s) = do
    let witnessText = if negated then "counterexample" else "witness"
    let (verified, evidenceText) = case s of
          Trivial status -> (status `xor` negated, "(trivial)")
          NonTrivial status -> case status of
            UnSAT -> (negated, "proved no" <+> witnessText <+> "exists")
            SAT Nothing -> (not negated, "no" <> witnessText <+> "found")
            SAT Just {} -> (not negated, witnessText <+> "found")
    pretty (statusSymbol verified) <+> "-" <+> evidenceText

--------------------------------------------------------------------------------
-- Verification status of a multi property

type MultiPropertyStatus = MultiProperty PropertyStatus

instance IsVerified MultiPropertyStatus where
  isVerified = \case
    MultiProperty ps -> and (fmap isVerified ps)
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

prettyUserVariableAssignment :: (UserVariable, VariableValue) -> Doc a
prettyUserVariableAssignment (UserVariable {..}, variableValue) = do
  let name = pretty userVarName
  let valueExpr = assignmentToExpr userVarDimensions (Vector.toList variableValue)
  let value = prettyFriendly (WithContext valueExpr emptyDBCtx)
  name <> ":" <+> value

assignmentToExpr :: TensorDimensions -> [Double] -> Expr Ix StandardBuiltin
assignmentToExpr [] xs = RatLiteral mempty (toRational (head xs))
assignmentToExpr (dim : dims) xs = do
  let vecConstructor = Builtin mempty (CConstructor $ LVec dim)
  let inputVarIndicesChunks = chunksOf (product dims) xs
  let elems = fmap (ExplicitArg mempty . assignmentToExpr dims) inputVarIndicesChunks
  normAppList mempty vecConstructor elems

traverseMultiPropertySATResults ::
  forall m.
  (Monad m) =>
  (PropertyAddress -> QuerySetNegationStatus -> Maybe UserVariableAssignment -> m ()) ->
  MultiPropertyStatus ->
  m ()
traverseMultiPropertySATResults f = go
  where
    go :: MultiPropertyStatus -> m ()
    go = \case
      MultiProperty ps -> do
        forM_ ps go
      SingleProperty address (PropertyStatus negated p) -> case p of
        Trivial {} -> return ()
        NonTrivial UnSAT -> return ()
        NonTrivial (SAT assignment) -> do
          f address negated assignment

--------------------------------------------------------------------------------
-- Verification status of the specification

type SpecificationStatus = Specification PropertyStatus

instance IsVerified SpecificationStatus where
  isVerified (Specification properties) =
    and (fmap (isVerified . snd) properties)

instance Pretty SpecificationStatus where
  pretty spec@(Specification properties) = do
    let result = "Specification summary:" <+> (if isVerified spec then "true" else "false")
    result
      <> line
      <> indent 2 (vsep (fmap (uncurry prettyMultiPropertyStatus) properties))
