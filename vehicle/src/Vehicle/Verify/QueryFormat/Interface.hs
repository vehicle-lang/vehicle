module Vehicle.Verify.QueryFormat.Interface where

import Control.Monad.Except (MonadError)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Vehicle.Compile.Error (CompileError)
import Vehicle.Compile.Prelude (Coefficient, ExternalOutputFormat, InputOrOutput, MonadLogger, Pretty (..))
import Vehicle.Data.Code.BooleanExpr (ConjunctAll)
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core

-- | Returns the string representation used by the query format
-- for a given network variable.
type CompileQueryVariable =
  -- Does this variable represent an input or an output
  InputOrOutput ->
  -- The shape of the overall tensor that contains the element variable.
  -- TensorShape ->
  -- The location of the element variable in the overall tensor.
  -- TensorIndices ->
  -- The position of the variable in the list of all input/output variables
  Int ->
  -- The returned string representation.
  QueryVariable

-- A single assertion for a query.
data QueryAssertion variable = QueryAssertion
  { lhs :: !(NonEmpty (Coefficient, variable)),
    rel :: !QueryRelation,
    rhs :: !Rational
  }

instance (Pretty variable) => Pretty (QueryAssertion variable) where
  pretty (QueryAssertion lhs rel rhs) = pretty lhs <> pretty rel <> pretty rhs

-- | The contents of a single query for a verifier.
data QueryContents = QueryContents
  { queryVariables :: [QueryVariable],
    queryAssertions :: ConjunctAll (QueryAssertion QueryVariable)
  }

-- | The command to format an individual query
type CompileQuery =
  forall m.
  (MonadLogger m, MonadError CompileError m) =>
  QueryAddress ->
  QueryContents ->
  m Text

-- | A format for an output query that verifiers can parse.
data QueryFormat = QueryFormat
  { queryFormatID :: QueryFormatID,
    queryOutputFormat :: ExternalOutputFormat,
    supportsStrictInequalities :: Bool,
    compileVariable :: CompileQueryVariable,
    compileQuery :: CompileQuery
  }
