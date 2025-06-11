module Vehicle.Verify.QueryFormat.Interface where

import Control.Monad.Except (MonadError)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Vehicle.Compile.Error (CompileError)
import Vehicle.Compile.Prelude (Coefficient, ExternalOutputFormat, InputOrOutput, MonadLogger, Name, Pretty (..))
import Vehicle.Data.Code.BooleanExpr (ConjunctAll)
import Vehicle.Data.Tensor (TensorIndices, TensorShape)
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core

-- | Returns the string representation used by the query format
-- for a given network variable.
type CompileQueryVariable =
  -- The position of the variable in the list of all input/output variables
  QueryVariableInfo ->
  -- The returned string representation.
  QueryVariable

data QueryVariableInfo = QueryVariableInfo
  { inputOrOutput :: InputOrOutput,
    networkName :: Name,
    numberOfNetworkApps :: Int,
    networkAppIndex :: Int,
    parentVariableShape :: TensorShape,
    parentVariableIndices :: TensorIndices
  }

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
    supportsMultipleNetworks :: Bool,
    compileVariable :: CompileQueryVariable,
    compileQuery :: CompileQuery
  }
