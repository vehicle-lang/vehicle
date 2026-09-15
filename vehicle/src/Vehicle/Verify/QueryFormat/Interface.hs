module Vehicle.Verify.QueryFormat.Interface where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Vehicle.Compile.Prelude (ExternalOutputFormat, InputOrOutput, MonadLogger, Name)
import Vehicle.Data.Bound (BoundedValue, Domain)
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
  { -- | Name of the network this query variable belongs to
    networkName :: Name,
    -- | Total number of applications of the network
    numberOfNetworkApps :: Int,
    -- | The index of the the network application this variable belongs to
    networkAppIndex :: Int,
    -- | Does the variable represent an input or output tensor
    inputOrOutput :: InputOrOutput,
    -- | The shape of the tensor
    parentVariableShape :: TensorShape,
    -- | The indices into the tensor to get the element of the tensor.
    parentVariableIndices :: TensorIndices
  }

-- A single assertion for a query.
data QueryAssertion variable = QueryAssertion
  { lhs :: !(NonEmpty (Rational, variable)),
    rel :: !QueryRelation,
    rhs :: !Rational
  }

-- Bounds on query variables, grouped by the network they belong to.
type QueryVariableBounds = [BoundedValue QueryVariable (Domain Rational)]

-- | The command to format an individual query
type CompileQuery =
  forall m.
  (MonadLogger m) =>
  QueryAddress ->
  MetaNetwork ->
  [QueryVariable] ->
  QueryVariableBounds ->
  ConjunctAll (QueryAssertion QueryVariable) ->
  m Text

-- | A format for an output query that solvers can parse.
data QueryFormat = QueryFormat
  { queryFormatID :: QueryFormatID,
    queryOutputFormat :: ExternalOutputFormat,
    supportsStrictInequalities :: Bool,
    supportsMultipleNetworks :: Bool,
    compileVariable :: CompileQueryVariable,
    compileQuery :: CompileQuery
  }
