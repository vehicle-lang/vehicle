module Vehicle.Verify.QueryFormat.Interface where

import Data.Text (Text)
import Vehicle.Backend.Queries.LinearExpr
import Vehicle.Prelude
import Vehicle.Prelude.Logging
import Vehicle.Syntax.AST (Name)
import Vehicle.Verify.QueryFormat.Core

-- | A format for an output query that verifiers can parse.
data QueryFormat = QueryFormat
  { queryFormatID :: QueryFormatID,
    queryOutputFormat :: ExternalOutputFormat,
    -- | The command to compile an individual query
    compileQuery ::
      forall m.
      (MonadLogger m) =>
      Name ->
      CLSTProblem ->
      m Text
  }
