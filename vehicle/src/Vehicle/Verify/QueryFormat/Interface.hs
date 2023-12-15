module Vehicle.Verify.QueryFormat.Interface where

import Control.Monad.Except (MonadError)
import Data.Text (Text)
import Vehicle.Backend.Queries.LinearExpr
import Vehicle.Compile.Error (CompileError)
import Vehicle.Compile.Prelude (DeclProvenance, ExternalOutputFormat, MonadLogger)
import Vehicle.Verify.QueryFormat.Core

-- | A format for an output query that verifiers can parse.
data QueryFormat = QueryFormat
  { queryFormatID :: QueryFormatID,
    queryOutputFormat :: ExternalOutputFormat,
    -- | The command to compile an individual query
    compileQuery ::
      forall m.
      (MonadLogger m, MonadError CompileError m) =>
      DeclProvenance ->
      CLSTProblem ->
      m Text
  }
