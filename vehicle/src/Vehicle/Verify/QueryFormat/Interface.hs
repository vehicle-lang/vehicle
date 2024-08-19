module Vehicle.Verify.QueryFormat.Interface where

import Control.Monad.Except (MonadError)
import Data.Text (Text)
import Vehicle.Compile.Error (CompileError)
import Vehicle.Compile.Prelude (Doc, ExternalOutputFormat, MonadLogger)
import Vehicle.Data.QuantifiedVariable
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core

type FormatQuery =
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
    -- | The command to format an individual query
    formatQuery :: FormatQuery,
    compileVar :: forall a. NetworkRationalVariable -> Doc a
  }
