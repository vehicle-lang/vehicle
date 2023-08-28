module Vehicle.Verify.QueryFormat.Core where

import Control.Monad.Writer (MonadWriter)
import Data.Text (Text)
import Vehicle.Backend.Queries.LinearExpr
import Vehicle.Prelude
import Vehicle.Verify.Core

--------------------------------------------------------------------------------
-- Query formats

data QueryFormatID
  = MarabouQueries
  | VNNLibQueries
  deriving (Show, Eq, Bounded, Enum)

instance Pretty QueryFormatID where
  pretty = \case
    MarabouQueries -> "Marabou query format"
    VNNLibQueries -> "VNNLib query format"

-- | A format for an output query that verifiers can parse.
data QueryFormat = QueryFormat
  { queryFormatID :: QueryFormatID,
    queryOutputFormat :: ExternalOutputFormat,
    -- | The command to compile an individual query
    compileQuery :: forall m. (MonadLogger m, MonadWriter UnsoundStrictOrderConversion m) => CLSTProblem -> m Text
  }
