module Vehicle.Verify.QueryFormat.Core where

import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Query formats

data QueryFormatID
  = MarabouQueries
  | VNNLibQueries
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Pretty QueryFormatID where
  pretty = \case
    MarabouQueries -> "Marabou query format"
    VNNLibQueries -> "VNNLib query format"

-- | A variable used in a query.
type QueryVariable = Name
