module Vehicle.Verify.QueryFormat.Core where

import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Query formats

data QueryFormatID
  = MarabouQueries
  | VNNLibQueries
  deriving (Show, Eq, Ord, Bounded, Enum, Read)

instance Pretty QueryFormatID where
  pretty = \case
    MarabouQueries -> "Marabou query format"
    VNNLibQueries -> "VNN-LIB"

-- | A variable used in a query.
-- In a one-to-one correspondence with `NetworkIOElementVariable`
type QueryVariable = Name
