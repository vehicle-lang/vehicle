module Vehicle.Verify.QueryFormat
  ( QueryFormat (..),
    QueryFormatID,
    queryFormats,
  )
where

import Vehicle.Verify.QueryFormat.Core
import Vehicle.Verify.QueryFormat.Marabou (marabouQueryFormat)
import Vehicle.Verify.QueryFormat.VNNLib (vnnlibQueryFormat)

queryFormats :: QueryFormatID -> QueryFormat
queryFormats = \case
  MarabouQueries -> marabouQueryFormat
  VNNLibQueries -> vnnlibQueryFormat
