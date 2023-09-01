module Vehicle.Verify.QueryFormat
  ( QueryFormatID (..),
    QueryFormat (..),
    queryFormats,
    marabouQueryFormat,
    vnnlibQueryFormat,
  )
where

import Vehicle.Verify.QueryFormat.Core
import Vehicle.Verify.QueryFormat.Interface
import Vehicle.Verify.QueryFormat.Marabou (marabouQueryFormat)
import Vehicle.Verify.QueryFormat.VNNLib (vnnlibQueryFormat)

queryFormats :: QueryFormatID -> QueryFormat
queryFormats = \case
  MarabouQueries -> marabouQueryFormat
  VNNLibQueries -> vnnlibQueryFormat
