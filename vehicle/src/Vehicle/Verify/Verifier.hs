module Vehicle.Verify.Verifier
  ( verifiers,
    queryFormats,
  )
where

import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Marabou (marabouQueryFormat)
import Vehicle.Verify.QueryFormat.VNNLib (vnnlibQueryFormat)
import Vehicle.Verify.Verifier.Marabou (marabouVerifier)

queryFormats :: QueryFormatID -> QueryFormat
queryFormats = \case
  MarabouQueries -> marabouQueryFormat
  VNNLibQueries -> vnnlibQueryFormat

verifiers :: VerifierID -> Verifier
verifiers = \case
  Marabou -> marabouVerifier
