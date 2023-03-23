module Vehicle.Verify.Verifier
  ( verifiers,
    queryFormats,
  )
where

import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Marabou (marabouQueryFormat)
import Vehicle.Verify.Verifier.Marabou (marabouVerifier)

queryFormats :: QueryFormatID -> QueryFormat
queryFormats = \case
  MarabouQueryFormat -> marabouQueryFormat

verifiers :: VerifierID -> Verifier
verifiers = \case
  Marabou -> marabouVerifier
