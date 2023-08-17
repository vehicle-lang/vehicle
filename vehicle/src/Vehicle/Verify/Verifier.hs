module Vehicle.Verify.Verifier
  ( Verifier (..),
    VerifierID,
    verifiers,
    VerifierExecutable,
  )
where

import Vehicle.Verify.Verifier.Core
import Vehicle.Verify.Verifier.Marabou (marabouVerifier)

verifiers :: VerifierID -> Verifier
verifiers = \case
  Marabou -> marabouVerifier
