module Vehicle.Verify.Verifier
  ( VerifierID (..),
    Verifier (..),
    verifiers,
    marabouVerifier,
    VerifierExecutable,
  )
where

import Vehicle.Verify.Verifier.Core
import Vehicle.Verify.Verifier.Marabou (marabouVerifier)

verifiers :: VerifierID -> Verifier
verifiers = \case
  Marabou -> marabouVerifier
