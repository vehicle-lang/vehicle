module Vehicle.Verify.Verifier
  ( VerifierID (..),
    Verifier (..),
    verifiers,
    marabouVerifier,
    printVerifierError,
    VerifierExecutable,
  )
where

import Vehicle.Verify.Verifier.Core
import Vehicle.Verify.Verifier.Marabou (marabouVerifier)
import Vehicle.Verify.Verifier.Test (testVerifier)

verifiers :: VerifierID -> Verifier
verifiers = \case
  Marabou -> marabouVerifier
  TestVerifier -> testVerifier
