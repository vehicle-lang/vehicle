module Vehicle.Verify.Verifier
  ( verifiers
  ) where

import Vehicle.Verify.Core
import Vehicle.Verify.Verifier.Interface
import Vehicle.Verify.Verifier.Marabou (marabouVerifier)

verifiers :: VerifierIdentifier -> Verifier
verifiers = \case
  Marabou -> marabouVerifier
