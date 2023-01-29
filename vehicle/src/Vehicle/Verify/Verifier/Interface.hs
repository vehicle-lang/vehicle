module Vehicle.Verify.Verifier.Interface where

import Control.Monad.IO.Class (MonadIO)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Queries.LinearExpr
import Vehicle.Compile.Queries.Variable
import Vehicle.Compile.Queries.VariableReconstruction
import Vehicle.Verify.Core
import Vehicle.Verify.Specification
import Vehicle.Verify.Specification.Status (SatisfiabilityStatus)

-- | The type of methods that compile queries for a verifier
type VerifierCompile =
  forall m.
  MonadLogger m =>
  CLSTProblem NetworkVariable ->
  m QueryText

-- | The type of methods to call a verifier on a query
type VerifierInvocation =
  forall m.
  MonadIO m =>
  VerifierExecutable ->
  MetaNetwork ->
  UserVarReconstructionInfo ->
  QueryFile ->
  m SatisfiabilityStatus

-- | A complete verifier implementation
data Verifier = Verifier
  { -- | The identifier for the verifier within Vehicle itself
    verifierIdentifier :: VerifierIdentifier,
    -- | The name of the executable for the verifier
    verifierExecutableName :: String,
    -- | The command to invoke the verifier
    invokeVerifier :: VerifierInvocation,
    -- | The command to compile an individual query
    compileQuery :: VerifierCompile
  }
