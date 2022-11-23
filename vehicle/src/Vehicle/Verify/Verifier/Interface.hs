module Vehicle.Verify.Verifier.Interface where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Vehicle.Compile.Queries.Linearity
import Vehicle.Compile.Prelude
import Vehicle.Compile.Resource
import Vehicle.Verify.Core
import Vehicle.Verify.Specification.Status (SatisfiabilityStatus)

-- | The type of methods that compile queries for a verifier
type VerifierCompile
  = forall m . MonadLogger m
  => CLSTProblem
  -> m (Doc ())

-- | The type of methods to call a verifier on a query
type VerifierInvocation
  = forall m. MonadIO m
  => VerifierExecutable
  -> NetworkLocations
  -> MetaNetwork
  -> UserVarReconstructionInfo
  -> QueryFile
  -> m SatisfiabilityStatus

-- | A complete verifier implementation
data Verifier = Verifier
  { verifierIdentifier     :: VerifierIdentifier
  -- ^ The identifier for the verifier within Vehicle itself
  , verifierExecutableName :: String
  -- ^ The name of the executable for the verifier
  , invokeVerifier         :: VerifierInvocation
  -- ^ The command to invoke the verifier
  , compileQuery           :: VerifierCompile
  -- ^ The command to compile an individual query
  , magicVariablePrefixes  :: (Text, Text)
  -- ^ Magic variables (should be folded into compileQuery)
  }
