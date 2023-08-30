module Vehicle.Verify.Verifier.Core where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Vehicle.Compile.Prelude
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core

--------------------------------------------------------------------------------
-- Verifiers

data VerifierID
  = Marabou
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

instance Pretty VerifierID where
  pretty = pretty . show

-- | Location of the verifier executable file
type VerifierExecutable = FilePath

-- | The type of methods to call a verifier on a query
type VerifierInvocation =
  forall m.
  (MonadLogger m, MonadIO m) =>
  VerifierExecutable ->
  MetaNetwork ->
  QueryFile ->
  m (Either Text (QueryResult NetworkVariableAssignment))

-- | A complete verifier implementation
data Verifier = Verifier
  { -- | The identifier for the verifier within Vehicle itself
    verifierIdentifier :: VerifierID,
    -- | The query format that the verifier accepts
    verifierQueryFormat :: QueryFormatID,
    -- | The name of the executable for the verifier
    verifierExecutableName :: String,
    -- | The command to invoke the verifier
    invokeVerifier :: VerifierInvocation
  }
