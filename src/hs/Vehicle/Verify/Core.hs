module Vehicle.Verify.Core where

import Vehicle.Prelude

data VerifierIdentifier
  = Marabou
  deriving (Eq, Ord, Show, Read)

instance Pretty VerifierIdentifier where
  pretty = pretty . show

-- | Location of a verifier query file.
type QueryFile = FilePath

-- | Location of the verifier executable file
type VerifierExecutable = FilePath
