module Vehicle.Backend.VNNLib.Core where

import Vehicle.Prelude
import Vehicle.Compile.Normalise.NetworkApplications (MetaNetwork)

--------------------------------------------------------------------------------
-- Data

data VNNLibProperty = VNNLibProperty
  { name        :: Symbol      -- ^ The name of the property
  , doc         :: Doc ()      -- ^ The problem to feed to the verifier
  , vars        :: [VNNVar]    -- ^ The list of variables in the property
  , negated     :: Bool        -- ^ Is the property negated?
  , metaNetwork :: MetaNetwork -- ^ Description of the network applications
  }

data VNNVar = VNNVar
  { name    :: Symbol      -- ^ Name of the variable
  , typ     :: VNNVarType  -- ^ Type of the variable
  }

data VNNVarType
  = VReal

instance Pretty VNNVarType where
  pretty VReal = "Real"
