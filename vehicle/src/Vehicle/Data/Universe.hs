module Vehicle.Data.Universe where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Universes

newtype UniverseLevel = UniverseLevel Int
  deriving (Eq, Ord, Show, Generic)

instance NFData UniverseLevel

instance Hashable UniverseLevel

instance Serialize UniverseLevel

instance Pretty UniverseLevel where
  pretty (UniverseLevel l) = "Type" <+> pretty l
