module Vehicle.Language.AST.Visibility where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)

import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Definitions

-- | Visibility of function arguments.
data Visibility = Explicit | Implicit | Instance
  deriving (Eq, Ord, Show, Generic)

instance NFData Visibility

instance Pretty Visibility where
  pretty = \case
    Explicit -> "Explicit"
    Implicit -> "Implicit"
    Instance -> "Instance"

instance Hashable Visibility

visProv :: Visibility -> Provenance -> Provenance
visProv Explicit = id
visProv Implicit = expandProvenance (1,1)
visProv Instance = expandProvenance (2,2)

-- | Type class for types which have provenance information

class HasVisibility a where
  visibilityOf :: a -> Visibility

isExplicit :: HasVisibility a => a -> Bool
isExplicit x = visibilityOf x == Explicit