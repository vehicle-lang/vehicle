module Vehicle.Prelude.Visibility where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Prettyprinter (Pretty(..), Doc, braces)

import Vehicle.Prelude.Provenance

--------------------------------------------------------------------------------
-- Definitions

-- | Visibility of function arguments
data Visibility = Explicit | Implicit | Constraint
  deriving (Eq, Ord, Show, Generic)

instance NFData Visibility

instance Pretty Visibility where
  pretty = \case
    Explicit -> "Explicit"
    Implicit -> "Implicit"
    Constraint -> "Constraint"

visBrackets :: Visibility -> Doc a -> Doc a
visBrackets Explicit   = id
visBrackets Implicit   = braces
visBrackets Constraint = braces . braces

visProv :: Visibility -> Provenance -> Provenance
visProv Explicit   = id
visProv Implicit   = expandProvenance (1,1)
visProv Constraint = expandProvenance (2,2)

--------------------------------------------------------------------------------
-- Type-classes

-- | Type class for types which have provenance information

class HasVisibility a where
  vis :: a -> Visibility