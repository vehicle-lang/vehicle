module Vehicle.Prelude.Visibility where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Prettyprinter (Doc, braces)

import Vehicle.Prelude.Provenance

--------------------------------------------------------------------------------
-- Definitions

-- | Visibility of function arguments
data Visibility = Explicit | Implicit | Constraint
  deriving (Eq, Ord, Show, Generic, NFData)

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
  visibility :: a -> Visibility