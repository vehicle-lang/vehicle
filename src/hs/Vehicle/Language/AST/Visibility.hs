module Vehicle.Language.AST.Visibility where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)

import Vehicle.Prelude
import Vehicle.Language.AST.Provenance

--------------------------------------------------------------------------------
-- Definitions

-- | Visibility of function arguments.
data Visibility
  = Explicit
  -- ^ Always have to be given explicitly
  | Implicit
  -- ^ Inferred via unification
  | Instance
  -- ^ Inferred via instance search/type class resolution
  deriving (Eq, Ord, Show, Generic)

instance NFData Visibility
instance Hashable Visibility

instance Pretty Visibility where
  pretty = \case
    Explicit   -> "Explicit"
    Implicit   -> "Implicit"
    Instance{} -> "Instance"


-- | Type class for types which have provenance information

class HasVisibility a where
  visibilityOf :: a -> Visibility

instance HasVisibility Visibility where
  visibilityOf = id

isExplicit :: HasVisibility a => a -> Bool
isExplicit x = visibilityOf x == Explicit

isImplicit :: HasVisibility a => a -> Bool
isImplicit x = visibilityOf x == Implicit

isInstance :: HasVisibility a => a -> Bool
isInstance x = case visibilityOf x of
  Instance{} -> True
  _          -> False

visibilityMatches :: (HasVisibility a, HasVisibility b) => a -> b -> Bool
visibilityMatches x y = visibilityOf x == visibilityOf y

expandByArgVisibility :: Visibility -> Provenance -> Provenance
expandByArgVisibility Explicit{} = id
expandByArgVisibility Implicit{} = expandProvenance (1,1)
expandByArgVisibility Instance{} = expandProvenance (2,2)