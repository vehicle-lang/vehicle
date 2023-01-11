module Vehicle.Syntax.AST.Visibility where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (Hashable (..))
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..))
import Vehicle.Syntax.AST.Provenance

--------------------------------------------------------------------------------
-- Definitions

-- | Whether the visible object was inserted by the
type InsertedByCompiler = Bool

-- | Visibility of function arguments.
data Visibility
  = -- | Always have to be given explicitly
    Explicit
  | -- | Inferred via unification
    Implicit InsertedByCompiler
  | -- | Inferred via instance search/type class resolution
    Instance InsertedByCompiler
  deriving (Show, Generic)

instance Eq Visibility where
  Explicit {} == Explicit {} = True
  Implicit {} == Implicit {} = True
  Instance {} == Instance {} = True
  _ == _ = False

instance Ord Visibility where
  Explicit {} <= _ = True
  Implicit {} <= Implicit {} = True
  Implicit {} <= Instance {} = True
  Instance {} <= Instance {} = True
  _ <= _ = False

instance NFData Visibility

instance Hashable Visibility where
  hashWithSalt s = \case
    Explicit {} -> s + 1
    Implicit {} -> s + 2
    Instance {} -> s + 3

instance ToJSON Visibility

instance FromJSON Visibility

instance Pretty Visibility where
  pretty = \case
    Explicit -> "Explicit"
    Implicit {} -> "Implicit"
    Instance {} -> "Instance"

-- | Type class for types which have provenance information
class HasVisibility a where
  visibilityOf :: a -> Visibility

instance HasVisibility Visibility where
  visibilityOf = id

isExplicit :: HasVisibility a => a -> Bool
isExplicit x = visibilityOf x == Explicit

isImplicit :: HasVisibility a => a -> Bool
isImplicit x = case visibilityOf x of
  Implicit {} -> True
  _ -> False

isInstance :: HasVisibility a => a -> Bool
isInstance x = case visibilityOf x of
  Instance {} -> True
  _ -> False

visibilityMatches :: (HasVisibility a, HasVisibility b) => a -> b -> Bool
visibilityMatches x y = visibilityOf x == visibilityOf y

expandByArgVisibility :: Visibility -> Provenance -> Provenance
expandByArgVisibility Explicit {} = id
expandByArgVisibility Implicit {} = expandProvenance (1, 1)
expandByArgVisibility Instance {} = expandProvenance (2, 2)
