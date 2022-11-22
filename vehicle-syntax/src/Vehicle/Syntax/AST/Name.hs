module Vehicle.Syntax.AST.Name where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Prettyprinter (Pretty(..))

--------------------------------------------------------------------------------
-- Definition

type Name = Text

-- | Bindings when using the named representation of the AST.
type NamedBinding = Name

--------------------------------------------------------------------------------
-- Identifiers

newtype Identifier = Identifier Name
  deriving (Eq, Ord, Show, Generic)

instance Pretty Identifier where
  pretty (Identifier s) = pretty s

instance NFData   Identifier
instance Hashable Identifier

class HasIdentifier a where
  identifierOf :: a -> Identifier

--------------------------------------------------------------------------------
-- Names

class HasName a name where
  nameOf :: a -> name

instance HasName Identifier Name where
  nameOf (Identifier name) = name
