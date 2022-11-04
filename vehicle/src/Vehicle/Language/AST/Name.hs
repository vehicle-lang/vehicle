module Vehicle.Language.AST.Name where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.Text (pack)
import GHC.Generics (Generic)

import Vehicle.Language.AST.Binder
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Definition

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
-- Type class

class HasName a name where
  nameOf :: a -> name

freshNames :: [Name]
freshNames = [ "_x" <> pack (show i) | i <- [0::Int ..]]

instance HasName (GenericBinder binder expr) binder where
  nameOf = binderRepresentation

instance HasName Identifier Name where
  nameOf (Identifier name) = name
