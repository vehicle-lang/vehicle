module Vehicle.Language.AST.Name where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Names

data Name
  = User Symbol  -- User-generated name
  | Machine      -- Automatically generated name
  deriving (Eq, Ord, Show, Generic)

instance NFData Name

instance Pretty Name where
  pretty (User symbol) = pretty symbol
  pretty Machine       = "Machine"

class HasName a where
  nameOf :: a -> Name

--------------------------------------------------------------------------------
-- Identifiers

newtype Identifier = Identifier Symbol
  deriving (Eq, Ord, Show, Generic)

instance Pretty Identifier where
  pretty (Identifier s) = pretty s

instance NFData Identifier

class HasIdentifier a where
  identifierOf :: a -> Identifier