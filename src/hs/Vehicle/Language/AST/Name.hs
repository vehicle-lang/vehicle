module Vehicle.Language.AST.Name where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Vehicle.Prelude

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