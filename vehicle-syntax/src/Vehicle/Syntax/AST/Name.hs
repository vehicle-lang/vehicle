module Vehicle.Syntax.AST.Name where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..))
import Data.Aeson (FromJSON, ToJSON, FromJSONKey, ToJSONKey)

--------------------------------------------------------------------------------
-- Definition

type Name = Text

-- | Bindings when using the named representation of the AST.
type NamedBinding = Name

--------------------------------------------------------------------------------
-- Module system

data Module
  = User
  | StdLib
  deriving (Eq, Ord, Show, Generic)

instance NFData   Module
instance Hashable Module
instance FromJSON Module
instance ToJSON   Module

instance Pretty Module where
  pretty = \case
    User   -> "User"
    StdLib -> "Stdlib"

--------------------------------------------------------------------------------
-- Identifiers

data Identifier = Identifier Module Name
  deriving (Eq, Ord, Show, Generic)

instance Pretty Identifier where
  pretty (Identifier m s) = pretty m <> "." <> pretty s

instance NFData      Identifier
instance Hashable    Identifier
instance FromJSON    Identifier
instance ToJSON      Identifier
instance FromJSONKey Identifier
instance ToJSONKey   Identifier

class HasIdentifier a where
  identifierOf :: a -> Identifier

moduleOf :: Identifier -> Module
moduleOf (Identifier m _) = m

--------------------------------------------------------------------------------
-- Names

class HasName a name where
  nameOf :: a -> name

instance HasName Identifier Name where
  nameOf (Identifier mod name) = name
