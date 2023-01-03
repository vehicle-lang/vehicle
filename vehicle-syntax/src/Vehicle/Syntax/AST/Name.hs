module Vehicle.Syntax.AST.Name where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Hashable (Hashable)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..))

--------------------------------------------------------------------------------
-- Definition

type Name = Text

--------------------------------------------------------------------------------
-- Module system

data Module
  = User
  | StdLib
  deriving (Eq, Ord, Show, Generic)

instance NFData Module

instance Hashable Module

instance FromJSON Module

instance ToJSON Module

instance Pretty Module where
  pretty = \case
    User -> "User"
    StdLib -> "Stdlib"

--------------------------------------------------------------------------------
-- Identifiers

data Identifier = Identifier Module Name
  deriving (Eq, Ord, Show, Generic)

instance Pretty Identifier where
  pretty (Identifier m s) = pretty m <> "." <> pretty s

instance NFData Identifier

instance Hashable Identifier

instance FromJSON Identifier

instance ToJSON Identifier

instance FromJSONKey Identifier

instance ToJSONKey Identifier

class HasIdentifier a where
  identifierOf :: a -> Identifier

moduleOf :: Identifier -> Module
moduleOf (Identifier m _) = m

identifierName :: Identifier -> Name
identifierName (Identifier _ n) = n

--------------------------------------------------------------------------------
-- Names

class HasName a name where
  nameOf :: a -> name

instance HasName Identifier Name where
  nameOf (Identifier mod name) = name
