module Vehicle.Syntax.AST.Name where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSONKey, ToJSON, ToJSONKey)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import Data.Serialize.Text ()
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

instance ToJSON Module

instance Serialize Module

instance Pretty Module where
  pretty = \case
    User -> "User"
    StdLib -> "Standard library"

--------------------------------------------------------------------------------
-- Identifiers

data Identifier = Identifier Module Name
  deriving (Eq, Ord, Show, Generic)

instance Pretty Identifier where
  pretty (Identifier m s) = pretty m <> "." <> pretty s

instance NFData Identifier

instance Hashable Identifier

instance ToJSON Identifier

instance ToJSONKey Identifier

instance Serialize Identifier

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
