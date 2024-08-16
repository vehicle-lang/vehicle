module Vehicle.Syntax.AST.Name where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON, ToJSONKey)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import Data.Serialize.Text ()
import Data.Text (Text)
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

instance FromJSON Module

instance Serialize Module

instance Pretty Module where
  pretty = \case
    User -> "User"
    StdLib -> "StdLib"

--------------------------------------------------------------------------------
-- Identifiers

data Identifier = Identifier Module Name
  deriving (Eq, Ord, Show, Generic)

instance Pretty Identifier where
  pretty (Identifier m s) = pretty m <> "." <> pretty s

instance NFData Identifier

instance Hashable Identifier

instance ToJSON Identifier

instance FromJSON Identifier

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

class HasName a name | a -> name where
  nameOf :: a -> name

instance HasName Identifier Name where
  nameOf (Identifier _mod name) = name

instance HasName Name Name where
  nameOf = id
