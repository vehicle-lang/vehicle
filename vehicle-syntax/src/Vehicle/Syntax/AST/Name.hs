module Vehicle.Syntax.AST.Name where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON, ToJSONKey)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import Data.Serialize.Text ()
import Data.Text (Text)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..), concatWith, dot, surround)

--------------------------------------------------------------------------------
-- Definition

type Name = Text

--------------------------------------------------------------------------------
-- Module system

data Module
  = User
  | StdLib
  | Record Name
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
    Record name -> pretty name

newtype ModulePath = ModulePath
  { modules :: [Module]
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData ModulePath

instance Hashable ModulePath

instance ToJSON ModulePath

instance FromJSON ModulePath

instance Serialize ModulePath

instance Pretty ModulePath where
  pretty (ModulePath m) = concatWith (surround dot) (fmap pretty m)

--------------------------------------------------------------------------------
-- Identifiers

data Identifier = Identifier
  { modulePath :: ModulePath,
    identifierName :: Name
  }
  deriving (Eq, Ord, Show, Generic)

instance Pretty Identifier where
  pretty (Identifier m s) = pretty m <> dot <> pretty s

instance NFData Identifier

instance Hashable Identifier

instance ToJSON Identifier

instance FromJSON Identifier

instance ToJSONKey Identifier

instance Serialize Identifier

class HasIdentifier a where
  identifierOf :: a -> Identifier

stdlibIdentifier :: Name -> Identifier
stdlibIdentifier = Identifier (ModulePath [StdLib])

isUserIdent :: Identifier -> Bool
isUserIdent ident = User `elem` modules (modulePath ident)

changeName :: Identifier -> Name -> Identifier
changeName Identifier {..} newName = Identifier {identifierName = newName, ..}

--------------------------------------------------------------------------------
-- Names

class HasName a name | a -> name where
  nameOf :: a -> name

instance HasName Identifier Name where
  nameOf (Identifier _mod name) = name

instance HasName Name Name where
  nameOf = id
