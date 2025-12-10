module Vehicle.Syntax.AST.Name where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON (..), ToJSON (..), ToJSONKey)
import Data.Hashable (Hashable)
import Data.List (intercalate)
import Data.List.Split (splitOn)
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

newtype ModulePath = ModulePath
  { path :: [String]
  }
  deriving (Eq, Ord, Generic)

instance NFData ModulePath

instance Hashable ModulePath

instance Serialize ModulePath

instance Pretty ModulePath where
  pretty (ModulePath path) = concatWith (surround dot) (fmap pretty path)

instance Show ModulePath where
  show (ModulePath path) = intercalate "." path

instance ToJSON ModulePath where
  toJSON = toJSON . show

instance FromJSON ModulePath where
  parseJSON value = readModulePath <$> parseJSON value

readModulePath :: String -> ModulePath
readModulePath s = ModulePath $ splitOn "." s

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

instance HasIdentifier Identifier where
  identifierOf = id

userModulePath :: ModulePath
userModulePath = ModulePath ["User"]

stdlibIdentifier :: Name -> Identifier
stdlibIdentifier = Identifier (ModulePath ["Definitions"])

isUserCode :: (HasIdentifier a) => a -> Bool
isUserCode object = modulePath (identifierOf object) == userModulePath

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
