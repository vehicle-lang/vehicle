{-# LANGUAGE TemplateHaskell #-}

module Vehicle.Libraries.StandardLibrary
  ( standardLibrary,
    findStdLibFunction,
    pattern TensorIdent,
    StdLibFunction (..),
    isFiniteQuantifier,
    toFiniteQuantifier,
    fromFiniteQuantifier,
  )
where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.Text (pack)
import Data.Text.Encoding (decodeUtf8)
import Vehicle.Libraries
import Vehicle.Prelude
import Vehicle.Syntax.AST

standardLibraryContentBS :: ByteString
standardLibraryContentBS = $(makeRelativeToProject "lib/std.vcl" >>= embedFile)

standardLibrary :: Library
standardLibrary =
  Library
    { libraryInfo =
        LibraryInfo
          { libraryName = "std",
            libraryVersion = preciseVehicleVersion
          },
      libraryContent = decodeUtf8 standardLibraryContentBS
    }

pattern TensorIdent :: Identifier
pattern TensorIdent = Identifier StdLib "Tensor"

data StdLibFunction
  = StdExistsIndex
  | StdForallIndex
  | StdExistsIn
  | StdForallIn
  | StdEqualsBool
  | StdNotEqualsBool
  | StdVectorToVector
  | StdEqualsVector
  | StdNotEqualsVector
  | StdAddVector
  | StdSubVector
  | StdMapVector
  | StdMapList
  | StdVectorToList
  | StdForeach
  | StdTensor
  deriving (Eq, Enum, Bounded)

instance Show StdLibFunction where
  show = \case
    StdMapList -> "mapList"
    StdMapVector -> "mapVector"
    StdExistsIndex -> "existsIndex"
    StdForallIndex -> "forallIndex"
    StdAddVector -> "addVector"
    StdSubVector -> "subVector"
    StdExistsIn -> "existsIn"
    StdForallIn -> "forallIn"
    StdEqualsBool -> "equalsBool"
    StdNotEqualsBool -> "notEqualsBool"
    StdEqualsVector -> "equalsVector"
    StdNotEqualsVector -> "notEqualsVector"
    StdVectorToVector -> "vectorToVector"
    StdVectorToList -> "vectorToList"
    StdForeach -> "foreachVector"
    StdTensor -> "Tensor"

instance Pretty StdLibFunction where
  pretty = pretty . show

instance HasIdentifier StdLibFunction where
  identifierOf f = Identifier StdLib $ pack $ show f

stdLibFunctions :: Map Name StdLibFunction
stdLibFunctions = Map.fromList $ fmap (\f -> (pack $ show f, f)) [minBound .. maxBound]

findStdLibFunction :: Identifier -> Maybe StdLibFunction
findStdLibFunction ident = Map.lookup (nameOf ident) stdLibFunctions

isFiniteQuantifier :: Identifier -> Bool
isFiniteQuantifier = isJust . toFiniteQuantifier

toFiniteQuantifier :: Identifier -> Maybe Quantifier
toFiniteQuantifier ident
  | ident == identifierOf StdForallIndex = Just Forall
  | ident == identifierOf StdExistsIndex = Just Exists
  | otherwise = Nothing

fromFiniteQuantifier :: Quantifier -> Identifier
fromFiniteQuantifier = \case
  Forall -> identifierOf StdForallIndex
  Exists -> identifierOf StdExistsIndex
