module Vehicle.Libraries.StandardLibrary.Names where

import Data.Map (Map)
import Data.Map qualified as Map (fromList, lookup)
import Data.Text (pack)
import Vehicle.Prelude
import Vehicle.Syntax.AST

data StdLibFunction
  = StdExistsIndex
  | StdForallIndex
  | StdExistsInList
  | StdForallInList
  | StdExistsInVector
  | StdForallInVector
  | StdEqualsBool
  | StdNotEqualsBool
  | StdEqualsVector
  | StdNotEqualsVector
  | StdAddVector
  | StdSubVector
  deriving (Eq, Enum, Bounded)

instance Show StdLibFunction where
  show = \case
    StdExistsIndex -> "existsIndex"
    StdForallIndex -> "forallIndex"
    StdAddVector -> "addVector"
    StdSubVector -> "subVector"
    StdExistsInList -> "existsInList"
    StdForallInList -> "forallInList"
    StdExistsInVector -> "existsInVector"
    StdForallInVector -> "forallInVector"
    StdEqualsBool -> "equalsBool"
    StdNotEqualsBool -> "notEqualsBool"
    StdEqualsVector -> "equalsVector"
    StdNotEqualsVector -> "notEqualsVector"

instance Pretty StdLibFunction where
  pretty = pretty . show

instance HasIdentifier StdLibFunction where
  identifierOf f = Identifier StdLib $ pack $ show f

stdLibFunctions :: Map Name StdLibFunction
stdLibFunctions = Map.fromList $ fmap (\f -> (pack $ show f, f)) [minBound .. maxBound]

findStdLibFunction :: Name -> Maybe StdLibFunction
findStdLibFunction name = Map.lookup name stdLibFunctions
