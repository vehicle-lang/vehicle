
module Vehicle.Language.StandardLibrary.Names where

import Data.Map (Map)
import Data.Map qualified as Map (fromList, lookup)
import Data.Text (pack)

import Vehicle.Prelude
import Vehicle.Language.AST.Core

data StdLibFunction
  = StdExistsBool
  | StdForallBool
  | StdExistsIndex
  | StdForallIndex
  | StdExistsVector
  | StdForallVector
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
    StdExistsBool      -> "existsBool"
    StdForallBool      -> "forallBool"
    StdExistsIndex     -> "existsIndex"
    StdForallIndex     -> "forallIndex"
    StdExistsVector    -> "existsVector"
    StdForallVector    -> "forallVector"
    StdExistsInList    -> "existsInList"
    StdForallInList    -> "forallInList"
    StdExistsInVector  -> "existsInVector"
    StdForallInVector  -> "forallInVector"
    StdEqualsBool      -> "equalsBool"
    StdNotEqualsBool   -> "notEqualsBool"
    StdEqualsVector    -> "equalsVector"
    StdNotEqualsVector -> "notEqualsVector"
    StdAddVector       -> "addVector"
    StdSubVector       -> "subVector"

instance Pretty StdLibFunction where
  pretty = pretty . show

instance HasIdentifier StdLibFunction where
  identifierOf f = Identifier $ pack $ show f

stdLibFunctions :: Map Name StdLibFunction
stdLibFunctions = Map.fromList $ fmap (\f -> (pack $ show f, f)) [minBound .. maxBound]

findStdLibFunction :: Name -> Maybe StdLibFunction
findStdLibFunction name = Map.lookup name stdLibFunctions

pattern PostulateExistsNat, PostulateForallNat :: Identifier
pattern PostulateExistsNat = Identifier "existsNat"
pattern PostulateForallNat = Identifier "forallNat"

pattern PostulateExistsInt, PostulateForallInt :: Identifier
pattern PostulateExistsInt = Identifier "existsInt"
pattern PostulateForallInt = Identifier "forallInt"

pattern PostulateExistsRat, PostulateForallRat :: Identifier
pattern PostulateExistsRat = Identifier "existsRat"
pattern PostulateForallRat = Identifier "forallRat"
