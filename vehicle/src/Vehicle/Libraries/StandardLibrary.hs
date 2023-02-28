module Vehicle.Libraries.StandardLibrary
  ( standardLibrary,
    findStdLibFunction,
    pattern TensorIdent,
    StdLibFunction (..),
  )
where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text, pack)
import Data.Version (Version)
import Prettyprinter (Pretty (..))
import Vehicle.Libraries
import Vehicle.Syntax.AST

stdlibName :: LibraryName
stdlibName = "stdlib"

stdlibVersion :: Version
stdlibVersion = [0, 8]

standardLibrary :: Library
standardLibrary =
  Library
    { libraryInfo =
        LibraryInfo
          { libraryName = stdlibName,
            libraryVersion = stdlibVersion
          },
      libraryContent = content
    }

content :: Text
content =
  "--------------\n\
  \-- Foldable --\n\
  \--------------\n\
  \\n\
  \bigAnd : forallT {f : Type -> Type} . {{HasFold f}} -> f Bool -> Bool\n\
  \bigAnd = fold (\\x y -> x and y) True\n\
  \\n\
  \bigOr : forallT {f : Type -> Type} . {{HasFold f}} -> f Bool -> Bool\n\
  \bigOr = fold (\\x y -> x or y) False\n\
  \\n\
  \forallIn : forallT {f : Type -> Type} . {{HasFold f}} -> {{HasMap f}} -> (A -> Bool) -> f A -> Bool\n\
  \forallIn f xs = bigAnd (map f xs)\n\
  \\n\
  \existsIn : forallT {f : Type -> Type} . {{HasFold f}} -> {{HasMap f}} -> (A -> Bool) -> f A -> Bool\n\
  \existsIn f xs = bigOr (map f xs)\n\
  \\n\
  \------------\n\
  \-- Vector --\n\
  \------------\n\
  \\n\
  \vectorToVector : forallT {n} {A} . Vector A n -> Vector A n\n\
  \vectorToVector xs = xs\n\
  \\n\
  \mapVector : forallT {n} {A} {B} . (A -> B) -> Vector A n -> Vector B n\n\
  \mapVector {n} {A} {B} f = dfold {A} {n} {Vector B} (\\{l} x xs -> f x ::v xs) []\n\
  \\n\
  \foreachVector : (Index n -> A) -> Vector A n\n\
  \foreachVector {A} {n} f = map f (indices n)\n\
  \\n\
  \zipWith : (A -> B -> C) -> Vector A n -> Vector B n -> Vector C n\n\
  \zipWith f xs ys = foreach i . f (xs ! i) (ys ! i)\n\
  \\n\
  \addVector : {{HasAdd A B C}} -> Vector A n -> Vector B n -> Vector C n\n\
  \addVector = zipWith (\\x y -> x + y)\n\
  \\n\
  \subVector : {{HasSub A B C}} -> Vector A n -> Vector B n -> Vector C n\n\
  \subVector = zipWith (\\x y -> x - y)\n\
  \\n\
  \equalsVector : {{HasEq A B}} -> Vector A n -> Vector B n -> Bool\n\
  \equalsVector xs ys = bigAnd (zipWith (\\x y -> x == y) xs ys)\n\
  \\n\
  \notEqualsVector : {{HasNotEq A B}} -> Vector A n -> Vector B n -> Bool\n\
  \notEqualsVector xs ys = bigOr (zipWith (\\x y -> x != y) xs ys)\n\
  \\n\
  \-----------\n\
  \-- Index --\n\
  \-----------\n\
  \\n\
  \existsIndex : (Index n -> Bool) -> Bool\n\
  \existsIndex f = bigOr (foreach i . f i)\n\
  \\n\
  \forallIndex : (Index n -> Bool) -> Bool\n\
  \forallIndex f = bigAnd (foreach i . f i)\n\
  \\n\
  \------------\n\
  \-- Tensor --\n\
  \------------\n\
  \\n\
  \Tensor : Type -> List Nat -> Type\n\
  \Tensor A ds = fold (\\d t -> Vector t d) A ds\n\
  \\n\
  \----------\n\
  \-- List --\n\
  \----------\n\
  \\n\
  \vectorToList : forallT {n} {A} . Vector A n -> List A\n\
  \vectorToList = fold (\\x xs -> x :: xs) nil\n\
  \\n\
  \mapList : (A -> B) -> List A -> List B\n\
  \mapList f = fold (\\x xs -> f x :: xs) nil\n\
  \"

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
