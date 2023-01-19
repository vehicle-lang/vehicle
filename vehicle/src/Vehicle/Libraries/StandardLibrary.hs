module Vehicle.Libraries.StandardLibrary
  ( standardLibrary,
    pattern TensorIdent,
  )
where

import Data.Text (Text)
import Data.Version (Version)
import Vehicle.Libraries
import Vehicle.Syntax.AST (Identifier (..), Module (..))

stdlibName :: LibraryName
stdlibName = "stdlib"

stdlibVersion :: Version
stdlibVersion = [0, 7]

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
  "------------\n\
  \-- Vector --\n\
  \------------\n\
  \\n\
  \zipWith : (A -> B -> C) -> Vector A n -> Vector B n -> Vector C n\n\
  \zipWith f xs ys = foreach i . f (xs ! i) (ys ! i)\n\
  \\n\
  \bigAnd : Vector Bool n -> Bool\n\
  \bigAnd = fold (\\x y -> x and y) True\n\
  \\n\
  \bigOr : Vector Bool n -> Bool\n\
  \bigOr = fold (\\x y -> x or y) False\n\
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
  \"

pattern TensorIdent :: Identifier
pattern TensorIdent = Identifier StdLib "Tensor"
