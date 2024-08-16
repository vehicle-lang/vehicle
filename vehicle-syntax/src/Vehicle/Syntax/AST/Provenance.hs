
module Vehicle.Syntax.AST.Provenance
  ( Provenance (..),
    tkProvenance,
    noProvenance,
    HasProvenance (..),
    expandProvenance,
    fillInProvenance,

    -- * Exported for 'Vehicle.Syntax.AST.Instances.NoThunks'
    Position (..),
    Range (..),
  )
where

import Control.DeepSeq (NFData (..))
import Data.Hashable (Hashable (..))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Serialize (Serialize)
import GHC.Generics (Generic (..))
import Prettyprinter (Pretty (..), (<+>))
import Vehicle.Syntax.AST.Name (Module (..))
import Vehicle.Syntax.Parse.Token

--------------------------------------------------------------------------------
-- Position

-- | A position in the source file is represented by a line number and a column
--  number.
--
--  Note we don't use the names `line` and `column` as they clash with the
--  `Prettyprinter` library.
data Position = Position
  { posLine :: Int,
    posColumn :: Int
  }
  deriving (Eq, Ord, Generic)

instance Show Position where
  show (Position l c) = show (l, c)

instance Pretty Position where
  pretty (Position l c) = "Line" <+> pretty l <+> "Column" <+> pretty c

instance Serialize Position

-- | Get the starting position of a token.
tkPosition :: (IsToken a) => a -> Position
tkPosition t = let (l, c) = tkLocation t in Position l c

alterColumn :: (Int -> Int) -> Position -> Position
alterColumn f (Position l c) = Position l (f c)

--------------------------------------------------------------------------------
-- Position ranges

-- All the code in this section relies on the assumption that we only use
-- inclusive span ranges in our code.

data Range = Range
  { start :: Position,
    end :: Position
  }
  deriving (Show, Eq, Generic)

instance Ord Range where
  Range s1 e1 <= Range s2 e2 = s1 < s2 || (s1 == s2 && e1 <= e2)

instance Semigroup Range where
  Range b1 _ <> Range _ b2 = Range b1 b2

instance Monoid Range where
  mempty = Range (Position 0 0) (Position 0 0)

instance Pretty Range where
  pretty (Range p1 p2) =
    if posLine p1 == posLine p2
      then
        "Line"
          <+> pretty (posLine p1)
          <> ","
          <+> "Columns"
          <+> pretty (posColumn p1)
          <> "-"
          <> pretty (posColumn p2)
      else pretty p1 <+> "-" <+> pretty p2

instance Serialize Range

expandRange :: (Int, Int) -> Range -> Range
expandRange (l, r) (Range {..}) =
  Range (alterColumn (\x -> x - l) start) (alterColumn (+ r) end)

--------------------------------------------------------------------------------
-- Provenance

data Provenance = Provenance
  { range :: Range,
    modul :: Module
  }
  deriving (Generic)

instance Show Provenance where
  show = const ""

instance NFData Provenance where
  rnf _ = ()

instance Eq Provenance where
  _x == _y = True

instance Hashable Provenance where
  hashWithSalt s _p = s

instance Serialize Provenance

-- | Get the provenance for a single token.
tkProvenance :: (IsToken a) => Module -> a -> Provenance
tkProvenance modl tk = Provenance (Range start end) modl
  where
    start = tkPosition tk
    end = Position (posLine start) (posColumn start + tkLength tk)

fillInProvenance :: NonEmpty Provenance -> Provenance
fillInProvenance provenances = do
  let (starts, ends) = NonEmpty.unzip (fmap getPositions provenances)
  let start = minimum starts
  let end = maximum ends
  Provenance (Range start end) (modul $ NonEmpty.head provenances)
  where
    getPositions :: Provenance -> (Position, Position)
    getPositions (Provenance (Range start end) _) = (start, end)

expandProvenance :: (Int, Int) -> Provenance -> Provenance
expandProvenance w (Provenance range o) = Provenance (expandRange w range) o

instance Pretty Provenance where
  pretty (Provenance origin modl) = case modl of
    User -> pretty origin
    StdLib -> pretty modl <> "," <+> pretty origin

instance Semigroup Provenance where
  Provenance origin1 owner1 <> Provenance origin2 _owner2 =
    Provenance (origin1 <> origin2) owner1

noProvenance :: Provenance
noProvenance = Provenance mempty User

instance Monoid Provenance where
  mempty = Provenance mempty User

--------------------------------------------------------------------------------
-- Type-classes

-- | Type class for types which have provenance information
class HasProvenance a where
  provenanceOf :: a -> Provenance

instance HasProvenance Provenance where
  provenanceOf = id

instance (HasProvenance a) => HasProvenance [a] where
  provenanceOf = foldMap provenanceOf

instance (HasProvenance a) => HasProvenance (NonEmpty a) where
  provenanceOf = foldMap provenanceOf

instance (HasProvenance a) => HasProvenance (a, b) where
  provenanceOf = provenanceOf . fst
