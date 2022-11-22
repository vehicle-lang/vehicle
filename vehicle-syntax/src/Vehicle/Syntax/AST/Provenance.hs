
module Vehicle.Syntax.AST.Provenance
  ( Provenance
  , tkProvenance
  , datasetProvenance
  , parameterProvenance
  , inserted
  , HasProvenance(..)
  , expandProvenance
  , fillInProvenance
  , wasInsertedByCompiler
  ) where

import Control.DeepSeq (NFData (..))
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (maybeToList)
import Data.Range hiding (joinRanges)
import GHC.Generics (Generic)
import Data.Hashable (Hashable (..))
import Prettyprinter (Pretty(..), (<+>), concatWith, squotes)
import Data.Text (Text)

import Vehicle.Syntax.Parse.Token

--------------------------------------------------------------------------------
-- Position

-- |A position in the source file is represented by a line number and a column
-- number.
--
-- Note we don't use the names `line` and `column` as they clash with the
-- `Prettyprinter` library.
data Position = Position
  { posLine   :: Int
  , posColumn :: Int
  } deriving (Eq, Ord)

instance Show Position where
  show (Position l c) = show (l, c)

instance Pretty Position where
  pretty (Position l c) = "Line" <+> pretty l <+> "Column" <+> pretty c

-- |Get the starting position of a token.
tkPosition :: IsToken a => a -> Position
tkPosition t = let (l, c) = tkLocation t in Position l c

alterColumn :: (Int -> Int) -> Position -> Position
alterColumn f (Position l c) = Position l (f c)

--------------------------------------------------------------------------------
-- Position ranges

-- All the code in this section relies on the assumption that we only use
-- inclusive span ranges in our code.

pattern IRange :: a -> a -> Range a
pattern IRange p1 p2 = SpanRange (Bound p1 Inclusive) (Bound p2 Inclusive)

instance Ord (Range Position) where
  IRange p1 p2 <= IRange p3 p4 = p1 < p3 || (p1 == p3 && p2 <= p4)
  _ <= _                       = True

instance Pretty (Range Position) where
  pretty (SpanRange (Bound p1 Inclusive) (Bound p2 Inclusive)) =
    if posLine p1 == posLine p2 then
      "Line" <+> pretty (posLine p1) <> "," <+>
      "Columns" <+> pretty (posColumn p1) <> "-" <> pretty (posColumn p2)
    else
      pretty p1 <+> "-" <+> pretty p2
  -- I think we exclusively use inclusive span ranges so for the moment
  -- don't do anything special when showing them.
  pretty r = pretty $ show r

-- | Takes the union of two sets of position ranges and then joins any adjacent
-- ranges (e.g. r1=[(1,10)-(1,20)] & r2=[(1,21)-(1,25)] gets mapped to
-- the single range [(1,10)-(1,25)]).
--
-- Ideally we would use the built in `joinRanges` from `Data.Range` to do this
-- but that works via the `Enum` type class and relies on the fact that
-- adjacent ranges are mapped to adjacent enum values. While it is possible to
-- construct an `Enum` instance for `Position` via a bijection between N
-- and N^2, it is not possible to construct such that adjacent ranges are mapped
-- to adjecent enum values.
--
-- TODO Ideally we submit a patch back to `Data.Range` modifying `joinRanges` to
-- take an arbitrary joining predicate such as `adjacent` below.
--
-- TODO this could be made more efficient by assuming that we have the invariant
-- that provenances are already merged, and therefore only merge the last range
joinRanges :: [Range Position] -> [Range Position] -> [Range Position]
joinRanges rs1 rs2 = combineRanges $ rs1 `union` rs2
  where
    adjacent :: Position -> Position -> Bool
    adjacent p1 p2 = posLine p1 == posLine p2 && 1 + posColumn p1 == posColumn p2

    -- Doesn't handle anything except inclusive ranges as that's all we use in our code
    -- at the moment.
    mergeRangePair :: Range Position -> Range Position -> Maybe (Range Position)
    mergeRangePair (SpanRange b1 (Bound p1 Inclusive)) (SpanRange (Bound p2 Inclusive) b2)
      | adjacent p1 p2 = Just $ SpanRange b1 b2
    mergeRangePair _r1 _r2 = Nothing

    -- Assumes that the ranges have been sorted by the `union` above
    combineRanges :: [Range Position] -> [Range Position]
    combineRanges (r1 : r2 : rs) = case mergeRangePair r1 r2 of
      Nothing  -> r1 : combineRanges (r2 : rs)
      Just r12 -> combineRanges (r12 : rs)
    combineRanges rs             = rs

fillInRanges :: [Range Position] -> Maybe (Range Position)
fillInRanges []  = Nothing
fillInRanges [r] = Just r
fillInRanges ranges  = let
  sortedRanges = sort ranges
  startRange   = head sortedRanges
  endRange     = last sortedRanges
  in case (startRange, endRange) of
    (SpanRange s _, SpanRange _ e) -> Just $ SpanRange s e
    _                              -> error "Invalid assumption that we only use span ranges"

expandRange :: (Int, Int) -> [Range Position] -> [Range Position]
expandRange (l , r) [IRange start end] = [IRange (alterColumn (\x -> x - l) start) (alterColumn (+ r) end)]
expandRange _       rs                 = rs

--------------------------------------------------------------------------------
-- Owner

data Owner
  = TheMachine
  | TheUser
  deriving (Show, Eq, Ord, Generic)

instance Semigroup Owner where
  TheUser    <> _ = TheUser
  TheMachine <> r = r

instance Monoid Owner where
  mempty = TheMachine

--------------------------------------------------------------------------------
-- Origin

-- |The origin of a piece of code
data Origin
  = FromSource [Range Position]
  -- ^ set of locations in the source file
  | FromParameter Text
  -- ^ name of the parameter
  | FromDataset Text
  deriving (Show, Eq, Ord, Generic)

instance Semigroup Origin where
  FromSource r1     <> FromSource r2   = FromSource (joinRanges r1 r2)
  p@FromSource{}    <> _               = p
  _                 <> p@FromSource{}  = p
  p@FromDataset{}   <> _               = p
  _                 <> p@FromDataset{} = p
  p@FromParameter{} <> _               = p

instance Monoid Origin where
  mempty = FromSource mempty

instance Pretty Origin where
  pretty (FromSource ranges) = case ranges of
    []  -> "no source location"
    [r] -> pretty r
    rs  -> concatWith (\u v -> u <+> "and" <+> v) (map pretty rs)
  pretty (FromParameter name) = "parameter" <+> squotes (pretty name)
  pretty (FromDataset   name) = "in dataset" <+> squotes (pretty name)

--------------------------------------------------------------------------------
-- Provenance

data Provenance = Provenance
  { origin :: Origin
  , owner  :: Owner
  } deriving (Generic)

instance Show Provenance where
  show = const ""

instance NFData Provenance where
  rnf _ = ()

instance Eq Provenance where
  _x == _y = True

instance Hashable Provenance where
  hashWithSalt s _p = s

-- |Get the provenance for a single token.
tkProvenance :: IsToken a => a -> Provenance
tkProvenance tk = Provenance (FromSource [start +=+ end]) TheUser
  where
    start = tkPosition tk
    end   = Position (posLine start) (posColumn start + tkLength tk)

datasetProvenance :: Text -> Provenance
datasetProvenance name = Provenance (FromDataset name) TheUser

parameterProvenance :: Text -> Provenance
parameterProvenance name = Provenance (FromParameter name) TheUser

-- | Marks the provenance as inserted by the compiler.
inserted :: Provenance -> Provenance
inserted (Provenance origin _owner) = Provenance origin TheMachine

fillInProvenance :: [Provenance] -> Provenance
fillInProvenance ps = Provenance (FromSource rs) TheUser
  where
    getRanges :: Provenance -> [Range Position]
    getRanges (Provenance (FromSource r) _) = r
    getRanges _                             = error
      "should not be filling in provenance on non-source file code"

    rs = maybeToList $ fillInRanges $ concatMap getRanges ps

expandProvenance :: (Int, Int) -> Provenance -> Provenance
expandProvenance w (Provenance (FromSource rs) o) = Provenance (FromSource (expandRange w rs)) o
expandProvenance _ p                              = p

instance Pretty Provenance where
  pretty (Provenance origin _) = pretty origin

instance Semigroup Provenance where
  Provenance origin1 owner1 <> Provenance origin2 owner2 =
    Provenance (origin1 <> origin2) (owner1 <> owner2)

instance Monoid Provenance where
  mempty = Provenance mempty mempty

--------------------------------------------------------------------------------
-- Type-classes

-- | Type class for types which have provenance information
class HasProvenance a where
  provenanceOf :: a -> Provenance

instance HasProvenance Provenance where
  provenanceOf = id

instance HasProvenance a => HasProvenance [a] where
  provenanceOf = foldMap provenanceOf

instance HasProvenance a => HasProvenance (NonEmpty a) where
  provenanceOf = foldMap provenanceOf

instance HasProvenance a => HasProvenance (a, b) where
  provenanceOf = provenanceOf . fst

wasInsertedByCompiler :: HasProvenance a => a -> Bool
wasInsertedByCompiler x = owner (provenanceOf x) == TheMachine
