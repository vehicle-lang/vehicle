
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
import Data.Hashable (Hashable (..))
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (maybeToList)
import Data.Range hiding (joinRanges)
import Data.Text (Text)
import GHC.Generics (Generic(..))
import Prettyprinter (Pretty (..), concatWith, squotes, (<+>))

import Vehicle.Syntax.Parse.Token
import Data.Aeson (FromJSON(..), ToJSON (..))

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
  } deriving (Eq, Ord, Generic)

instance Show Position where
  show (Position l c) = show (l, c)

instance Pretty Position where
  pretty (Position l c) = "Line" <+> pretty l <+> "Column" <+> pretty c

instance ToJSON   Position
instance FromJSON Position

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

-- Doesn't handle anything except inclusive ranges as that's all we use in our code
-- at the moment.
mergeRangePair :: Range Position -> Range Position -> Range Position
mergeRangePair (IRange b1 _) (IRange _ b2) = IRange b1 b2
mergeRangePair _ _ = error "Invalid assumption that we only use span ranges"

fillInRanges :: [Range Position] -> Range Position
fillInRanges []  = mempty
fillInRanges [r] = r
fillInRanges ranges  = let
  sortedRanges = sort ranges
  startRange   = head sortedRanges
  endRange     = last sortedRanges
  in case (startRange, endRange) of
    (SpanRange s _, SpanRange _ e) -> SpanRange s e
    _                              -> error "Invalid assumption that we only use span ranges"

expandRange :: (Int, Int) -> Range Position -> Range Position
expandRange (l , r) (IRange start end) = IRange (alterColumn (\x -> x - l) start) (alterColumn (+ r) end)
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

instance ToJSON   Owner
instance FromJSON Owner


--------------------------------------------------------------------------------
-- Origin

-- |The origin of a piece of code
data Origin
  = FromSource (Range Position)
  -- ^ set of locations in the source file
  | FromParameter Text
  -- ^ name of the parameter
  | FromDataset Text
  deriving (Show, Eq, Ord, Generic)

instance Semigroup Origin where
  FromSource r1     <> FromSource r2   = FromSource (mergeRangePair r1 r2)
  p@FromSource{}    <> _               = p
  _                 <> p@FromSource{}  = p
  p@FromDataset{}   <> _               = p
  _                 <> p@FromDataset{} = p
  p@FromParameter{} <> _               = p

instance Monoid Origin where
  mempty = FromSource mempty

instance Pretty Origin where
  pretty = \case
    FromSource    range -> pretty range
    FromParameter name  -> "parameter" <+> squotes (pretty name)
    FromDataset   name  -> "in dataset" <+> squotes (pretty name)

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

instance ToJSON   Provenance where
  toJSON _ = toJSON ()

instance FromJSON Provenance where
  parseJSON _ = return mempty

instance Eq Provenance where
  _x == _y = True

instance Hashable Provenance where
  hashWithSalt s _p = s

-- |Get the provenance for a single token.
tkProvenance :: IsToken a => a -> Provenance
tkProvenance tk = Provenance (FromSource (start +=+ end)) TheUser
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

fillInProvenance :: NonEmpty Provenance -> Provenance
fillInProvenance ps = Provenance (FromSource rs) TheUser
  where
    getRanges :: Provenance -> Range Position
    getRanges (Provenance (FromSource r) _) = r
    getRanges _                             = error
      "should not be filling in provenance on non-source file code"

    rs = fillInRanges $ map getRanges ps

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
