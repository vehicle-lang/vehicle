{-# LANGUAGE CPP #-}


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
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson.Types (Parser, Value)
import Data.Hashable (Hashable (..))
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (maybeToList)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prettyprinter (Doc, Pretty (..), concatWith, squotes, (<+>))
import Vehicle.Syntax.Parse.Token (IsToken, Token (Tk), tkLength, tkLocation)

#if nothunks
import NoThunks.Class (NoThunks)
#endif

--------------------------------------------------------------------------------
-- Position

-- | A position in the source file is represented by a line number and a column
-- number.
--
-- Note we don't use the names `line` and `column` as they clash with the
-- `Prettyprinter` library.
data Position = Position
  { posLine   :: !Int,
    posColumn :: !Int
  } deriving (Eq, Ord, Generic)

instance Show Position where
  show (Position l c) = show (l, c)

instance Pretty Position where
  pretty (Position l c) = "Line" <+> pretty l <+> "Column" <+> pretty c

#if nothunks
instance NoThunks Position
#endif

instance ToJSON Position

instance FromJSON Position

-- | Get the starting position of a token.
tkPosition :: IsToken a => a -> Position
tkPosition t = let (l, c) = tkLocation t in Position l c

alterColumn :: (Int -> Int) -> Position -> Position
alterColumn f (Position l c) = Position l (f c)

--------------------------------------------------------------------------------
-- Position ranges

-- All the code in this section relies on the assumption that we only use
-- inclusive span ranges in our code.

data Range = Range
  { start :: !Position,
    end   :: !Position
  } deriving (Show, Eq, Generic)

#if nothunks
instance NoThunks Range
#endif

instance Ord Range where
  Range s1 e1 <= Range s2 e2 = s1 < s2 || (s1 == s2 && e1 <= e1)

instance Pretty Range where
  pretty (Range p1 p2) =
    if posLine p1 == posLine p2 then
      "Line" <+> pretty (posLine p1) <> "," <+>
      "Columns" <+> pretty (posColumn p1) <> "-" <> pretty (posColumn p2)
    else
      pretty p1 <+> "-" <+> pretty p2

-- Doesn't handle anything except inclusive ranges as that's all we use in our code
-- at the moment.
mergeRangePair :: Range -> Range -> Range
mergeRangePair (Range b1 _) (Range _ b2) = Range b1 b2

expandRange :: (Int, Int) -> Range -> Range
expandRange (l, r) (Range start end) =
  Range (alterColumn (\x -> x - l) start) (alterColumn (+ r) end)

--------------------------------------------------------------------------------
-- Owner

data Owner
  = TheMachine
  | TheUser
  deriving (Show, Eq, Ord, Generic)

instance Semigroup Owner where
  TheUser <> _    = TheUser
  TheMachine <> r = r

instance Monoid Owner where
  mempty = TheMachine

#if nothunks
instance NoThunks Owner
#endif

instance ToJSON Owner

instance FromJSON Owner

--------------------------------------------------------------------------------
-- Origin

-- | The origin of a piece of code
data Origin
  -- | set of locations in the source file
  = FromSource !Range
  -- | name of the parameter
  | FromParameter !Text
  | FromDataset !Text
  deriving (Show, Eq, Ord, Generic)

#if nothunks
instance NoThunks Origin
#endif

instance Semigroup Origin where
  FromSource r1     <> FromSource r2   = FromSource (mergeRangePair r1 r2)
  p@FromSource{}    <> _               = p
  _                 <> p@FromSource{}  = p
  p@FromDataset{}   <> _               = p
  _                 <> p@FromDataset{} = p
  p@FromParameter{} <> _               = p

instance Monoid Origin where
  mempty = FromSource (Range (Position 0 0) (Position 0 0))

instance Pretty Origin where
  pretty = \case
    FromSource    range -> pretty range
    FromParameter name  -> "parameter" <+> squotes (pretty name)
    FromDataset   name  -> "in dataset" <+> squotes (pretty name)

--------------------------------------------------------------------------------
-- Provenance

data Provenance = Provenance
  { origin :: !Origin
  , owner  :: !Owner
  } deriving (Generic)

instance Show Provenance where
  show = const ""

instance NFData Provenance where
  rnf _ = ()

#if nothunks
instance NoThunks Provenance
#endif

instance ToJSON Provenance where
  toJSON _ = toJSON ()

instance FromJSON Provenance where
  parseJSON _ = return mempty

instance Eq Provenance where
  _x == _y = True

instance Hashable Provenance where
  hashWithSalt s _p = s

-- | Get the provenance for a single token.
tkProvenance :: IsToken a => a -> Provenance
tkProvenance tk = Provenance (FromSource (Range start end)) TheUser
  where
    start = tkPosition tk
    end = Position (posLine start) (posColumn start + tkLength tk)

datasetProvenance :: Text -> Provenance
datasetProvenance name = Provenance (FromDataset name) TheUser

parameterProvenance :: Text -> Provenance
parameterProvenance name = Provenance (FromParameter name) TheUser

-- | Marks the provenance as inserted by the compiler.
inserted :: Provenance -> Provenance
inserted (Provenance origin _owner) = Provenance origin TheMachine

fillInProvenance :: NonEmpty Provenance -> Provenance
fillInProvenance provenances = do
  let (starts, ends) = NonEmpty.unzip (fmap getPositions provenances)
  let start = minimum starts
  let end = maximum ends
  Provenance (FromSource (Range start end)) TheUser
  where
    getPositions :: Provenance -> (Position, Position)
    getPositions (Provenance origin _) = case origin of
      FromSource (Range start end) -> (start, end)
      _ -> error
        "Should not be filling in provenance from non-source file locations"

expandProvenance :: (Int, Int) -> Provenance -> Provenance
expandProvenance w (Provenance (FromSource rs) o) = Provenance (FromSource (expandRange w rs)) o
expandProvenance _ p = p

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
