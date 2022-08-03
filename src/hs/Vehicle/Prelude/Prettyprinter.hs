{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Prelude.Prettyprinter
  ( module CommonPrettyprinter
  , module Vehicle.Prelude.Prettyprinter
  )
  where

import Data.Text (Text)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet (toAscList)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap (toAscList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Map (Map)
import Data.Map qualified as Map (toAscList)
import Data.Version (Version, showVersion)

import Prettyprinter ( line', unAnnotate, layoutPretty, defaultLayoutOptions, surround, group)
import Prettyprinter.Internal (Doc(Annotated))
import Prettyprinter.Render.String (renderString)
import Prettyprinter.Render.Text (renderStrict)

-- This stuff we re-export
import Prettyprinter as CommonPrettyprinter
  ( Pretty(..)
  , Doc
  , (<+>)
  , parens
  , braces
  , concatWith
  , softline
  , line
  , squotes
  , align
  , nest
  , indent
  , unAnnotate
  )

-- * Additions to the prettyprinter library

--------------------------------------------------------------------------------
-- More flexible combinators

-- Redefining some pretty printer primitives to work with `Foldable`.
-- Can remove once https://github.com/quchen/prettyprinter/pull/200 is released.

hsep :: Foldable t => t (Doc ann) -> Doc ann
hsep = concatWith (<+>)

vsep :: Foldable t => t (Doc ann) -> Doc ann
vsep = concatWith (\x y -> x <> line <> y)

hcat :: Foldable t => t (Doc ann) -> Doc ann
hcat = concatWith (<>)

vcat :: Foldable t => t (Doc ann) -> Doc ann
vcat = concatWith (\x y -> x <> line' <> y)

commaSep :: Foldable t => t (Doc ann) -> Doc ann
commaSep = concatWith (surround ", ")

numberedList :: [Doc ann] -> Doc ann
numberedList elems = vsep (zipWith (\i e -> pretty i <> "." <+> e) [(1::Int)..] elems)

prettyFlatList :: [Doc ann] -> Doc ann
prettyFlatList xs = "[" <+> commaSep xs <+> "]"

--------------------------------------------------------------------------------
-- Useful utility functions

vsep2 :: Foldable t => t (Doc ann) -> Doc ann
vsep2 = concatWith (\x y -> x <> line <> line <> y)

docAnn :: Doc ann -> Maybe ann
docAnn (Annotated a _) = Just a
docAnn _               = Nothing

layoutAsString :: Doc ann -> String
layoutAsString = renderString . layoutPretty defaultLayoutOptions

layoutAsText :: Doc ann -> Text
layoutAsText = renderStrict . layoutPretty defaultLayoutOptions

--------------------------------------------------------------------------------
-- Pretty printing of datatypes

prettyIntMap :: IntMap (Doc a) -> Doc a
prettyIntMap = prettyMapEntries . IntMap.toAscList

prettyMap :: (Pretty key, Pretty value) => Map key value -> Doc a
prettyMap = prettyMapEntries . Map.toAscList . fmap pretty

prettyMapEntries :: Pretty key => [(key, Doc a)] -> Doc a
prettyMapEntries entries = result
  where
  (keys, values) = unzip entries
  keys' = fmap pretty keys
  entries' = zipWith (\k v -> k <+> ":=" <+> v) keys' values
  result = "{" <+> align (group
    (concatWith (\x y -> x <> ";" <> line <> y) entries')
    <> softline <> "}")

prettySet :: Pretty value => Set value -> Doc b
prettySet xs = prettySetLike (pretty <$> Set.toList xs)

prettySetLike :: [Doc a] -> Doc a
prettySetLike xs = "{" <+> align (group
  (concatWith (\x y -> x <> ";" <> line <> y) xs)
  <> softline <> "}")

instance Pretty Rational where
  pretty p = pretty (fromRational p :: Double)
  --pretty (numerator p) <> "/" <> pretty (denominator p)

instance Pretty IntSet where
  pretty m = pretty (IntSet.toAscList m)

instance Pretty a => Pretty (IntMap a) where
  pretty = prettyIntMap . fmap pretty

instance Pretty Version where
  pretty = pretty . showVersion