{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Prelude.Prettyprinter
  ( module CommonPrettyprinter,
    module Vehicle.Prelude.Prettyprinter,
  )
where

-- This stuff we re-export

import Data.Bifunctor (Bifunctor (..))
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap (toAscList)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet (toAscList)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map (toAscList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Version (Version, showVersion)
import Data.Void (Void)
import Numeric (showFFloat)
import Prettyprinter (group, line', surround, unAnnotate)
import Prettyprinter as CommonPrettyprinter
  ( Doc,
    Pretty (..),
    align,
    braces,
    concatWith,
    group,
    hang,
    indent,
    line,
    nest,
    parens,
    softline,
    squotes,
    unAnnotate,
    (<+>),
  )
import Prettyprinter.Internal (Doc (Annotated))

-- * Additions to the prettyprinter library

-- | A |Doc| without any annotations
type UnAnnDoc = Doc Void

--------------------------------------------------------------------------------
-- More flexible combinators

-- Redefining some pretty printer primitives to work with `Foldable`.
-- Can remove once https://github.com/quchen/prettyprinter/pull/200 is released.

hsep :: (Foldable t) => t (Doc ann) -> Doc ann
hsep = concatWith (<+>)

vsep :: (Foldable t) => t (Doc ann) -> Doc ann
vsep = concatWith (\x y -> x <> line <> y)

hcat :: (Foldable t) => t (Doc ann) -> Doc ann
hcat = concatWith (<>)

vcat :: (Foldable t) => t (Doc ann) -> Doc ann
vcat = concatWith (\x y -> x <> line' <> y)

commaSep :: (Foldable t) => t (Doc ann) -> Doc ann
commaSep = concatWith (surround ", ")

numberedList :: [Doc ann] -> Doc ann
numberedList elems = vsep (zipWith (\i e -> pretty i <> "." <+> e) [(1 :: Int) ..] elems)

lineIndent :: Doc ann -> Doc ann
lineIndent x = line <> indent 2 x

prettyFlatList :: [Doc ann] -> Doc ann
prettyFlatList xs = "[" <+> commaSep xs <+> "]"

prettyMultiLineList :: [Doc ann] -> Doc ann
prettyMultiLineList xs =
  "["
    <+> group (concatWith (\x y -> x <> line <> "," <+> y) xs)
    <> line
    <> "]"

prettyNonEmptyList :: NonEmpty (Doc ann) -> Doc ann
prettyNonEmptyList xs = case NonEmpty.init xs of
  [] -> NonEmpty.last xs
  ys -> commaSep ys <+> "and" <+> NonEmpty.last xs

-- | Shortens a list by turning [a1, a2, a3, a4, a5] into
-- [a1, a2, ... 2 more ..., a5]
dotDotList :: Int -> [Doc a] -> [Doc a]
dotDotList maxElems xs = do
  let n = length xs
  if n <= maxElems
    then xs
    else do
      let start = take (maxElems - 1) xs
      let middle = "..." <+> pretty (n - maxElems) <+> "more" <+> "..."
      let end = drop (n - 1) xs
      start <> [middle] <> end

--------------------------------------------------------------------------------
-- Useful utility functions

vsep2 :: (Foldable t) => t (Doc ann) -> Doc ann
vsep2 = concatWith (\x y -> x <> line <> line <> y)

docAnn :: Doc ann -> Maybe ann
docAnn (Annotated a _) = Just a
docAnn _ = Nothing

quotePretty :: (Pretty a) => a -> Doc b
quotePretty = squotes . pretty

--------------------------------------------------------------------------------
-- Pretty printing of datatypes

prettyMap :: (Pretty key, Pretty value) => Map key value -> Doc a
prettyMap = prettyMapEntries . fmap (bimap pretty pretty) . Map.toAscList

prettyMapEntries :: [(Doc a, Doc a)] -> Doc a
prettyMapEntries entries = prettySetLike entries'
  where
    (keys, values) = unzip entries
    entries' = zipWith (\k v -> k <+> ":=" <+> v) keys values

prettySet :: (Pretty value) => Set value -> Doc b
prettySet xs = prettySetLike (pretty <$> Set.toList xs)

prettySetLike :: [Doc a] -> Doc a
prettySetLike xs =
  "{"
    <+> concatWith (\x y -> x <> line <> ";" <+> y) xs
    <> line
    <> "}"

prettyRationalAsFloat :: Rational -> Doc a
prettyRationalAsFloat p = do
  let f = realToFrac p :: Double
  pretty $ showFFloat Nothing f ""

instance Pretty IntSet where
  pretty m = pretty (IntSet.toAscList m)

instance (Pretty a) => Pretty (IntMap a) where
  pretty = prettyMapEntries . fmap (bimap pretty pretty) . IntMap.toAscList

instance Pretty Version where
  pretty = pretty . showVersion

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty = \case
    Left x -> pretty x
    Right x -> pretty x
