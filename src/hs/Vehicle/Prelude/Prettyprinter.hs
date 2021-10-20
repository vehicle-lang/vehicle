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

import Prettyprinter ( line', unAnnotate, layoutPretty, defaultLayoutOptions)
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
  , group
  , softline
  , line
  , squotes
  , align
  , nest
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

instance Pretty IntSet where
  pretty m = pretty (IntSet.toAscList m)

instance Pretty (IntMap (Doc b)) where
  pretty m = result
    where
      entries = IntMap.toAscList m
      (keys, values) = unzip entries
      keys' = fmap pretty keys
      entries' = zipWith (\k v -> k <+> ":=" <+> v) keys' values

      result = unAnnotate $ "{" <+> align (group
        (concatWith (\x y -> x <> ";" <> line <> y) entries')
        <> softline <> "}")