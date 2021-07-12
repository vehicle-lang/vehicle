
module Vehicle.Prelude.Prettyprinter where

import Prettyprinter ( Doc, (<+>), concatWith, line, line' )
import Prettyprinter.Internal (Doc(Annotated))

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