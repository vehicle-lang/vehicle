{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Vehicle.Prelude.Provenance
  ( Provenance(..)
  , tkProvenance
  , HasProvenance(..)
  ) where

import Data.Range (Range)
import Data.Range qualified as Range
import Prettyprinter

import Vehicle.Prelude.Token ( tkRange, IsToken, Position, Token(..) )
import Vehicle.Prelude.Types ( K(K) )

-- | A set of locations in the source file
newtype Provenance = Provenance { fromProvenance :: [Range Position] }
  deriving (Eq, Show)

instance Semigroup Provenance where
  r1 <> r2 = Provenance $ fromProvenance r1 `Range.union` fromProvenance r2

instance Monoid Provenance where
  mempty = Provenance []

instance Pretty Provenance where
  -- TODO probably need to do something more elegant here.
  pretty = pretty . show

-- | Class for types which have provenance information

-- | TODO - Replace with general class `Has` once we have indexed lists?
class HasProvenance a where
  prov :: a -> Provenance

instance HasProvenance Provenance where
  prov = id

instance (HasProvenance a , Foldable t) => HasProvenance (t a) where
  prov xs = foldMap prov xs

instance HasProvenance a => HasProvenance (K a s) where
  prov (K x) = prov x

-- |Get the provenance for a single token.
tkProvenance :: IsToken a => a -> Provenance
tkProvenance = Provenance . tkRange
