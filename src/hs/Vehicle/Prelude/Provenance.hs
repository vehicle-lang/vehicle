{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Vehicle.Prelude.Provenance
  ( Provenance(..)
  , tkProvenance
  , HasProvenance(..)
  ) where

import           Data.Range (Range)
import qualified Data.Range as Range

import           Vehicle.Prelude.Token ( tkRange, IsToken, Position, Token(..) )
import           Vehicle.Prelude.Types
import           Vehicle.Prelude.Sort

-- | A set of locations in the source file
newtype Provenance = Provenance { fromProvenance :: [Range Position] }
  deriving (Eq, Show)

instance Semigroup Provenance where
  r1 <> r2 = Provenance $ fromProvenance r1 `Range.union` fromProvenance r2

instance Monoid Provenance where
  mempty = Provenance []

-- | Class for types which have provenance information

-- | TODO - Replace with general class `Has` once we have indexed lists?
class HasProvenance a where
  prov :: a -> Provenance

instance HasProvenance Provenance where
  prov = id

instance HasProvenance a => HasProvenance [a] where
  prov xs = foldMap prov xs

instance HasProvenance a => HasProvenance (K a s) where
  prov (K x) = prov x

instance (KnownSort s, HasProvenance (a s)) => HasProvenance ((a :*: b) s) where
  prov (x :*: _y) = prov x

-- |Get the provenance for a single token.
tkProvenance :: IsToken a => a -> Provenance
tkProvenance = Provenance . tkRange
