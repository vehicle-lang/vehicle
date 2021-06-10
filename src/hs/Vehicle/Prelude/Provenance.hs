{-# LANGUAGE FlexibleContexts #-}

module Vehicle.Prelude.Provenance
  ( Provenance(..)
  , tkProvenance
  ) where

import           Data.Range (Range)
import qualified Data.Range as Range

import           Vehicle.Prelude.Token ( tkRange, IsToken, Position, Token(..) )

newtype Provenance = Provenance { fromProvenance :: [Range Position] }
  deriving (Eq, Show)

instance Semigroup Provenance where
  r1 <> r2 = Provenance $ fromProvenance r1 `Range.union` fromProvenance r2

instance Monoid Provenance where
  mempty = Provenance []

-- |Get the provenance for a single token.
tkProvenance :: IsToken a => a -> Provenance
tkProvenance = Provenance . tkRange
