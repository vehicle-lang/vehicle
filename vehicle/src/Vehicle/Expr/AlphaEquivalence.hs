{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Expr.AlphaEquivalence
  ( AlphaEquivalence (..),
  )
where

import Data.Hashable (Hashable (..))
import Vehicle.Compile.Prelude
import Vehicle.Expr.CoDeBruijn
import Vehicle.Expr.CoDeBruijn.Conversion
import Vehicle.Expr.Normalised

instance Hashable CheckedArg

instance Hashable CheckedExpr where
  hashWithSalt s e = hashWithSalt s (toCoDBExpr e)

instance Hashable BasicNormArg

instance Hashable BasicNormBinder

instance Hashable BasicNormExpr

class AlphaEquivalence a where
  alphaEq :: a -> a -> Bool

instance AlphaEquivalence CoDBExpr where
  alphaEq e1 e2 = hash e1 == hash e2

instance AlphaEquivalence CheckedExpr where
  alphaEq e1 e2 = hash e1 == hash e2
