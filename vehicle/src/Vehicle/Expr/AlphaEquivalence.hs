{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Expr.AlphaEquivalence
  ( AlphaEquivalence (..),
  )
where

import Data.Hashable (Hashable (..))
import Vehicle.Expr.CoDeBruijn
import Vehicle.Expr.CoDeBruijn.Conversion
import Vehicle.Expr.DeBruijn

instance Hashable DBArg

instance Hashable DBExpr where
  hashWithSalt s e = hashWithSalt s (toCoDBExpr e)

class AlphaEquivalence a where
  alphaEq :: a -> a -> Bool

instance AlphaEquivalence CoDBExpr where
  alphaEq e1 e2 = hash e1 == hash e2

instance AlphaEquivalence DBExpr where
  alphaEq e1 e2 = hash e1 == hash e2
