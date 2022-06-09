module Vehicle.Compile.AlphaEquivalence
  ( AlphaEquivalence(..)
  , hashDBExpr
  , hashCoDBExpr
  ) where

import Data.Hashable (Hashable(..))

import Vehicle.Compile.Prelude
import Vehicle.Compile.CoDeBruijnify

hashCoDBExpr :: CoDBExpr -> Int
hashCoDBExpr = hash

hashDBExpr :: DBExpr -> Int
hashDBExpr e = hashCoDBExpr (toCoDBExpr e)

class AlphaEquivalence a where
  alphaEq :: a -> a -> Bool

instance AlphaEquivalence CoDBExpr where
  alphaEq e1 e2 = hashCoDBExpr e1 == hashCoDBExpr e2

instance AlphaEquivalence DBExpr where
  alphaEq e1 e2 = hashDBExpr e1 == hashDBExpr e2