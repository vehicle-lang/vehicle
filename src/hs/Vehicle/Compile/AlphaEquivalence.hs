module Vehicle.Compile.AlphaEquivalence
  ( AlphaEquivalence(..)
  , hashDBExpr
  , hashCoDBExpr
  ) where

import Data.Hashable (Hashable(..))

import Vehicle.Language.AST
import Vehicle.Compile.CoDeBruijnify

hashCoDBExpr :: CoDBExpr ann -> Int
hashCoDBExpr e = hash (mkHashable e)

hashDBExpr :: DBExpr ann -> Int
hashDBExpr e = hashCoDBExpr (toCoDBExpr e)

class AlphaEquivalence a where
  alphaEq :: a -> a -> Bool

instance AlphaEquivalence (CoDBExpr ann) where
  alphaEq e1 e2 = hashCoDBExpr e1 == hashCoDBExpr e2

instance AlphaEquivalence (DBExpr ann) where
  alphaEq e1 e2 = hashDBExpr e1 == hashDBExpr e2