{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Expr.AlphaEquivalence
  ( AlphaEquivalence (..),
  )
where

import Data.Hashable (Hashable (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Expr.CoDeBruijn
import Vehicle.Expr.CoDeBruijn.Conversion

instance Hashable TypeCheckedArg

instance Hashable TypeCheckedExpr where
  hashWithSalt s e = hashWithSalt s (toCoDBExpr e)

instance Hashable StandardNormArg

instance Hashable StandardNormBinder

instance Hashable StandardNormExpr

class AlphaEquivalence a where
  alphaEq :: a -> a -> Bool

instance AlphaEquivalence CoDBExpr where
  alphaEq e1 e2 = hash e1 == hash e2

instance AlphaEquivalence TypeCheckedExpr where
  alphaEq e1 e2 = hash e1 == hash e2
