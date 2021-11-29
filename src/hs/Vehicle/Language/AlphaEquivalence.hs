module Vehicle.Language.AlphaEquivalence
  ( alphaEq
  ) where

import Data.Hashable (Hashable(..))

import Vehicle.Language.AST

hashDB :: DeBruijnExpr ann -> Int
hashDB e = hash $ fmap fst (toCodebruijn e)

alphaEq :: DeBruijnExpr ann -> DeBruijnExpr ann -> Bool
alphaEq e1 e2 = hashDB e1 == hashDB e2