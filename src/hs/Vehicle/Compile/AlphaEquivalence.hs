module Vehicle.Compile.AlphaEquivalence
  ( alphaEq
  , hashDB
  ) where

import Data.Hashable (Hashable(..))

import Vehicle.Language.AST
import Vehicle.Compile.CoDeBruijnify

hashDB :: DBExpr ann -> Int
hashDB e = hash $ toHashableCodebruijn e

alphaEq :: DBExpr ann -> DBExpr ann -> Bool
alphaEq e1 e2 = hashDB e1 == hashDB e2