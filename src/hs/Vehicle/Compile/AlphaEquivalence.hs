module Vehicle.Compile.AlphaEquivalence
  ( alphaEq
  , hashDB
  ) where

import Data.Hashable (Hashable(..))

import Vehicle.Language.AST ( DBExpr )
import Vehicle.Compile.CoDeBruijnify ( toHashableCoDBExpr )

hashDB :: DBExpr ann -> Int
hashDB e = hash $ toHashableCoDBExpr e

alphaEq :: DBExpr ann -> DBExpr ann -> Bool
alphaEq e1 e2 = hashDB e1 == hashDB e2