{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Expr.Hashing () where

import Data.Hashable (Hashable (..))
import Vehicle.Compile.Type.Subsystem.Standard.Core

-- We used to have full blown alpha-equivalence based on co-deBruijn indices
-- but this proved to be unnecessary. It's still in the repo's history if
-- need be though.

instance Hashable StandardNormArg

instance Hashable StandardNormBinder

instance Hashable StandardNormExpr

instance Hashable TypeCheckedArg

instance Hashable TypeCheckedBinder

instance Hashable TypeCheckedExpr
