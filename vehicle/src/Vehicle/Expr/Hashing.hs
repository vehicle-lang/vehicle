{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Expr.Hashing () where

import Data.Hashable (Hashable (..))
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised
import Vehicle.Syntax.AST

-- We used to have full blown alpha-equivalence based on co-deBruijn indices
-- but this proved to be unnecessary. It's still in the repo's history if
-- need be though.

instance (Hashable builtin) => Hashable (VArg builtin)

instance (Hashable builtin) => Hashable (VBinder builtin)

instance (Hashable builtin) => Hashable (Value builtin)

instance (Hashable builtin) => Hashable (Arg Ix builtin)

instance (Hashable builtin) => Hashable (Binder Ix builtin)

instance (Hashable builtin) => Hashable (Expr Ix builtin)
