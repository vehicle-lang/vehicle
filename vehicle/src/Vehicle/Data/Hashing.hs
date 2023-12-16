{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Data.Hashing () where

import Data.Hashable (Hashable (..))
import GHC.Generics (Generic)
import Vehicle.Data.DeBruijn
import Vehicle.Data.NormalisedExpr
import Vehicle.Syntax.AST

-- We used to have full blown alpha-equivalence based on co-deBruijn indices
-- but this proved to be unnecessary. It's still in the repo's history if
-- need be though.

instance (Hashable builtin, Generic builtin) => Hashable (BoundEnvValue 'WHNF builtin)

instance (Hashable builtin, Generic builtin) => Hashable (Body 'WHNF builtin)

instance (Hashable builtin, Generic builtin) => Hashable (WHNFValue builtin)

instance (Hashable expr) => Hashable (GenericArg expr)

instance (Hashable expr) => Hashable (GenericBinder expr)

instance (Hashable builtin) => Hashable (Expr Ix builtin)
