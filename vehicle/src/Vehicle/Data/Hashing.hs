{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Data.Hashing () where

import Data.Hashable (Hashable (..))
-- import GHC.Generics (Generic)

-- import Vehicle.Data.Expr.Normalised

import GHC.Generics (Generic)
import Vehicle.Data.DeBruijn
import Vehicle.Data.Expr.Normalised (Value, WHNFClosure)
import Vehicle.Syntax.AST

-- We used to have full blown alpha-equivalence based on co-deBruijn indices
-- but this proved to be unnecessary. It's still in the repo's history if
-- need be though.

instance (Hashable builtin, Generic builtin) => Hashable (WHNFClosure builtin)

instance (Hashable closure, Hashable builtin) => Hashable (Value closure builtin)

instance (Hashable expr) => Hashable (GenericArg expr)

instance (Hashable expr) => Hashable (GenericBinder expr)

instance (Hashable builtin) => Hashable (Expr Ix builtin)
