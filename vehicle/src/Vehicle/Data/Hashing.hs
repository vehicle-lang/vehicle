{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Data.Hashing () where

import Data.Hashable (Hashable (..))
import Vehicle.Data.Code.Expr (Expr)
import Vehicle.Prelude

-- We used to have full blown alpha-equivalence based on co-deBruijn indices
-- but this proved to be unnecessary. It's still in the repo's history if
-- need be though.

instance (Hashable expr) => Hashable (GenericArg expr)

instance (Hashable expr) => Hashable (GenericBinder expr)

instance (Hashable builtin) => Hashable (Expr builtin)
