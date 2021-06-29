{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module Vehicle.Core.Compile
  ( compile
  ) where

import           Control.Monad.Except (Except, withExcept)
import           Vehicle.Core.AST
import           Vehicle.Core.Compile.Scope (symbolToDeBruijn, ScopeError(..))
import           Vehicle.Prelude

newtype CompileError
  = ScopeError ScopeError

compile ::
  (KnownSort sort) =>
  Tree (K Symbol) (K Provenance) sort ->
  Except CompileError (ATree (K Provenance) sort)
compile tree0 = withExcept ScopeError (symbolToDeBruijn tree0)
