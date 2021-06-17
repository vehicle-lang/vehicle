{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module Vehicle.Core.Compile
  ( compile
  ) where

import           Control.Monad.Except (Except, withExcept)
import           Vehicle.Core.AST
import           Vehicle.Core.Compile.Scope (checkScope, ScopeError(..))
import           Vehicle.Prelude

newtype CompileError
  = ScopeError ScopeError
  deriving Show

compile ::
  (IsToken name, KnownSort sort) =>
  Tree (K name) Builtin (K Provenance) sort ->
  Except CompileError (ATree (K Provenance) sort)
compile tree0 = withExcept ScopeError (checkScope tree0)
