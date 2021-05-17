{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module Vehicle.Core.Compile
  ( compile
  ) where

import           Control.Monad.Except (Except, withExcept)
import           Vehicle.Core.AST
import           Vehicle.Core.Compile.Builtin (checkBuiltins, BuiltinError(..))
import           Vehicle.Core.Compile.Provenance (saveProvenance)
import           Vehicle.Core.Compile.Scope (checkScope, ScopeError(..))
import           Vehicle.Prelude

data CompileError
  = BuiltinError BuiltinError
  | ScopeError ScopeError
  deriving Show

compile ::
  (IsToken name, IsToken builtin, KnownSort sort) =>
  Tree (K name) (K builtin) ann sort ->
  Except CompileError (ATree (K Provenance) sort)
compile tree0 = do
  let tree1 = mapTreeAnn ifst (saveProvenance tree0)
  tree2 <- withExcept BuiltinError (checkBuiltins tree1)
  tree3 <- withExcept ScopeError (checkScope tree2)
  return tree3
