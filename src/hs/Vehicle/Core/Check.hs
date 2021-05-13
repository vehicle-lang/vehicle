{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module Vehicle.Core.Check where

import           Control.Monad.Except (Except, withExcept)
import           Vehicle.Core.AST
import           Vehicle.Core.Check.Builtin
import           Vehicle.Core.Check.Provenance
import           Vehicle.Core.Check.Scope
import           Vehicle.Prelude

data CheckError
  = BuiltinError BuiltinError
  | ScopeError ScopeError
  deriving Show

type TCM a = Except CheckError a

check ::
  (IsToken name, IsToken builtin, KnownSort sort) =>
  Tree (K name) (K builtin) ann sort ->
  TCM (Tree DeBruijn Builtin (K Provenance :*: ann) sort)
check tree0 = do
  let tree1 = saveProvenance tree0
  tree2 <- withExcept BuiltinError (checkBuiltins tree1)
  tree3 <- withExcept ScopeError (checkScope tree2)
  return tree3
