{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module Vehicle.Core.Check where

import Vehicle.Core.Type
import Vehicle.Core.Check.Builtin
import Vehicle.Core.Check.Provenance
import Vehicle.Core.Check.Scope
import Vehicle.Prelude

data CheckError
  = BuiltinError BuiltinError
  deriving Show

instance Exception CheckError

type MonadCheck m = MonadError CheckError

check ::
  (TCM m, IsToken name, IsToken builtin, KnownSort sort) =>
  Tree (K name) (K builtin) ann sort ->
  m (Tree (K name) Builtin (K Provenance :*: ann) sort)
check tree0 = do
  let tree1 = saveProvenance tree0
  tree2 <- withExcept BuiltinError (checkBuiltins tree1)
  return tree2
