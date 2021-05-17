{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}

module Vehicle.Core.Compile.Provenance where

import           Control.Monad.Writer
import           Vehicle.Core.AST
import           Vehicle.Prelude


-- |Get the provenance for a single layer.
tellProvenance ::
  (IsToken name, IsToken builtin, KnownSort sort) =>
  TreeF (K name) (K builtin) ann sort sorted -> Writer Provenance ()

tellProvenance (tree :: TreeF name builtin ann sort sorted) = case sortSing :: SSort sort of

  -- Kinds
  SKIND -> case tree of
    KConF  _ann op -> tell (tkProvenance op)
    _              -> return ()

  -- Types
  STYPE -> case tree of
    TVarF _ann n  -> tell (tkProvenance n)
    TConF _ann op -> tell (tkProvenance op)
    _             -> return ()

  -- Type arguments
  STARG -> case tree of
    TArgF _ann n -> tell (tkProvenance n)

  -- Expressions
  SEXPR -> case tree of
    EVarF _ann n  -> tell (tkProvenance n)
    EConF _ann op -> tell (tkProvenance op)
    _             -> return ()

  -- Expression arguments
  SEARG -> case tree of
    EArgF _ann n -> tell (tkProvenance n)

  -- Declarations
  SDECL -> return ()

  -- Programs
  SPROG -> return ()


-- |Save the provenance information at each node in the tree.
saveProvenance ::
  (IsToken name, IsToken builtin, KnownSort sort) =>
  Tree (K name) (K builtin) ann sort ->
  Tree (K name) (K builtin) (K Provenance :*: ann) sort

saveProvenance = fst . runWriter . foldTreeM go
  where
    go tree = do
      ((), p) <- listen (tellProvenance tree)
      return . embed $ mapTreeFAnn (K p :*:) tree
