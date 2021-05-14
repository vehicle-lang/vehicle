{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RankNTypes          #-}

module Vehicle.Core.AST
  ( module X
  , ATree
  , AKind
  , AType
  , ATArg
  , AExpr
  , AEArg
  , ADecl
  , AProg
  , ATreeF
  , AKindF
  , ATypeF
  , ATArgF
  , AExprF
  , AEArgF
  , ADeclF
  , AProgF
  ) where

import Vehicle.Core.AST.Builtin   as X
import Vehicle.Core.AST.Core      as X
import Vehicle.Core.AST.DeBruijn  as X
import Vehicle.Core.AST.Instance  as X ()
import Vehicle.Core.AST.Recursive as X

-- |Abstract syntax trees, where the representation of names and builtins is fixed.
type ATree (ann :: Sort -> *) (sort :: Sort)
  = Tree DeBruijn Builtin ann sort

type AKind ann = ATree ann 'KIND
type AType ann = ATree ann 'TYPE
type ATArg ann = ATree ann 'TARG
type AExpr ann = ATree ann 'EXPR
type AEArg ann = ATree ann 'EARG
type ADecl ann = ATree ann 'DECL
type AProg ann = ATree ann 'PROG

-- |Abstract syntax trees, where the representation of names and builtins is fixed.
type ATreeF (ann :: Sort -> *) (sort :: Sort) (sorted :: Sort -> *)
  = TreeF DeBruijn Builtin ann sort sorted

type AKindF ann sorted = ATreeF ann 'KIND sorted
type ATypeF ann sorted = ATreeF ann 'TYPE sorted
type ATArgF ann sorted = ATreeF ann 'TARG sorted
type AExprF ann sorted = ATreeF ann 'EXPR sorted
type AEArgF ann sorted = ATreeF ann 'EARG sorted
type ADeclF ann sorted = ATreeF ann 'DECL sorted
type AProgF ann sorted = ATreeF ann 'PROG sorted
