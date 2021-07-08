{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Vehicle.Core.AST.Info.Core where

import Vehicle.Core.AST.Core
import Vehicle.Core.AST.DeBruijn
import Vehicle.Core.AST.Instance ()
import Vehicle.Core.AST.Recursive
import Vehicle.Prelude

-- |Abstract syntax trees, where the representation of names is fixed.
-- The intermediate representation overwhich analysis is performed.
type AbsTree (ann :: Sort -> *) (sort :: Sort) = Tree DeBruijn ann sort
type AbsKind ann = AbsTree ann 'KIND
type AbsType ann = AbsTree ann 'TYPE
type AbsTArg ann = AbsTree ann 'TARG
type AbsExpr ann = AbsTree ann 'EXPR
type AbsEArg ann = AbsTree ann 'EARG
type AbsDecl ann = AbsTree ann 'DECL
type AbsProg ann = AbsTree ann 'PROG

-- |Abstract syntax tree functors.
type AbsTreeF (ann :: Sort -> *) (sort :: Sort) (sorted :: Sort -> *)
  = TreeF DeBruijn ann sort sorted
type AbsKindF ann sorted = AbsTreeF ann 'KIND sorted
type AbsTypeF ann sorted = AbsTreeF ann 'TYPE sorted
type AbsTArgF ann sorted = AbsTreeF ann 'TARG sorted
type AbsExprF ann sorted = AbsTreeF ann 'EXPR sorted
type AbsEArgF ann sorted = AbsTreeF ann 'EARG sorted
type AbsDeclF ann sorted = AbsTreeF ann 'DECL sorted
type AbsProgF ann sorted = AbsTreeF ann 'PROG sorted

-- |Type information, based on sort.
newtype Info (name :: Sort -> *) (sort :: Sort) = Info { unInfo :: INFO name sort }

type InfoAnn name = Info name :*: K Provenance

-- |Computes type information based on sort; kinds for types, types for expressions.
type family INFO (name :: Sort -> *) (sort :: Sort) where
  INFO name 'KIND = ()
  INFO name 'TYPE = Kind name (InfoAnn name)
  INFO name 'TARG = Kind name (InfoAnn name)
  INFO name 'EXPR = Type name (InfoAnn name)
  INFO name 'EARG = Type name (InfoAnn name)
  INFO name 'DECL = ()
  INFO name 'PROG = ()

instance KnownSort sort => Eq (Info name sort) where
  x == y = case sortSing @sort of
    SKIND -> x == y
    STYPE -> x == y
    STARG -> x == y
    SEXPR -> x == y
    SEARG -> x == y
    SDECL -> x == y
    SPROG -> x == y

instance Semigroup (Info name 'KIND) where
  Info () <> Info () = Info ()

instance Monoid (Info name 'KIND) where
  mempty = Info ()

instance Semigroup (Info name 'DECL) where
  Info () <> Info () = Info ()

instance Monoid (Info name 'DECL) where
  mempty = Info ()

instance Semigroup (Info name 'PROG) where
  Info () <> Info () = Info ()

instance Monoid (Info name 'PROG) where
  mempty = Info ()
