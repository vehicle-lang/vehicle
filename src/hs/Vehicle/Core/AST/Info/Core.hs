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
newtype Info (sort :: Sort) = Info { unInfo :: INFO sort }

-- |Computes type information based on sort; kinds for types, types for expressions.
type family INFO (sort :: Sort) where
  INFO 'KIND = ()
  INFO 'TYPE = AbsKind (Info :*: K Provenance)
  INFO 'TARG = AbsKind (Info :*: K Provenance)
  INFO 'EXPR = AbsType (Info :*: K Provenance)
  INFO 'EARG = AbsType (Info :*: K Provenance)
  INFO 'DECL = ()
  INFO 'PROG = ()

instance KnownSort sort => Eq (Info sort) where
  Info info1 == Info info2 =
    case sortSing @sort of
      SKIND -> info1 == info2
      STYPE -> info1 == info2
      STARG -> info1 == info2
      SEXPR -> info1 == info2
      SEARG -> info1 == info2
      SDECL -> info1 == info2
      SPROG -> info1 == info2

instance Semigroup (Info 'KIND) where
  Info () <> Info () = Info ()

instance Monoid (Info 'KIND) where
  mempty = Info ()

instance Semigroup (Info 'DECL) where
  Info () <> Info () = Info ()

instance Monoid (Info 'DECL) where
  mempty = Info ()

instance Semigroup (Info 'PROG) where
  Info () <> Info () = Info ()

instance Monoid (Info 'PROG) where
  mempty = Info ()