{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE FlexibleInstances #-}
module Vehicle.Frontend.AST.Info where

import Vehicle.Frontend.AST.Core
import Vehicle.Prelude

-- |Type information, based on sort.
newtype Info (sort :: Sort) = Info { unInfo :: INFO sort }

-- |Computes type information based on sort; kinds for types, types for expressions.
type family INFO (sort :: Sort) where
  INFO 'KIND = ()
  INFO 'TYPE = Kind (Info :*: K Provenance)
  INFO 'TARG = Kind (Info :*: K Provenance)
  INFO 'EXPR = Type (Info :*: K Provenance)
  INFO 'EARG = Type (Info :*: K Provenance)
  INFO 'DECL = ()
  INFO 'PROG = ()

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