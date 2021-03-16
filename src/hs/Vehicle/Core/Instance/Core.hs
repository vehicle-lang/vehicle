{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Vehicle.Core.Instance.Core where

import Vehicle.Core.Type

-- * Kinds

deriving instance
  ( Eq (builtin 'KIND)
  , Eq (ann 'KIND (name 'KIND) (builtin 'KIND))
  ) => Eq (Kind name builtin ann)

deriving instance
  ( Ord (builtin 'KIND)
  , Ord (ann 'KIND (name 'KIND) (builtin 'KIND))
  ) => Ord (Kind name builtin ann)

deriving instance
  ( Show (builtin 'KIND)
  , Show (ann 'KIND (name 'KIND) (builtin 'KIND))
  ) => Show (Kind name builtin ann)

deriving instance
  ( Read (builtin 'KIND)
  , Read (ann 'KIND (name 'KIND) (builtin 'KIND))
  ) => Read (Kind name builtin ann)

-- * Types

deriving instance
  ( Eq (name 'KIND)
  , Eq (builtin 'KIND)
  , Eq (ann 'KIND (name 'KIND) (builtin 'KIND))
  , Eq (name 'TYPE)
  , Eq (builtin 'TYPE)
  , Eq (ann 'TYPE (name 'TYPE) (builtin 'TYPE))
  , Eq (name 'TARG)
  , Eq (builtin 'TARG)
  , Eq (ann 'TARG (name 'TARG) (builtin 'TARG))
  ) => Eq (Type name builtin ann)

-- * Expressions

deriving instance
  ( Eq (name 'KIND)
  , Eq (builtin 'KIND)
  , Eq (ann 'KIND (name 'KIND) (builtin 'KIND))
  , Eq (name 'TYPE)
  , Eq (builtin 'TYPE)
  , Eq (ann 'TYPE (name 'TYPE) (builtin 'TYPE))
  , Eq (name 'TARG)
  , Eq (builtin 'TARG)
  , Eq (ann 'TARG (name 'TARG) (builtin 'TARG))
  , Eq (name 'EXPR)
  , Eq (builtin 'EXPR)
  , Eq (ann 'EXPR (name 'EXPR) (builtin 'EXPR))
  , Eq (name 'EARG)
  , Eq (builtin 'EARG)
  , Eq (ann 'EARG (name 'EARG) (builtin 'EARG))
  ) => Eq (Expr name builtin ann)

-- * Declarations

deriving instance
  ( Eq (name 'KIND)
  , Eq (builtin 'KIND)
  , Eq (ann 'KIND (name 'KIND) (builtin 'KIND))
  , Eq (name 'TYPE)
  , Eq (builtin 'TYPE)
  , Eq (ann 'TYPE (name 'TYPE) (builtin 'TYPE))
  , Eq (name 'TARG)
  , Eq (builtin 'TARG)
  , Eq (ann 'TARG (name 'TARG) (builtin 'TARG))
  , Eq (name 'EXPR)
  , Eq (builtin 'EXPR)
  , Eq (ann 'EXPR (name 'EXPR) (builtin 'EXPR))
  , Eq (name 'EARG)
  , Eq (builtin 'EARG)
  , Eq (ann 'EARG (name 'EARG) (builtin 'EARG))
  , Eq (name 'DECL)
  , Eq (builtin 'DECL)
  , Eq (ann 'DECL (name 'DECL) (builtin 'DECL))
  ) => Eq (Decl name builtin ann)

-- * Type arguments

deriving instance
  ( Eq (name 'KIND)
  , Eq (builtin 'KIND)
  , Eq (ann 'KIND (name 'KIND) (builtin 'KIND))
  , Eq (name 'TYPE)
  , Eq (builtin 'TYPE)
  , Eq (ann 'TYPE (name 'TYPE) (builtin 'TYPE))
  , Eq (name 'TARG)
  , Eq (builtin 'TARG)
  , Eq (ann 'TARG (name 'TARG) (builtin 'TARG))
  ) => Eq (TArg name builtin ann)

-- * Expression arguments

deriving instance
  ( Eq (name 'KIND)
  , Eq (builtin 'KIND)
  , Eq (ann 'KIND (name 'KIND) (builtin 'KIND))
  , Eq (name 'TYPE)
  , Eq (builtin 'TYPE)
  , Eq (ann 'TYPE (name 'TYPE) (builtin 'TYPE))
  , Eq (name 'TARG)
  , Eq (builtin 'TARG)
  , Eq (ann 'TARG (name 'TARG) (builtin 'TARG))
  , Eq (name 'EXPR)
  , Eq (builtin 'EXPR)
  , Eq (ann 'EXPR (name 'EXPR) (builtin 'EXPR))
  , Eq (name 'EARG)
  , Eq (name 'EARG)
  , Eq (builtin 'EARG)
  , Eq (ann 'EARG (name 'EARG) (builtin 'EARG))
  ) => Eq (EArg name builtin ann)
