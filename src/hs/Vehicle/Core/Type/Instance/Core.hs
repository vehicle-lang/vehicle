{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Vehicle.Core.Type.Instance.Core where

import Vehicle.Core.Type.Core

-- * Kinds

deriving instance
  ( Eq (builtin 'KIND)
  , Eq (ann 'KIND)
  ) => Eq (Kind name builtin ann)

deriving instance
  ( Ord (builtin 'KIND)
  , Ord (ann 'KIND)
  ) => Ord (Kind name builtin ann)

deriving instance
  ( Show (builtin 'KIND)
  , Show (ann 'KIND)
  ) => Show (Kind name builtin ann)

deriving instance
  ( Read (builtin 'KIND)
  , Read (ann 'KIND)
  ) => Read (Kind name builtin ann)

-- * Types

deriving instance
  ( Eq (name 'KIND)
  , Eq (builtin 'KIND)
  , Eq (ann 'KIND)
  , Eq (name 'TYPE)
  , Eq (builtin 'TYPE)
  , Eq (ann 'TYPE)
  , Eq (name 'TARG)
  , Eq (builtin 'TARG)
  , Eq (ann 'TARG)
  ) => Eq (Type name builtin ann)

-- * Expressions

deriving instance
  ( Eq (name 'KIND)
  , Eq (builtin 'KIND)
  , Eq (ann 'KIND)
  , Eq (name 'TYPE)
  , Eq (builtin 'TYPE)
  , Eq (ann 'TYPE)
  , Eq (name 'TARG)
  , Eq (builtin 'TARG)
  , Eq (ann 'TARG)
  , Eq (name 'EXPR)
  , Eq (builtin 'EXPR)
  , Eq (ann 'EXPR)
  , Eq (name 'EARG)
  , Eq (builtin 'EARG)
  , Eq (ann 'EARG)
  ) => Eq (Expr name builtin ann)

-- * Declarations

deriving instance
  ( Eq (name 'KIND)
  , Eq (builtin 'KIND)
  , Eq (ann 'KIND)
  , Eq (name 'TYPE)
  , Eq (builtin 'TYPE)
  , Eq (ann 'TYPE)
  , Eq (name 'TARG)
  , Eq (builtin 'TARG)
  , Eq (ann 'TARG)
  , Eq (name 'EXPR)
  , Eq (builtin 'EXPR)
  , Eq (ann 'EXPR)
  , Eq (name 'EARG)
  , Eq (builtin 'EARG)
  , Eq (ann 'EARG)
  , Eq (name 'DECL)
  , Eq (builtin 'DECL)
  , Eq (ann 'DECL)
  ) => Eq (Decl name builtin ann)

-- * Type arguments

deriving instance
  ( Eq (name 'KIND)
  , Eq (builtin 'KIND)
  , Eq (ann 'KIND)
  , Eq (name 'TYPE)
  , Eq (builtin 'TYPE)
  , Eq (ann 'TYPE)
  , Eq (name 'TARG)
  , Eq (builtin 'TARG)
  , Eq (ann 'TARG)
  ) => Eq (TArg name builtin ann)

-- * Expression arguments

deriving instance
  ( Eq (name 'KIND)
  , Eq (builtin 'KIND)
  , Eq (ann 'KIND)
  , Eq (name 'TYPE)
  , Eq (builtin 'TYPE)
  , Eq (ann 'TYPE)
  , Eq (name 'TARG)
  , Eq (builtin 'TARG)
  , Eq (ann 'TARG)
  , Eq (name 'EXPR)
  , Eq (builtin 'EXPR)
  , Eq (ann 'EXPR)
  , Eq (name 'EARG)
  , Eq (name 'EARG)
  , Eq (builtin 'EARG)
  , Eq (ann 'EARG)
  ) => Eq (EArg name builtin ann)

-- -}
-- -}
-- -}
-- -}
-- -}
