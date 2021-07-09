{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Vehicle.Core.AST.Instance where

import Vehicle.Core.AST.Core
import Vehicle.Prelude.Sort

-- * Kinds

deriving instance
  ( Eq (ann 'KIND)
  ) => Eq (Kind name ann)

deriving instance
  ( Ord (ann 'KIND)
  ) => Ord (Kind name ann)

deriving instance
  ( Show (ann 'KIND)
  ) => Show (Kind name ann)

-- * Types

deriving instance
  ( Eq (name 'KIND)
  , Eq (ann 'KIND)
  , Eq (name 'TYPE)
  , Eq (ann 'TYPE)
  , Eq (name 'TARG)
  , Eq (ann 'TARG)
  ) => Eq (Type name ann)

deriving instance
  ( Show (name 'KIND)
  , Show (ann 'KIND)
  , Show (name 'TYPE)
  , Show (ann 'TYPE)
  , Show (name 'TARG)
  , Show (ann 'TARG)
  ) => Show (Type name ann)

-- * Expressions

deriving instance
  ( Eq (name 'KIND)
  , Eq (ann 'KIND)
  , Eq (name 'TYPE)
  , Eq (ann 'TYPE)
  , Eq (name 'TARG)
  , Eq (ann 'TARG)
  , Eq (name 'EXPR)
  , Eq (ann 'EXPR)
  , Eq (name 'EARG)
  , Eq (ann 'EARG)
  ) => Eq (Expr name ann)

deriving instance
  ( Show (name 'KIND)
  , Show (ann 'KIND)
  , Show (name 'TYPE)
  , Show (ann 'TYPE)
  , Show (name 'TARG)
  , Show (ann 'TARG)
  , Show (name 'EXPR)
  , Show (ann 'EXPR)
  , Show (name 'EARG)
  , Show (ann 'EARG)
  ) => Show (Expr name ann)

-- * Declarations

deriving instance
  ( Eq (name 'KIND)
  , Eq (ann 'KIND)
  , Eq (name 'TYPE)
  , Eq (ann 'TYPE)
  , Eq (name 'TARG)
  , Eq (ann 'TARG)
  , Eq (name 'EXPR)
  , Eq (ann 'EXPR)
  , Eq (name 'EARG)
  , Eq (ann 'EARG)
  , Eq (name 'DECL)
  , Eq (ann 'DECL)
  ) => Eq (Decl name ann)

deriving instance
  ( Show (name 'KIND)
  , Show (ann 'KIND)
  , Show (name 'TYPE)
  , Show (ann 'TYPE)
  , Show (name 'TARG)
  , Show (ann 'TARG)
  , Show (name 'EXPR)
  , Show (ann 'EXPR)
  , Show (name 'EARG)
  , Show (ann 'EARG)
  , Show (name 'DECL)
  , Show (ann 'DECL)
  ) => Show (Decl name ann)

-- * Type arguments

deriving instance
  ( Eq (name 'KIND)
  , Eq (ann 'KIND)
  , Eq (name 'TYPE)
  , Eq (ann 'TYPE)
  , Eq (name 'TARG)
  , Eq (ann 'TARG)
  ) => Eq (TArg name ann)

deriving instance
  ( Show (name 'KIND)
  , Show (ann 'KIND)
  , Show (name 'TYPE)
  , Show (ann 'TYPE)
  , Show (name 'TARG)
  , Show (ann 'TARG)
  ) => Show (TArg name ann)

-- * Expression arguments

deriving instance
  ( Eq (name 'KIND)
  , Eq (ann 'KIND)
  , Eq (name 'TYPE)
  , Eq (ann 'TYPE)
  , Eq (name 'TARG)
  , Eq (ann 'TARG)
  , Eq (name 'EXPR)
  , Eq (ann 'EXPR)
  , Eq (name 'EARG)
  , Eq (ann 'EARG)
  ) => Eq (EArg name ann)

deriving instance
  ( Show (name 'KIND)
  , Show (ann 'KIND)
  , Show (name 'TYPE)
  , Show (ann 'TYPE)
  , Show (name 'TARG)
  , Show (ann 'TARG)
  , Show (name 'EXPR)
  , Show (ann 'EXPR)
  , Show (name 'EARG)
  , Show (ann 'EARG)
  ) => Show (EArg name ann)

-- * Programs

deriving instance
  ( Show (name 'KIND)
  , Show (ann 'KIND)
  , Show (name 'TYPE)
  , Show (ann 'TYPE)
  , Show (name 'TARG)
  , Show (ann 'TARG)
  , Show (name 'EXPR)
  , Show (ann 'EXPR)
  , Show (name 'EARG)
  , Show (ann 'EARG)
  , Show (name 'DECL)
  , Show (ann 'DECL)
  , Show (name 'PROG)
  , Show (ann 'PROG)
  ) => Show (Prog name ann)
