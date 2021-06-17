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

-- * Type arguments

deriving instance
  ( Eq (name 'KIND)
  , Eq (ann 'KIND)
  , Eq (name 'TYPE)
  , Eq (ann 'TYPE)
  , Eq (name 'TARG)
  , Eq (ann 'TARG)
  ) => Eq (TArg name ann)

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