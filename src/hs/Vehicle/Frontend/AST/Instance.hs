{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Vehicle.Frontend.AST.Instance where

import Vehicle.Frontend.AST.Core
import Vehicle.Prelude.Sort (Sort(..))

-- * Kinds

deriving instance
  ( Eq (ann 'KIND)
  ) => Eq (Kind ann)

deriving instance
  ( Ord (ann 'KIND)
  ) => Ord (Kind ann)

deriving instance
  ( Show (ann 'KIND)
  ) => Show (Kind ann)

deriving instance
  ( Read (ann 'KIND)
  ) => Read (Kind ann)

-- * Types

deriving instance
  ( Eq (ann 'KIND)
  , Eq (ann 'TYPE)
  , Eq (ann 'TARG)
  ) => Eq (Type ann)

-- * Expressions

deriving instance
  ( Eq (ann 'KIND)
  , Eq (ann 'TYPE)
  , Eq (ann 'TARG)
  , Eq (ann 'EXPR)
  , Eq (ann 'EARG)
  , Eq (ann 'DECL)
  ) => Eq (Expr ann)

-- * Declarations

deriving instance
  ( Eq (ann 'KIND)
  , Eq (ann 'TYPE)
  , Eq (ann 'TARG)
  , Eq (ann 'EXPR)
  , Eq (ann 'EARG)
  , Eq (ann 'DECL)
  ) => Eq (Decl ann)

-- * Type arguments

deriving instance
  ( Eq (ann 'KIND)
  , Eq (ann 'TYPE)
  , Eq (ann 'TARG)
  ) => Eq (TArg ann)

-- * Expression arguments

deriving instance
  ( Eq (ann 'KIND)
  , Eq (ann 'TYPE)
  , Eq (ann 'TARG)
  , Eq (ann 'EXPR)
  , Eq (ann 'EARG)
  ) => Eq (EArg ann)