{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | This module exports the datatype representations of the builtin symbols.
module Vehicle.Core.AST.Builtin where

import           Vehicle.Core.AST.Core

data Builtin (sort :: Sort) where

  -- Builtin kinds
  KFun     :: Builtin 'KIND
  KType    :: Builtin 'KIND
  KDim     :: Builtin 'KIND
  KDimList :: Builtin 'KIND

  -- Builtin types
  TFun     :: Builtin 'TYPE
  TBool    :: Builtin 'TYPE
  TProp    :: Builtin 'TYPE
  TInt     :: Builtin 'TYPE
  TReal    :: Builtin 'TYPE
  TList    :: Builtin 'TYPE
  TTensor  :: Builtin 'TYPE
  TAdd     :: Builtin 'TYPE
  TCons    :: Builtin 'TYPE

  -- Builtin expressions
  EIf      :: Builtin 'EXPR
  EImpl    :: Builtin 'EXPR
  EAnd     :: Builtin 'EXPR
  EOr      :: Builtin 'EXPR
  ENot     :: Builtin 'EXPR
  ETrue    :: Builtin 'EXPR
  EFalse   :: Builtin 'EXPR
  EEq      :: Builtin 'EXPR
  ENeq     :: Builtin 'EXPR
  ELe      :: Builtin 'EXPR
  ELt      :: Builtin 'EXPR
  EGe      :: Builtin 'EXPR
  EGt      :: Builtin 'EXPR
  EMul     :: Builtin 'EXPR
  EDiv     :: Builtin 'EXPR
  EAdd     :: Builtin 'EXPR
  ESub     :: Builtin 'EXPR
  ENeg     :: Builtin 'EXPR
  ECons    :: Builtin 'EXPR
  EAt      :: Builtin 'EXPR
  EAll     :: Builtin 'EXPR
  EAny     :: Builtin 'EXPR

deriving instance Eq (Builtin 'KIND)
deriving instance Eq (Builtin 'TYPE)
deriving instance Eq (Builtin 'TARG)
deriving instance Eq (Builtin 'EXPR)
deriving instance Eq (Builtin 'EARG)
deriving instance Eq (Builtin 'DECL)
deriving instance Eq (Builtin 'PROG)

deriving instance Ord (Builtin 'KIND)
deriving instance Ord (Builtin 'TYPE)
deriving instance Ord (Builtin 'TARG)
deriving instance Ord (Builtin 'EXPR)
deriving instance Ord (Builtin 'EARG)
deriving instance Ord (Builtin 'DECL)
deriving instance Ord (Builtin 'PROG)

deriving instance Show (Builtin 'KIND)
deriving instance Show (Builtin 'TYPE)
deriving instance Show (Builtin 'TARG)
deriving instance Show (Builtin 'EXPR)
deriving instance Show (Builtin 'EARG)
deriving instance Show (Builtin 'DECL)
deriving instance Show (Builtin 'PROG)
