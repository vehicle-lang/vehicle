{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | This module exports the datatype representations of the builtin symbols.
module Vehicle.Core.Type.Builtin where

import           Vehicle.Core.Type.Core
import           Vehicle.Prelude

data Builtin (sort :: Sort) = Builtin Position (BuiltinOp sort)

data BuiltinOp (sort :: Sort) where

  -- Builtin kinds
  KFun    :: BuiltinOp 'KIND
  KType   :: BuiltinOp 'KIND
  KDim    :: BuiltinOp 'KIND
  KList   :: BuiltinOp 'KIND

  -- Builtin types
  TFun    :: BuiltinOp 'TYPE
  TBool   :: BuiltinOp 'TYPE
  TInt    :: BuiltinOp 'TYPE
  TReal   :: BuiltinOp 'TYPE
  TList   :: BuiltinOp 'TYPE
  TTensor :: BuiltinOp 'TYPE
  TAdd    :: BuiltinOp 'TYPE
  TNil    :: BuiltinOp 'TYPE
  TCons   :: BuiltinOp 'TYPE

  -- Builtin expressions
  EIf     :: BuiltinOp 'EXPR
  EImpl   :: BuiltinOp 'EXPR
  EAnd    :: BuiltinOp 'EXPR
  EOr     :: BuiltinOp 'EXPR
  ENot    :: BuiltinOp 'EXPR
  ETrue   :: BuiltinOp 'EXPR
  EFalse  :: BuiltinOp 'EXPR
  EEq     :: BuiltinOp 'EXPR
  ENeq    :: BuiltinOp 'EXPR
  ELe     :: BuiltinOp 'EXPR
  ELt     :: BuiltinOp 'EXPR
  EGe     :: BuiltinOp 'EXPR
  EGt     :: BuiltinOp 'EXPR
  EMul    :: BuiltinOp 'EXPR
  EDiv    :: BuiltinOp 'EXPR
  EAdd    :: BuiltinOp 'EXPR
  ESub    :: BuiltinOp 'EXPR
  ENeg    :: BuiltinOp 'EXPR
  ECons   :: BuiltinOp 'EXPR
  ENil    :: BuiltinOp 'EXPR
  EAt     :: BuiltinOp 'EXPR
  EAll    :: BuiltinOp 'EXPR
  EAny    :: BuiltinOp 'EXPR

deriving instance Eq (BuiltinOp sort)
deriving instance Ord (BuiltinOp sort)
deriving instance Show (BuiltinOp sort)
