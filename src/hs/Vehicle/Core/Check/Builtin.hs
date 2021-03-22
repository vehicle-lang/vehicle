{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}


-- | This module implements the check to see if there are any unknown builtins
-- and converts the builtin representation to a data type (as opposed to 'Text').
module Vehicle.Core.Check.Builtin where

import           Control.Monad.Except (MonadError(..))
import           Data.Text (Text)
import           Vehicle.Core.Check.Core
import           Vehicle.Core.Type
import qualified Vehicle.Core.Abs as VCA (SortedBuiltin(..), Builtin(..))
import           Vehicle.Prelude


data Builtin (sort :: Sort) = Builtin
  { pos :: Position
  , op  :: BuiltinOp sort
  }

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

builtinKinds :: [(Text, BuiltinOp 'KIND)]
builtinKinds =
  [ "->"   |-> KFun
  , "Type" |-> KType
  , "Dim"  |-> KDim
  , "List" |-> KList
  ]


builtinTypes :: [(Text, BuiltinOp 'TYPE)]
builtinTypes =
  [ "->"     |-> TFun
  , "Bool"   |-> TBool
  , "Int"    |-> TInt
  , "Real"   |-> TReal
  , "List"   |-> TList
  , "Tensor" |-> TTensor
  , "+"      |-> TAdd
  , "::"     |-> TCons
  ]

builtinExprs :: [(Text, BuiltinOp 'EXPR)]
builtinExprs =
  [ "if"    |-> EIf
  , "=>"    |-> EImpl
  , "and"   |-> EAnd
  , "or"    |-> EOr
  , "not"   |-> ENot
  , "True"  |-> ETrue
  , "False" |-> EFalse
  , "=="    |-> EEq
  , "!="    |-> ENeq
  , "<="    |-> ELe
  , "<"     |-> ELt
  , ">="    |-> EGe
  , ">"     |-> EGt
  , "*"     |-> EMul
  , "/"     |-> EDiv
  , "-"     |-> ESub
  , "~"     |-> ENeg
  -- ^ Negation is changed from "-" to "~" during elaboration.
  , "!"     |-> EAt
  , "::"    |-> ECons
  , "all"   |-> EAll
  , "any"   |-> EAny
  ]

checkBuiltinWithMap :: TCM m => [(Text, BuiltinOp sort)] -> VCA.SortedBuiltin sort -> m (Builtin sort)
checkBuiltinWithMap builtins tk = case lookup (tkText tk) builtins of
  Nothing -> throwError (UnknownBuiltin (toToken tk))
  Just op -> return (Builtin { pos = tkPos tk, op = op })

checkBuiltin :: TCM m => SSort sort -> VCA.SortedBuiltin sort -> m (Builtin sort)
checkBuiltin ssort tk = case ssort of
  SKIND -> checkBuiltinWithMap builtinKinds tk
  STYPE -> checkBuiltinWithMap builtinTypes tk
  SEXPR -> checkBuiltinWithMap builtinExprs tk
  _     -> throwError (UnknownBuiltin (toToken tk))

checkBuiltins :: (TCM m) => SSort sort -> Tree sort name VCA.SortedBuiltin ann -> m (Tree sort name Builtin ann)
checkBuiltins = mapBuiltinM checkBuiltin
