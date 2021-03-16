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

data Builtin (sort :: Sort) where

  -- Builtin kinds
  KType   :: Builtin 'KIND
  KDim    :: Builtin 'KIND
  KList   :: Builtin 'KIND

  -- Builtin types
  TFun    :: Builtin 'TYPE
  TBool   :: Builtin 'TYPE
  TInt    :: Builtin 'TYPE
  TReal   :: Builtin 'TYPE
  TList   :: Builtin 'TYPE
  TTensor :: Builtin 'TYPE
  TAdd    :: Builtin 'TYPE
  TNil    :: Builtin 'TYPE
  TCons   :: Builtin 'TYPE

  -- Builtin expressions
  EIf     :: Builtin 'EXPR
  EImpl   :: Builtin 'EXPR
  EAnd    :: Builtin 'EXPR
  EOr     :: Builtin 'EXPR
  ENot    :: Builtin 'EXPR
  ETrue   :: Builtin 'EXPR
  EFalse  :: Builtin 'EXPR
  EEq     :: Builtin 'EXPR
  ENeq    :: Builtin 'EXPR
  ELe     :: Builtin 'EXPR
  ELt     :: Builtin 'EXPR
  EGe     :: Builtin 'EXPR
  EGt     :: Builtin 'EXPR
  EMul    :: Builtin 'EXPR
  EDiv    :: Builtin 'EXPR
  EAdd    :: Builtin 'EXPR
  ESub    :: Builtin 'EXPR
  ENeg    :: Builtin 'EXPR
  ECons   :: Builtin 'EXPR
  ENil    :: Builtin 'EXPR
  EAt     :: Builtin 'EXPR
  EAll    :: Builtin 'EXPR
  EAny    :: Builtin 'EXPR

deriving instance Eq (Builtin sort)
deriving instance Ord (Builtin sort)
deriving instance Show (Builtin sort)

builtinKinds :: [(Text, Builtin 'KIND)]
builtinKinds =
  [ "Type" |-> KType
  , "Dim"  |-> KDim
  , "List" |-> KList
  ]

builtinTypes :: [(Text, Builtin 'TYPE)]
builtinTypes =
  [ "->"     |-> TFun
  , "Bool"   |-> TBool
  , "Int"    |-> TInt
  , "Real"   |-> TReal
  , "List"   |-> TList
  , "Tensor" |-> TTensor
  , "+"      |-> TAdd
  , "Nil"    |-> TNil
  , "Cons"   |-> TCons
  ]

builtinExprs :: [(Text, Builtin 'EXPR)]
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
  , "~"     |-> ENeg -- NOTE: negation is changed from "-" to "~" during elaboration
  , "!"     |-> EAt
  , "Cons"  |-> ECons
  , "Nil"   |-> ENil
  , "all"   |-> EAll
  , "any"   |-> EAny
  ]

checkBuiltin :: MonadCheck m => [(Text, Builtin sort)] -> VCA.SortedBuiltin sort -> m (Builtin sort)
checkBuiltin builtins tk =
  maybe (throwError (UnknownBuiltin (toToken tk))) return (lookup (tkName tk) builtins)
