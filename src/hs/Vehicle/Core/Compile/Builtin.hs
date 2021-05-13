{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module implements the check to see if there are any unknown builtins
-- and converts the builtin representation to a data type (as opposed to 'Symbol').
module Vehicle.Core.Compile.Builtin where

import Control.Monad.Except (MonadError(..))
import Vehicle.Core.AST
import Vehicle.Prelude

-- |Type of errors thrown by builtin checking.
newtype BuiltinError
  = UnknownBuiltin Token
  deriving (Show)

builtinKinds :: [(Symbol, BuiltinOp 'KIND)]
builtinKinds =
  [ "->"   |-> KFun
  , "Type" |-> KType
  , "Dim"  |-> KDim
  , "List" |-> KList
  ]

builtinTypes :: [(Symbol, BuiltinOp 'TYPE)]
builtinTypes =
  [ "->"     |-> TFun
  , "Bool"   |-> TBool
  , "Prop"   |-> TProp
  , "Int"    |-> TInt
  , "Real"   |-> TReal
  , "List"   |-> TList
  , "Tensor" |-> TTensor
  , "+"      |-> TAdd
  , "::"     |-> TCons
  ]

builtinExprs :: [(Symbol, BuiltinOp 'EXPR)]
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

checkBuiltinWithMap ::
  (MonadError BuiltinError m, IsToken builtin) =>
  [(Symbol, BuiltinOp sort)] ->
  K builtin sort ->
  m (Builtin sort)
checkBuiltinWithMap builtins tk = case lookup (tkSym tk) builtins of
  Nothing -> throwError (UnknownBuiltin (toToken tk))
  Just op -> return (Builtin (tkPos tk) op)

checkBuiltin ::
  (MonadError BuiltinError m, IsToken builtin, KnownSort sort) =>
  K builtin sort ->
  m (Builtin sort)
checkBuiltin (tk :: K builtin sort) = case sortSing :: SSort sort of
  SKIND -> checkBuiltinWithMap builtinKinds tk
  STYPE -> checkBuiltinWithMap builtinTypes tk
  SEXPR -> checkBuiltinWithMap builtinExprs tk
  _     -> throwError (UnknownBuiltin (toToken tk))

checkBuiltins ::
  (MonadError BuiltinError m, IsToken builtin, KnownSort sort) =>
  Tree name (K builtin) ann sort ->
  m (Tree name Builtin ann sort)
checkBuiltins = traverseTreeBuiltin checkBuiltin
