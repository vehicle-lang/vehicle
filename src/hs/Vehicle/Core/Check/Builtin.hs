{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module implements the check to see if there are any unknown builtins
-- and converts the builtin representation to a data type (as opposed to 'Text').
module Vehicle.Core.Check.Builtin where

import           Control.Monad.Except (MonadError(..))
import           Data.Text (Text)
import           Vehicle.Core.Check.Core
import           Vehicle.Core.Type
import           Vehicle.Prelude

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

checkBuiltinWithMap :: (IsToken tk, TCM m) => [(Text, BuiltinOp sort)] -> K tk sort -> m (Builtin sort)
checkBuiltinWithMap builtins tk = case lookup (tkText tk) builtins of
  Nothing -> throwError (UnknownBuiltin (toToken tk))
  Just op -> return (Builtin (tkPos tk) op)

checkBuiltin :: (IsToken tk, KnownSort sort, TCM m) => K tk sort -> m (Builtin sort)
checkBuiltin (tk :: K tk sort) = case sortSing :: SSort sort of
  SKIND -> checkBuiltinWithMap builtinKinds tk
  STYPE -> checkBuiltinWithMap builtinTypes tk
  SEXPR -> checkBuiltinWithMap builtinExprs tk
  _     -> throwError (UnknownBuiltin (toToken tk))

checkBuiltins :: (IsToken tk, KnownSort sort, TCM m) => Tree name (K tk) ann sort -> m (Tree name Builtin ann sort)
checkBuiltins = mapBuiltinM checkBuiltin
