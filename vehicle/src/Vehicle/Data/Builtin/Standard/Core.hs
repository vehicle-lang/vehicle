{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Data.Builtin.Standard.Core
  ( module Syntax,
  )
where

import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Code.Expr
import Vehicle.Syntax.Builtin as Syntax

-----------------------------------------------------------------------------
-- Classes

isBuiltinCoercion :: Builtin -> Bool
isBuiltinCoercion = \case
  BuiltinFunction FromNat {} -> True
  BuiltinFunction FromRat {} -> True
  TypeClassOp FromNatTC {} -> True
  TypeClassOp FromRatTC {} -> True
  TypeClassOp FromVecTC {} -> True
  _ -> False

instance PrintableBuiltin Builtin where
  isCoercion = isBuiltinCoercion

instance BuiltinHasBoolLiterals Builtin where
  mkBoolBuiltinLit b = BuiltinConstructor (LBool b)
  getBoolBuiltinLit = \case
    BuiltinConstructor (LBool b) -> Just b
    _ -> Nothing

instance BuiltinHasIndexLiterals Builtin where
  getIndexBuiltinLit e = case e of
    BuiltinConstructor (LIndex n) -> Just n
    _ -> Nothing
  mkIndexBuiltinLit x = BuiltinConstructor (LIndex x)

instance BuiltinHasNatLiterals Builtin where
  getNatBuiltinLit e = case e of
    BuiltinConstructor (LNat b) -> Just b
    _ -> Nothing
  mkNatBuiltinLit x = BuiltinConstructor (LNat x)

instance BuiltinHasRatLiterals Builtin where
  getRatBuiltinLit e = case e of
    BuiltinConstructor (LRat b) -> Just b
    _ -> Nothing
  mkRatBuiltinLit x = BuiltinConstructor (LRat x)

instance BuiltinHasListLiterals Builtin where
  isBuiltinNil e = case e of
    BuiltinConstructor Nil -> True
    _ -> False
  mkBuiltinNil = BuiltinConstructor Nil

  isBuiltinCons e = case e of
    BuiltinConstructor Cons -> True
    _ -> False
  mkBuiltinCons = BuiltinConstructor Cons

instance BuiltinHasVecLiterals Builtin where
  getVecBuiltinLit e = case e of
    BuiltinConstructor (LVec n) -> Just n
    _ -> Nothing
  mkVecBuiltinLit n = BuiltinConstructor (LVec n)

instance BuiltinHasStandardTypeClasses Builtin where
  mkBuiltinTypeClass = TypeClass

instance BuiltinHasStandardTypes Builtin where
  mkBuiltinType = BuiltinType
  getBuiltinType = \case
    BuiltinType c -> Just c
    _ -> Nothing

  mkNatInDomainConstraint = NatInDomainConstraint

instance BuiltinHasStandardData Builtin where
  mkBuiltinFunction = BuiltinFunction
  getBuiltinFunction = \case
    BuiltinFunction c -> Just c
    _ -> Nothing

  mkBuiltinConstructor = BuiltinConstructor
  getBuiltinConstructor = \case
    BuiltinConstructor c -> Just c
    _ -> Nothing

  getBuiltinTypeClassOp = \case
    TypeClassOp op -> Just op
    _ -> Nothing
