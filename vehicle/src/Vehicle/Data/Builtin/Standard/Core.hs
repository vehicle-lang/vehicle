{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Data.Builtin.Standard.Core
  ( module Syntax,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Vehicle.Data.Expr.Interface
import Vehicle.Data.Expr.Normalised
import Vehicle.Syntax.AST
import Vehicle.Syntax.Builtin as Syntax

-----------------------------------------------------------------------------
-- Literal instances for `Value`

instance HasBoolLits (Value closure Builtin) where
  mkBoolLit _p b = VBuiltin (BuiltinConstructor (LBool b)) []
  getBoolLit = \case
    VBuiltin (BuiltinConstructor (LBool b)) [] -> Just (mempty, b)
    _ -> Nothing

instance HasIndexLits (Value closure Builtin) where
  getIndexLit e = case e of
    VBuiltin (BuiltinConstructor (LIndex n)) [] -> Just (mempty, n)
    _ -> Nothing
  mkIndexLit _p x = VBuiltin (BuiltinConstructor (LIndex x)) mempty

instance HasNatLits (Value closure Builtin) where
  getNatLit e = case e of
    VBuiltin (BuiltinConstructor (LNat b)) [] -> Just (mempty, b)
    _ -> Nothing
  mkNatLit _p x = VBuiltin (BuiltinConstructor (LNat x)) mempty

instance HasRatLits (Value closure Builtin) where
  getRatLit e = case e of
    VBuiltin (BuiltinConstructor (LRat b)) [] -> Just (mempty, b)
    _ -> Nothing
  mkRatLit _p x = VBuiltin (BuiltinConstructor (LRat x)) mempty

instance HasStandardVecLits (Value closure Builtin) where
  mkHomoVector t xs = VBuiltin (BuiltinConstructor (LVec (length xs))) (t : xs)
  getHomoVector = \case
    VBuiltin (BuiltinConstructor (LVec _)) (t : xs) -> Just (t, xs)
    _ -> Nothing

instance HasStandardListLits (Value closure Builtin) where
  getNil e = case getConstructor e of
    Just (p, Nil, [t]) -> Just (p, t)
    _ -> Nothing
  mkNil t = mkConstructor mempty Nil [t]

  getCons e = case getConstructor e of
    Just (_p, Cons, [t, x, xs]) -> Just (t, x, xs)
    _ -> Nothing
  mkCons t x xs = mkConstructor mempty Cons [t, x, xs]

-----------------------------------------------------------------------------
-- Literal intstances for `Expr`

instance HasBoolLits (Expr var Builtin) where
  getBoolLit e = case e of
    Builtin _ (BuiltinConstructor (LBool b)) -> Just (mempty, b)
    _ -> Nothing
  mkBoolLit p x = Builtin p (BuiltinConstructor (LBool x))

instance HasIndexLits (Expr var Builtin) where
  getIndexLit e = case e of
    BuiltinExpr _ (BuiltinConstructor (LIndex n)) [] -> Just (mempty, n)
    _ -> Nothing
  mkIndexLit p x = Builtin p (BuiltinConstructor (LIndex x))

instance HasNatLits (Expr var Builtin) where
  getNatLit e = case e of
    Builtin _ (BuiltinConstructor (LNat b)) -> Just (mempty, b)
    _ -> Nothing
  mkNatLit p x = Builtin p (BuiltinConstructor (LNat x))

instance HasRatLits (Expr var Builtin) where
  getRatLit e = case e of
    Builtin _ (BuiltinConstructor (LRat b)) -> Just (mempty, b)
    _ -> Nothing
  mkRatLit p x = Builtin p (BuiltinConstructor (LRat x))

instance HasStandardVecLits (Expr var Builtin) where
  mkHomoVector t xs = BuiltinExpr mempty (BuiltinConstructor (LVec (length xs))) (t :| xs)
  getHomoVector = \case
    BuiltinExpr _ (BuiltinConstructor (LVec _)) (t :| xs) -> Just (t, xs)
    _ -> Nothing

instance (Show var) => HasStandardListLits (Expr var Builtin) where
  getNil e = case getConstructor e of
    Just (p, Nil, [t]) -> Just (p, t)
    _ -> Nothing
  mkNil t = mkConstructor mempty Nil [t]

  getCons e = case getConstructor e of
    Just (_p, Cons, [t, x, xs]) -> Just (t, x, xs)
    _ -> Nothing
  mkCons t x xs = mkConstructor mempty Cons [t, x, xs]
