-- | This module handles the coercions that occur during scope checking.
module Vehicle.Compile.Scope.Coercions
  ( insertCoercions,
    removeCoercions,
  )
where

import Data.Maybe (listToMaybe, mapMaybe)
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Code.Expr
import Vehicle.Data.Code.Interface
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Definition

data Coercion
  = CoerceNatLiteral
  | CoerceRatLiteral
  | CoerceVectorLiteral
  | CoerceTensorType
  deriving (Enum, Bounded)

--------------------------------------------------------------------------------
-- Coercion insertion

type CoercionInsertion = Provenance -> Builtin -> [Arg Builtin] -> Maybe (Expr Builtin)

insertCoercions :: CoercionInsertion
insertCoercions p b args =
  listToMaybe $ mapMaybe (\c -> insertIndividualCoercion c p b args) enumerate

insertIndividualCoercion :: Coercion -> CoercionInsertion
insertIndividualCoercion = \case
  CoerceNatLiteral -> insertNatLiteralCoercion
  CoerceRatLiteral -> insertRatLiteralCoercion
  CoerceVectorLiteral -> insertVectorLiteralCoercion
  CoerceTensorType -> insertTensorTypeCoercion

--------------------------------------------------------------------------------
-- Coercion removal

type CoercionRemoval = Provenance -> Builtin -> [Arg Builtin] -> Maybe (Expr Builtin)

removeCoercions :: CoercionRemoval
removeCoercions p b args =
  listToMaybe $ mapMaybe (\c -> removeIndividualCoercion c p b args) enumerate

removeIndividualCoercion :: Coercion -> CoercionRemoval
removeIndividualCoercion = \case
  CoerceNatLiteral -> removeNatLiteralCoercion
  CoerceRatLiteral -> removeRatLiteralCoercion
  CoerceVectorLiteral -> removeVectorLiteralCoercion
  CoerceTensorType -> removeTensorTypeCoercion

--------------------------------------------------------------------------------
-- CoerceNatLiteral

insertNatLiteralCoercion :: CoercionInsertion
insertNatLiteralCoercion p builtin args = case builtin of
  BuiltinConstructor (NatLiteral {}) ->
    Just $
      insertLitCoercion FromNatTC p builtin args
  _ -> Nothing

removeNatLiteralCoercion :: CoercionRemoval
removeNatLiteralCoercion _p builtin args = case (builtin, args) of
  (TypeClassOp FromNatTC {}, _) -> firstExplicit args
  (BuiltinCast (FromNat FromNatToIndex), _ : value : _) -> Just $ argExpr value
  (BuiltinCast (FromNat FromNatToNat), value : _) -> Just $ argExpr value
  (BuiltinCast (FromNat FromNatToRat), value : _) -> Just $ argExpr value
  _ -> Nothing

--------------------------------------------------------------------------------
-- CoerceRatLiteral

insertRatLiteralCoercion :: CoercionInsertion
insertRatLiteralCoercion p builtin args = case builtin of
  BuiltinConstructor (RatTensorLiteral {}) -> Just $ do
    insertLitCoercion FromRatTC p builtin args
  _ -> Nothing

removeRatLiteralCoercion :: CoercionRemoval
removeRatLiteralCoercion _p builtin args = case (builtin, args) of
  (TypeClassOp FromRatTC {}, _) -> firstExplicit args
  (BuiltinCast (FromRat FromRatToRat), value : _) -> Just $ argExpr value
  _ -> Nothing

--------------------------------------------------------------------------------
-- CoerceVectorLiteral

insertVectorLiteralCoercion :: CoercionInsertion
insertVectorLiteralCoercion p builtin args = case builtin of
  BuiltinConstructor (VectorLiteral {}) -> Just $ do
    insertLitCoercion FromVecTC p builtin args
  _ -> Nothing

removeVectorLiteralCoercion :: CoercionRemoval
removeVectorLiteralCoercion _p builtin args = case (builtin, args) of
  (TypeClassOp FromVecTC {}, _) -> firstExplicit args
  (BuiltinCast (FromVec FromVecToList), _ : _ : value : _) -> Just $ argExpr value
  (BuiltinCast (FromVec FromVecToVec), _ : _ : value : _) -> Just $ argExpr value
  (BuiltinCast (FromVec FromVecToTensor), _ : _ : _ : value : _) -> Just $ argExpr value
  (BuiltinConstructor Nil, getExpr accessSpine -> Just (NilArgs t)) -> Just $ IVecLiteral t []
  (BuiltinConstructor Cons, getExpr accessSpine -> Just (ConsArgs t x xs)) -> do
    IVecLiteral t . (x :) <$> getListExpr xs
  (BuiltinFunction StackTensor, getExpr accessSpine -> Just stackArgs) -> do
    return $ IVecLiteral (stackType stackArgs) (stackElements stackArgs)
  _ -> Nothing

--------------------------------------------------------------------------------
-- CoerceTensorLiteral

insertTensorTypeCoercion :: CoercionInsertion
insertTensorTypeCoercion p builtin args = case builtin of
  BuiltinType TensorType -> Just $ do
    normAppList (Builtin p (TypeClassOp TensorTypeTC)) args
  _ -> Nothing

removeTensorTypeCoercion :: CoercionRemoval
removeTensorTypeCoercion p builtin args = case (builtin, args) of
  (TypeClassOp TensorTypeTC, tElem : dim : _) -> Just $ normAppList (Builtin p (BuiltinType TensorType)) [tElem, dim]
  (BuiltinType TensorType, [tElem, argExpr -> IDimNil]) -> Just $ argExpr tElem
  _ -> Nothing

--------------------------------------------------------------------------------
-- Utilities

insertLitCoercion :: TypeClassOp -> Provenance -> Builtin -> [Arg Builtin] -> Expr Builtin
insertLitCoercion tcOp p builtin args = do
  let coercion = Builtin p (TypeClassOp tcOp)
  let coercee = normAppList (Builtin p builtin) args
  normAppList coercion [explicit coercee]

-- As we may be either pre- or post-type checking, we cannot in general rely
-- on the type-class coercions to have all their non-explicit arguments inserted
-- so we use the invariant that they all have a single explicit argument.
firstExplicit :: [Arg Builtin] -> Maybe (Expr Builtin)
firstExplicit args = case filter isExplicit args of
  arg : _ -> Just $ argExpr arg
  [] -> Nothing
