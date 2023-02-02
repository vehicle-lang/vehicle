module Vehicle.Compile.Normalise.Core where

import Data.List.NonEmpty (NonEmpty (..))
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Libraries.StandardLibrary (pattern TensorIdent)

--------------------------------------------------------------------------------
-- Normalising equality

nfEq ::
  Provenance ->
  EqualityDomain ->
  EqualityOp ->
  TypeCheckedArg ->
  TypeCheckedArg ->
  TypeCheckedExpr
nfEq p dom eq e1 e2 = case (dom, argExpr e1, argExpr e2) of
  (EqIndex, IndexLiteral _ _ x, IndexLiteral _ _ y) -> BoolLiteral p (equalityOp eq x y)
  (EqNat, NatLiteral _ x, NatLiteral _ y) -> BoolLiteral p (equalityOp eq x y)
  (EqInt, IntLiteral _ x, IntLiteral _ y) -> BoolLiteral p (equalityOp eq x y)
  (EqRat, RatLiteral _ x, RatLiteral _ y) -> BoolLiteral p (equalityOp eq x y)
  _ -> EqualityExpr p dom eq [e1, e2]

--------------------------------------------------------------------------------
-- Normalising tensor types

nfTensor ::
  Provenance ->
  TypeCheckedType ->
  TypeCheckedExpr ->
  TypeCheckedType
nfTensor p tElem dims = case dims of
  NilExpr {} -> tElem
  AppConsExpr _ _ d ds -> VectorType p (nfTensor p tElem ds) d
  _ -> App p (FreeVar p TensorIdent) (ExplicitArg p <$> [tElem, dims])

--------------------------------------------------------------------------------
-- Normalising orders

nfOrder ::
  Provenance ->
  OrderDomain ->
  OrderOp ->
  TypeCheckedArg ->
  TypeCheckedArg ->
  TypeCheckedExpr
nfOrder p dom ord arg1 arg2 = case (dom, argExpr arg1, argExpr arg2) of
  (OrderNat, NatLiteral _ x, NatLiteral _ y) -> BoolLiteral p (orderOp ord x y)
  (OrderIndex, IndexLiteral _ _ x, IndexLiteral _ _ y) -> BoolLiteral p (orderOp ord x y)
  (OrderInt, IntLiteral _ x, IntLiteral _ y) -> BoolLiteral p (orderOp ord x y)
  (OrderRat, RatLiteral _ x, RatLiteral _ y) -> BoolLiteral p (orderOp ord x y)
  _ -> OrderExpr p dom ord [arg1, arg2]

--------------------------------------------------------------------------------
-- Normalising boolean operations

normaliseNotArg :: TypeCheckedArg -> TypeCheckedArg
normaliseNotArg x = Arg (provenanceOf x) Explicit Relevant $ nfNot (provenanceOf x) x

nfNot :: Provenance -> TypeCheckedArg -> TypeCheckedExpr
nfNot p arg = case argExpr arg of
  BoolLiteral _ b -> BoolLiteral p (not b)
  _ -> NotExpr p [arg]

nfAnd :: Provenance -> TypeCheckedArg -> TypeCheckedArg -> TypeCheckedExpr
nfAnd p arg1 arg2 = case (argExpr arg1, argExpr arg2) of
  (TrueExpr _, _) -> argExpr arg2
  (FalseExpr _, _) -> FalseExpr p
  (_, TrueExpr _) -> argExpr arg1
  (_, FalseExpr _) -> FalseExpr p
  _ -> AndExpr p [arg1, arg2]

nfOr :: Provenance -> TypeCheckedArg -> TypeCheckedArg -> TypeCheckedExpr
nfOr p arg1 arg2 = case (argExpr arg1, argExpr arg2) of
  (TrueExpr _, _) -> TrueExpr p
  (FalseExpr _, _) -> argExpr arg2
  (_, TrueExpr _) -> TrueExpr p
  (_, FalseExpr _) -> argExpr arg1
  _ -> OrExpr p [arg1, arg2]

nfImplies :: Provenance -> TypeCheckedArg -> TypeCheckedArg -> TypeCheckedExpr
nfImplies p arg1 arg2 = case (argExpr arg1, argExpr arg2) of
  (TrueExpr _, _) -> argExpr arg2
  (FalseExpr _, _) -> TrueExpr p
  (_, TrueExpr _) -> TrueExpr p
  (_, FalseExpr _) -> NotExpr p [arg2]
  _ -> ImpliesExpr p [arg1, arg2]

nfIf :: Provenance -> TypeCheckedExpr -> TypeCheckedArg -> TypeCheckedArg -> TypeCheckedArg -> TypeCheckedExpr
nfIf p t condition e1 e2 = case argExpr condition of
  TrueExpr _ -> argExpr e1
  FalseExpr _ -> argExpr e2
  _ -> IfExpr p t [condition, e1, e2]

-----------------------------------------------------------------------------
-- Normalising conversion

nfFromRat :: MonadCompile m => FromRatDomain -> TypeCheckedArg -> m TypeCheckedExpr
nfFromRat dom (ExplicitArg _ rat@RatLiteral {}) = case dom of
  FromRatToRat -> return rat
nfFromRat _ _ = unexpectedExprError "conversion FromRat" "non-Rat"

nfFromVec :: MonadCompile m => FromVecDomain -> TypeCheckedArg -> m TypeCheckedExpr
nfFromVec dom (ExplicitArg _ vec@(AnnVecLiteral p tElem xs)) = case dom of
  FromVecToList -> return $ mkList p tElem xs
  FromVecToVec -> return vec
nfFromVec _ _ = unexpectedExprError "conversion FromVec" "non-Vec"

-----------------------------------------------------------------------------
-- Normalising numeric operations

normaliseNegArg :: NegDomain -> TypeCheckedArg -> TypeCheckedArg
normaliseNegArg dom x = ExplicitArg (provenanceOf x) $ nfNeg (provenanceOf x) dom x

nfNeg :: Provenance -> NegDomain -> TypeCheckedArg -> TypeCheckedExpr
nfNeg p dom e = case (dom, argExpr e) of
  (NegInt, IntLiteral _ x) -> IntLiteral p (-x)
  (NegRat, RatLiteral _ x) -> RatLiteral p (-x)
  _ -> NegExpr p dom [e]

nfAdd :: Provenance -> AddDomain -> TypeCheckedArg -> TypeCheckedArg -> TypeCheckedExpr
nfAdd p dom arg1 arg2 = case (dom, argExpr arg1, argExpr arg2) of
  (AddNat, NatLiteral _ x, NatLiteral _ y) -> NatLiteral p (x + y)
  (AddInt, IntLiteral _ x, IntLiteral _ y) -> IntLiteral p (x + y)
  (AddRat, RatLiteral _ x, RatLiteral _ y) -> RatLiteral p (x + y)
  _ -> AddExpr p dom [arg1, arg2]

nfSub :: Provenance -> SubDomain -> TypeCheckedArg -> TypeCheckedArg -> TypeCheckedExpr
nfSub p dom arg1 arg2 = case (dom, argExpr arg1, argExpr arg2) of
  (SubInt, IntLiteral _ x, IntLiteral _ y) -> IntLiteral p (x - y)
  (SubRat, RatLiteral _ x, RatLiteral _ y) -> RatLiteral p (x - y)
  _ -> SubExpr p dom [arg1, arg2]

nfMul :: Provenance -> MulDomain -> TypeCheckedArg -> TypeCheckedArg -> TypeCheckedExpr
nfMul p dom arg1 arg2 = case (dom, argExpr arg1, argExpr arg2) of
  (MulNat, NatLiteral _ x, NatLiteral _ y) -> NatLiteral p (x * y)
  (MulInt, IntLiteral _ x, IntLiteral _ y) -> IntLiteral p (x * y)
  (MulRat, RatLiteral _ x, RatLiteral _ y) -> RatLiteral p (x * y)
  _ -> MulExpr p dom [arg1, arg2]

nfDiv :: Provenance -> DivDomain -> TypeCheckedArg -> TypeCheckedArg -> TypeCheckedExpr
nfDiv p dom arg1 arg2 = case (dom, argExpr arg1, argExpr arg2) of
  (DivRat, RatLiteral _ x, RatLiteral _ y) -> RatLiteral p (x / y)
  _ -> DivExpr p dom [arg1, arg2]

-----------------------------------------------------------------------------
-- Normalising container operations

nfAt :: Provenance -> TypeCheckedExpr -> TypeCheckedExpr -> TypeCheckedArg -> TypeCheckedArg -> TypeCheckedExpr
nfAt p tElem tDim vector index = case (argExpr vector, argExpr index) of
  (AnnVecLiteral _ _ es, IndexLiteral _ _ i) -> es !! fromIntegral i
  _ ->
    BuiltinFunctionExpr
      p
      At
      ( ImplicitArg p tElem
          :| ImplicitArg p tDim
          : [vector, index]
      )

zipWithVector ::
  Provenance ->
  TypeCheckedType ->
  TypeCheckedType ->
  TypeCheckedType ->
  TypeCheckedExpr ->
  TypeCheckedExpr ->
  TypeCheckedExpr ->
  TypeCheckedExpr ->
  TypeCheckedExpr
zipWithVector p tElem1 tElem2 tRes size fn xs ys = do
  App
    p
    (FreeVar p $ Identifier StdLib "zipWith")
    [ ImplicitArg p tElem1,
      ImplicitArg p tElem2,
      ImplicitArg p tRes,
      ImplicitArg p size,
      ExplicitArg p fn,
      ExplicitArg p xs,
      ExplicitArg p ys
    ]

bigOp ::
  Provenance ->
  Identifier ->
  TypeCheckedExpr ->
  TypeCheckedExpr ->
  TypeCheckedExpr
bigOp p identifier size xs =
  App
    p
    (FreeVar p identifier)
    [ ImplicitArg p size,
      ExplicitArg p xs
    ]
