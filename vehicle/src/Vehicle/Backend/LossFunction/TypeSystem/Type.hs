module Vehicle.Backend.LossFunction.TypeSystem.Type
  ( typeLossBuiltin,
  --     convertToLossTypes,
  )
where

import Vehicle.Backend.LossFunction.TypeSystem.Core
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Subsystem.Standard.Type qualified as Standard
import Vehicle.Expr.DSL
import Vehicle.Expr.DeBruijn
import Prelude hiding (pi)

-- | Return the type of the provided builtin.
typeLossBuiltin :: Provenance -> LossBuiltin -> Type Ix LossBuiltin
typeLossBuiltin p b = fromDSL p $ case b of
  BuiltinConstructor c -> Standard.typeOfBuiltinConstructor c
  BuiltinFunction f -> Standard.typeOfBuiltinFunction f
  BuiltinType t -> Standard.typeOfBuiltinType t
  NatInDomainConstraint -> Standard.typeOfNatInDomainConstraint
  Loss -> type0
  LossTC tc -> typeOfLossTypeClass tc
  LossTCOp op -> typeOfLossTypeClassOp op

typeOfLossTypeClassOp :: LossTypeClassOp -> LossDSLExpr
typeOfLossTypeClassOp = \case
  NotTC -> typeOfOp1 hasNot
  AndTC -> typeOfOp2 hasAnd
  OrTC -> typeOfOp2 hasOr
  ImpliesTC -> typeOfOp2 hasImplies
  RatOrderTC ord -> typeOfOp2 (hasRatOrder ord)
  RatEqTC eq -> typeOfOp2 (hasRatEq eq)
  QuantTC q -> forAllTypePairs $ \t1 t2 -> hasQuant q t1 t2 .~~~> (t1 ~> t2) ~> t2

typeOfLossTypeClass :: LossTypeClass -> LossDSLExpr
typeOfLossTypeClass = \case
  HasNot -> type0 ~> type0 ~> type0
  HasAnd -> type0 ~> type0 ~> type0 ~> type0
  HasOr -> type0 ~> type0 ~> type0 ~> type0
  HasImplies -> type0 ~> type0 ~> type0 ~> type0
  HasRatOrder {} -> type0 ~> type0 ~> type0 ~> type0
  HasRatEq {} -> type0 ~> type0 ~> type0 ~> type0
  HasQuant {} -> type0 ~> type0 ~> type0

typeOfOp1 ::
  (LossDSLExpr -> LossDSLExpr -> LossDSLExpr) ->
  LossDSLExpr
typeOfOp1 constraint =
  forAllTypePairs $ \t1 t2 ->
    constraint t1 t2 .~~~> t1 ~> t2

typeOfOp2 ::
  (LossDSLExpr -> LossDSLExpr -> LossDSLExpr -> LossDSLExpr) ->
  LossDSLExpr
typeOfOp2 constraint =
  forAllTypeTriples $ \t1 t2 t3 ->
    constraint t1 t2 t3 .~~~> t1 ~> t2 ~> t3
