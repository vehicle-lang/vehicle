module Vehicle.Backend.LossFunction.TypeSystem.Type
  ( typeLossBuiltin,
  --     convertToLossTypes,
  )
where

import Vehicle.Backend.LossFunction.TypeSystem.Core
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Irrelevance (removeIrrelevantCode)
import Vehicle.Compile.Type.Subsystem.Standard.Type qualified as Standard
import Vehicle.Expr.BuiltinInterface
import Vehicle.Expr.DSL
import Prelude hiding (pi)

-- | Return the type of the provided builtin.
typeLossBuiltin :: Provenance -> LossBuiltin -> Type Ix LossBuiltin
typeLossBuiltin p b = case b of
  BuiltinConstructor c -> fromStandard $ Standard.typeOfBuiltinConstructor c
  BuiltinFunction f -> fromStandard $ Standard.typeOfBuiltinFunction f
  BuiltinType t -> fromStandard $ Standard.typeOfBuiltinType t
  NatInDomainConstraint -> fromStandard Standard.typeOfNatInDomainConstraint
  LossTC tc -> fromDSL p $ typeOfLossTypeClass tc
  LossTCOp op -> fromDSL p $ typeOfLossTypeClassOp op
  -- We should only ever be typing `Optimise` for its arity information. If
  -- this changes then we're going to need to tighten things up here, as it's
  -- type should be taken from the `DifferentiableLogic` rather than assuming
  -- it's of type `Rat`
  Optimise {} -> fromDSL p $ forAll "t" type0 $ \t -> (tRat ~> tRat ~> tRat) ~> (t ~> tRat) ~> tRat
  where
    fromStandard = removeIrrelevantCode . fromDSL p

typeOfLossTypeClassOp :: LossTypeClassOp -> LossDSLExpr
typeOfLossTypeClassOp = \case
  LBoolTC b -> typeOfOp0 (hasBoolLiteral b)
  NotTC -> typeOfOp1 hasNot
  AndTC -> typeOfOp2 hasAnd
  OrTC -> typeOfOp2 hasOr
  ImpliesTC -> typeOfOp2 hasImplies
  RatOrderTC ord -> typeOfOp2 (hasRatOrder ord)
  RatEqTC eq -> typeOfOp2 (hasRatEq eq)
  QuantTC q -> forAllTypePairs $ \t1 t2 -> hasQuant q (t1 ~> t2) t2 .~~~> (t1 ~> t2) ~> t2

typeOfLossTypeClass :: LossTypeClass -> LossDSLExpr
typeOfLossTypeClass = \case
  IsBoolType -> type0 ~> type0
  HasBoolLiteral {} -> type0 ~> type0
  HasNot -> type0 ~> type0 ~> type0
  HasAnd -> type0 ~> type0 ~> type0 ~> type0
  HasOr -> type0 ~> type0 ~> type0 ~> type0
  HasImplies -> type0 ~> type0 ~> type0 ~> type0
  HasRatOrder {} -> type0 ~> type0 ~> type0 ~> type0
  HasRatEq {} -> type0 ~> type0 ~> type0 ~> type0
  HasQuant {} -> type0 ~> type0 ~> type0
  ValidPropertyBaseType -> type0 ~> type0

typeOfOp0 ::
  (LossDSLExpr -> LossDSLExpr) ->
  LossDSLExpr
typeOfOp0 constraint =
  forAllTypes $ \t ->
    constraint t ~~~> t

typeOfOp1 ::
  (LossDSLExpr -> LossDSLExpr -> LossDSLExpr) ->
  LossDSLExpr
typeOfOp1 constraint =
  forAllTypePairs $ \t1 t2 ->
    constraint t1 t2 ~~~> t1 ~> t2

typeOfOp2 ::
  (LossDSLExpr -> LossDSLExpr -> LossDSLExpr -> LossDSLExpr) ->
  LossDSLExpr
typeOfOp2 constraint =
  forAllTypeTriples $ \t1 t2 t3 ->
    constraint t1 t2 t3 ~~~> t1 ~> t2 ~> t3
