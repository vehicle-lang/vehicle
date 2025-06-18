{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use id" #-}

module Vehicle.Data.Builtin.Decidability.Instances
  ( decidabilityBuiltinInstances,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Core (InstanceCandidate (..), InstanceDatabase (..))
import Vehicle.Data.Builtin.Core (BuiltinFunction (..), DerivedFunction (..))
import Vehicle.Data.Builtin.Decidability
import Vehicle.Data.Code.DSL
import Vehicle.Data.DSL

decidabilityBuiltinInstances :: InstanceDatabase DecidabilityBuiltin
decidabilityBuiltinInstances = makeInstanceDatabase allInstances mempty

-- Manually declared here as we have no way of declaring them in the language
-- itself.

-- Also note that annoyingly because of a lack of first class records we have
-- to duplicate the context for both the candidate and the candidate's solution.

allInstances :: [InstanceCandidate DecidabilityBuiltin]
allInstances =
  mkCandidate
    <$>
    --------------
    -- Property --
    --------------
    [ ( decTypeClass ValidPropertyType [tProp],
        tUnit,
        False
      )
    ]
      -------------
      -- Network --
      -------------
      <> [ ( forAllDims $ \ds1 ->
               forAllDims $ \ds2 ->
                 decTypeClass ValidNetworkType [tRatTensor ds1 ~> tRatTensor ds2],
             lamDims $ \_ds1 ->
               lamDims $ \_ds2 ->
                 tUnit,
             False
           )
         ]
      -------------
      -- Tensors --
      -------------
      <> [ ( isTensorType,
             tTensorRaw,
             True
           ),
           ( isTensorType,
             propTensor,
             False
           )
         ]
      <> tensorTypeClassCandidate FieldNot (builtinFunction Not) PropNot
      <> tensorTypeClassCandidate FieldAnd (builtinFunction And) PropAnd
      <> tensorTypeClassCandidate FieldOr (builtinFunction Or) PropOr
      <> tensorTypeClassCandidate FieldImplies (builtinFunction Implies) PropImplies
      <> tensorTypeClassCandidate FieldReduceAnd (builtinFunction ReduceAndTensor) PropAnd
      <> tensorTypeClassCandidate FieldReduceOr (builtinFunction ReduceOrTensor) PropOr
      <> tensorTypeClassCandidate FieldFromBoolTensorLiteral boolTensorToBoolTensor BoolTensorToProp
      <> tensorTypeClassCandidate FieldAtTensor (builtinFunction AtTensor) PropNaryProductAt
      <> tensorTypeClassCandidate FieldForeachTensor (builtinFunction ForeachTensor) PropNaryProductForeach
      <> comparisonCandidates Le
      <> comparisonCandidates Lt
      <> comparisonCandidates Ge
      <> comparisonCandidates Gt
      <> comparisonCandidates Eq
      <> comparisonCandidates Ne
      <> quantifierCandidates Forall
      <> quantifierCandidates Exists
      -------------
      -- Vectors --
      -------------
      <> [ ( isVectorType,
             tVectorRaw,
             True
           ),
           ( isVectorType,
             propVector,
             False
           )
         ]
      <> vectorTypeClassCandidate FieldFromVectorLiteral vectorToVector BoolVectorToProp
      <> vectorTypeClassCandidate FieldAtVector (builtinFunction AtVector) PropNaryProductAt
      <> vectorTypeClassCandidate FieldForeachVector (builtinFunction ForeachVector) PropNaryProductForeach

type TempCandidate = (DSLExpr DecidabilityBuiltin, DSLExpr DecidabilityBuiltin, Bool)

decTypeClass :: DecidabilityBuiltinTypeClass -> NonEmpty (DSLExpr DecidabilityBuiltin) -> DSLExpr DecidabilityBuiltin
decTypeClass tc args = builtin (DecidabilityBuiltinTypeClass tc) @@ args

boolTensorToBoolTensor :: DSLExpr DecidabilityBuiltin
boolTensorToBoolTensor =
  lamDims $ \ds ->
    explLam "bs" (tBoolTensor ds) $ \bs -> bs

vectorToVector :: DSLExpr DecidabilityBuiltin
vectorToVector =
  lamType $ \tElem ->
    lamDim $ \d ->
      explLam "bs" (tVector tElem d) $ \bs -> bs

vectorTypeClassCandidate ::
  VectorTypeClassField ->
  DSLExpr DecidabilityBuiltin ->
  DecidabilityBuiltinFunction ->
  [TempCandidate]
vectorTypeClassCandidate field standardOp typeOp =
  [ ( decTypeClass (HasVectorTypeClassField field) [tVectorRaw],
      standardOp,
      False
    ),
    ( decTypeClass (HasVectorTypeClassField field) [propVector],
      decFunction typeOp,
      False
    )
  ]

tensorTypeClassCandidate ::
  TensorTypeClassField ->
  DSLExpr DecidabilityBuiltin ->
  DecidabilityBuiltinFunction ->
  [TempCandidate]
tensorTypeClassCandidate field standardOp typeOp =
  [ ( decTypeClass (HasTensorTypeClassField field) [tTensorRaw],
      standardOp,
      False
    ),
    ( decTypeClass (HasTensorTypeClassField field) [propTensor],
      decFunction typeOp,
      False
    )
  ]

comparisonCandidates :: ComparisonOp -> [TempCandidate]
comparisonCandidates op =
  tensorTypeClassCandidate (FieldCompareIndex op) (builtinFunction $ CompareIndex op) (PropCompareIndex op)
    <> tensorTypeClassCandidate (FieldCompareNat op) (builtinFunction $ CompareNat op) (PropCompareNat op)
    <> tensorTypeClassCandidate (FieldCompareRatTensorPointwise op) (builtinFunction $ CompareRatTensorPointwise op) (PropCompareRatTensorPointwise op)
    <> tensorTypeClassCandidate (FieldCompareRatTensorReduced op) (builtinDerivedFunction $ CompareRatTensorReduced op) (PropCompareRatTensorPointwise op)

quantifierCandidates :: Quantifier -> [TempCandidate]
quantifierCandidates q =
  tensorTypeClassCandidate (FieldQuantifyIndex q) (builtinDerivedFunction $ QuantifyIndex q) (PropQuantifyIndex q)
    <> tensorTypeClassCandidate (FieldQuantifyInList q) (builtinDerivedFunction $ QuantifyInList q) (PropQuantifyInList q)
