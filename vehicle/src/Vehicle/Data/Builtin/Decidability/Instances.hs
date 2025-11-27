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
import Vehicle.Data.AST.Decl (InstancePriority, DefRecordSort (..))
import Vehicle.Data.Builtin.Core (BuiltinFunction (..), BuiltinType (..), DerivedFunction (..))
import Vehicle.Data.Builtin.Decidability
import Vehicle.Data.Code.DSL
import Vehicle.Data.DSL
import Vehicle.Compile.Prelude (Decl, GenericDecl (..), stdlibIdentifier)
import Vehicle.Data.AST.Record (FieldName(..))

decidabilityBuiltinInstances :: [Decl DecidabilityBuiltin]
decidabilityBuiltinInstances = 
  [ decidabilityTypeClass
  , _
  , _
  ]


decidabilityTypeClass :: Decl DecidabilityBuiltin
decidabilityTypeClass = DefRecord mempty (stdlibIdentifier "BooleanImplementation") (Just AnnTypeClass) [] 
  [ (FieldName mempty "BooleanTypeTC", _)
  , (FieldName mempty "FromBoolTensorLiteralTC", _)
  , (FieldName mempty "FieldNot", _)
  , (FieldName mempty "FieldAnd", _)
  , (FieldName mempty "FieldOr", _)
  , (FieldName mempty "FieldImplies", _)
  , (FieldName mempty "FieldReduceAnd", _)
  , (FieldName mempty "FieldReduceOr", _)
  , (FieldName mempty "FieldCompareNatEq", _)
  , (FieldName mempty "FieldCompareNatNe", _)
  , (FieldName mempty "FieldCompareNatLe", _)
  , (FieldName mempty "FieldCompareNatLt", _)
  , (FieldName mempty "FieldCompareNatGe", _)
  , (FieldName mempty "FieldCompareNatGt", _)
  , (FieldName mempty "FieldCompareIndexEq", _)
  , (FieldName mempty "FieldCompareIndexNe", _)
  , (FieldName mempty "FieldCompareIndexLe", _)
  , (FieldName mempty "FieldCompareIndexLt", _)
  , (FieldName mempty "FieldCompareIndexGe", _)
  , (FieldName mempty "FieldCompareIndexGt", _)
  , (FieldName mempty "FieldCompareRatTensorPointwiseEq", _)
  , (FieldName mempty "FieldCompareRatTensorPointwiseNe", _)
  , (FieldName mempty "FieldCompareRatTensorPointwiseLe", _)
  , (FieldName mempty "FieldCompareRatTensorPointwiseLt", _)
  , (FieldName mempty "FieldCompareRatTensorPointwiseGe", _)
  , (FieldName mempty "FieldCompareRatTensorPointwiseGt", _)
  , (FieldName mempty "FieldCompareRatTensorReducedEq", _)
  , (FieldName mempty "FieldCompareRatTensorReducedNe", _)
  , (FieldName mempty "FieldCompareRatTensorReducedLe", _)
  , (FieldName mempty "FieldCompareRatTensorReducedLt", _)
  , (FieldName mempty "FieldCompareRatTensorReducedGe", _)
  , (FieldName mempty "FieldCompareRatTensorReducedGt", _)
  , (FieldName mempty "FieldExistsIndex", _)
  , (FieldName mempty "FieldForallIndex", _)
  , (FieldName mempty "FieldForallInList", _)
  , (FieldName mempty "FieldExistsInList", _)
  ]

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
        Nothing
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
             Nothing
           )
         ]
      -------------
      -- Tensors --
      -------------
      <> tensorTypeClassCandidate FieldBooleanType (builtinType BoolType) PropType
      <> tensorTypeClassCandidate FieldNot (builtinFunction Not) PropNot
      <> tensorTypeClassCandidate FieldAnd (builtinFunction And) PropAnd
      <> tensorTypeClassCandidate FieldOr (builtinFunction Or) PropOr
      <> tensorTypeClassCandidate FieldImplies (builtinFunction Implies) PropImplies
      <> tensorTypeClassCandidate FieldReduceAnd (builtinFunction ReduceAndTensor) PropAnd
      <> tensorTypeClassCandidate FieldReduceOr (builtinFunction ReduceOrTensor) PropOr
      <> tensorTypeClassCandidate FieldFromBoolTensorLiteral boolTensorToBoolTensor BoolTensorToProp
      <> comparisonCandidates Le
      <> comparisonCandidates Lt
      <> comparisonCandidates Ge
      <> comparisonCandidates Gt
      <> comparisonCandidates Eq
      <> comparisonCandidates Ne
      <> quantifierCandidates Forall
      <> quantifierCandidates Exists

type TempCandidate =
  ( DSLExpr DecidabilityBuiltin,
    DSLExpr DecidabilityBuiltin,
    Maybe InstancePriority
  )

decTypeClass :: DecidabilityBuiltinTypeClass -> NonEmpty (DSLExpr DecidabilityBuiltin) -> DSLExpr DecidabilityBuiltin
decTypeClass tc args = builtin (DecidabilityBuiltinTypeClass tc) @@ args

boolTensorToBoolTensor :: DSLExpr DecidabilityBuiltin
boolTensorToBoolTensor =
  lamDims $ \ds ->
    explLam "bs" (tBoolTensor ds) $ \bs -> bs

tensorTypeClassCandidate ::
  BooleanTypeClassField ->
  DSLExpr DecidabilityBuiltin ->
  DecidabilityBuiltinFunction ->
  [TempCandidate]
tensorTypeClassCandidate field standardOp typeOp =
  [ ( decTypeClass (HasBooleanTypeClassField field) [tBool],
      standardOp,
      Nothing
    ),
    ( decTypeClass (HasBooleanTypeClassField field) [tProp],
      decFunction typeOp,
      Nothing
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
