{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use id" #-}

module Vehicle.Data.Builtin.Decidability.Instances
  ( decidabilityBuiltinInstances,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Vehicle.Compile.Prelude (Decl, Expr (..), GenericDecl (..), GenericRecordField, Name, Relevance (..), Type, stdlibIdentifier)
import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Core (InstanceCandidate (..), InstanceDatabase (..))
import Vehicle.Data.AST.Decl (DefFunctionSort (..), DefRecordSort (..), FunctionDeclAnnotation (..), InstancePriority)
import Vehicle.Data.AST.Record (FieldName (..))
import Vehicle.Data.Builtin.Core (BuiltinFunction (..), BuiltinType (..), DerivedFunction (..))
import Vehicle.Data.Builtin.Decidability
import Vehicle.Data.Code.DSL
import Vehicle.Data.DSL

decidabilityBuiltinInstances :: [Decl DecidabilityBuiltin]
decidabilityBuiltinInstances =
  [ decidabilityTypeClass,
    boolInstance,
    _
  ]

decidabilityTypeClass :: Decl DecidabilityBuiltin
decidabilityTypeClass =
  DefRecord mempty (stdlibIdentifier "BooleanImplementation") (Just AnnTypeClass) [] $
    fmap
      mkField
      [ ("BoolTypeTC", type0),
        ("FromBoolTensorLiteralTC", forAllDims $ \ds -> tBoolTensor ds ~> tTensor tBoolTC ds),
        ("NotTC", forAllDims $ \ds -> tTensor tBoolTC ds ~> tTensor tBoolTC ds),
        ("AndTC", forAllDims $ \ds -> tTensor tBoolTC ds ~> tTensor tBoolTC ds ~> tTensor tBoolTC ds),
        ("OrTC", forAllDims $ \ds -> tTensor tBoolTC ds ~> tTensor tBoolTC ds ~> tTensor tBoolTC ds),
        ("ImpliesTC", forAllDims $ \ds -> tTensor tBoolTC ds ~> tTensor tBoolTC ds ~> tTensor tBoolTC ds),
        ("ReduceAndTC", forAllDims $ \ds -> tTensor tBoolTC dimNil ~> tTensor tBoolTC ds ~> tTensor tBoolTC dimNil),
        ("ReduceOrTC", forAllDims $ \ds -> tTensor tBoolTC dimNil ~> tTensor tBoolTC ds ~> tTensor tBoolTC dimNil),
        ("CompareNatEqTC", tNat ~> tNat ~> tTensor tBoolTC dimNil),
        ("CompareNatNeTC", tNat ~> tNat ~> tTensor tBoolTC dimNil),
        ("CompareNatLeTC", tNat ~> tNat ~> tTensor tBoolTC dimNil),
        ("CompareNatLtTC", tNat ~> tNat ~> tTensor tBoolTC dimNil),
        ("CompareNatGeTC", tNat ~> tNat ~> tTensor tBoolTC dimNil),
        ("CompareNatGtTC", tNat ~> tNat ~> tTensor tBoolTC dimNil),
        ("CompareIndexEqTC", forAllDim Irrelevant $ \n -> tIndex n ~> tIndex n ~> tTensor tBoolTC dimNil),
        ("CompareIndexNeTC", forAllDim Irrelevant $ \n -> tIndex n ~> tIndex n ~> tTensor tBoolTC dimNil),
        ("CompareIndexLeTC", forAllDim Irrelevant $ \n -> tIndex n ~> tIndex n ~> tTensor tBoolTC dimNil),
        ("CompareIndexLtTC", forAllDim Irrelevant $ \n -> tIndex n ~> tIndex n ~> tTensor tBoolTC dimNil),
        ("CompareIndexGeTC", forAllDim Irrelevant $ \n -> tIndex n ~> tIndex n ~> tTensor tBoolTC dimNil),
        ("CompareIndexGtTC", forAllDim Irrelevant $ \n -> tIndex n ~> tIndex n ~> tTensor tBoolTC dimNil),
        ("CompareRatTensorPointwiseEqTC", forAllDims $ \ds -> tRatTensor ds ~> tRatTensor ds ~> tTensor tBoolTC ds),
        ("CompareRatTensorPointwiseNeTC", forAllDims $ \ds -> tRatTensor ds ~> tRatTensor ds ~> tTensor tBoolTC ds),
        ("CompareRatTensorPointwiseLeTC", forAllDims $ \ds -> tRatTensor ds ~> tRatTensor ds ~> tTensor tBoolTC ds),
        ("CompareRatTensorPointwiseLtTC", forAllDims $ \ds -> tRatTensor ds ~> tRatTensor ds ~> tTensor tBoolTC ds),
        ("CompareRatTensorPointwiseGeTC", forAllDims $ \ds -> tRatTensor ds ~> tRatTensor ds ~> tTensor tBoolTC ds),
        ("CompareRatTensorPointwiseGtTC", forAllDims $ \ds -> tRatTensor ds ~> tRatTensor ds ~> tTensor tBoolTC ds),
        ("CompareRatTensorReducedEqTC", forAllDims $ \ds -> tRatTensor ds ~> tRatTensor ds ~> tBoolTC),
        ("CompareRatTensorReducedNeTC", forAllDims $ \ds -> tRatTensor ds ~> tRatTensor ds ~> tBoolTC),
        ("CompareRatTensorReducedLeTC", forAllDims $ \ds -> tRatTensor ds ~> tRatTensor ds ~> tBoolTC),
        ("CompareRatTensorReducedLtTC", forAllDims $ \ds -> tRatTensor ds ~> tRatTensor ds ~> tBoolTC),
        ("CompareRatTensorReducedGeTC", forAllDims $ \ds -> tRatTensor ds ~> tRatTensor ds ~> tBoolTC),
        ("CompareRatTensorReducedGtTC", forAllDims $ \ds -> tRatTensor ds ~> tRatTensor ds ~> tBoolTC),
        ("ExistsIndexTC", forAllDim Relevant $ \d -> (tIndex d ~> tTensor tBoolTC dimNil) ~> tTensor tBoolTC dimNil),
        ("ForallIndexTC", forAllDim Relevant $ \d -> (tIndex d ~> tTensor tBoolTC dimNil) ~> tTensor tBoolTC dimNil),
        ("ForallInListTC", forAllTypes $ \tElem -> (tElem ~> tTensor tBoolTC dimNil) ~> tList tElem ~> tTensor tBoolTC dimNil),
        ("ExistsInListTC", forAllTypes $ \tElem -> (tElem ~> tTensor tBoolTC dimNil) ~> tList tElem ~> tTensor tBoolTC dimNil)
      ]
  where
    mkField :: (Name, DSLExpr DecidabilityBuiltin) -> GenericRecordField (Type DecidabilityBuiltin)
    mkField (name, typ) = (FieldName mempty name, fromDSL mempty _)

    tBoolTC :: DSLExpr DecidabilityBuiltin
    tBoolTC = _

implementationType :: Expr DecidabilityBuiltin
implementationType = FreeVar mempty (stdlibIdentifier "BooleanImplementation")

boolInstance :: Decl DecidabilityBuiltin
boolInstance =
  DefFunction
    mempty
    (stdlibIdentifier "boolImpl")
    (FunctionDecl 0 $ Just $ AnnInstance Nothing)
    implementationType
    $ Record mempty implementationType
    $ fmap
      mkField
      [ ("BoolTypeTC", tBool),
        ("FromBoolTensorLiteralTC", boolTensorToBoolTensor),
        ("NotTC", builtinFunction Not),
        ("AndTC", builtinFunction And),
        ("OrTC", builtinFunction Or),
        ("ImpliesTC", builtinFunction Implies),
        ("ReduceAndTC", builtinFunction ReduceAndTensor),
        ("ReduceOrTC", builtinFunction ReduceOrTensor),
        ("CompareNatEqTC", builtinFunction $ CompareNat Eq),
        ("CompareNatNeTC", builtinFunction $ CompareNat Ne),
        ("CompareNatLeTC", builtinFunction $ CompareNat Le),
        ("CompareNatLtTC", builtinFunction $ CompareNat Lt),
        ("CompareNatGeTC", builtinFunction $ CompareNat Ge),
        ("CompareNatGtTC", builtinFunction $ CompareNat Gt),
        ("CompareIndexEqTC", builtinFunction $ CompareIndex Eq),
        ("CompareIndexNeTC", builtinFunction $ CompareIndex Ne),
        ("CompareIndexLeTC", builtinFunction $ CompareIndex Le),
        ("CompareIndexLtTC", builtinFunction $ CompareIndex Lt),
        ("CompareIndexGeTC", builtinFunction $ CompareIndex Ge),
        ("CompareIndexGtTC", builtinFunction $ CompareIndex Gt),
        ("CompareRatTensorPointwiseEqTC", builtinFunction $ CompareRatTensorPointwise Eq),
        ("CompareRatTensorPointwiseNeTC", builtinFunction $ CompareRatTensorPointwise Ne),
        ("CompareRatTensorPointwiseLeTC", builtinFunction $ CompareRatTensorPointwise Le),
        ("CompareRatTensorPointwiseLtTC", builtinFunction $ CompareRatTensorPointwise Lt),
        ("CompareRatTensorPointwiseGeTC", builtinFunction $ CompareRatTensorPointwise Ge),
        ("CompareRatTensorPointwiseGtTC", builtinFunction $ CompareRatTensorPointwise Gt),
        ("CompareRatTensorReducedEqTC", builtinDerivedFunction $ CompareRatTensorReduced Eq),
        ("CompareRatTensorReducedNeTC", builtinDerivedFunction $ CompareRatTensorReduced Ne),
        ("CompareRatTensorReducedLeTC", builtinDerivedFunction $ CompareRatTensorReduced Le),
        ("CompareRatTensorReducedLtTC", builtinDerivedFunction $ CompareRatTensorReduced Lt),
        ("CompareRatTensorReducedGeTC", builtinDerivedFunction $ CompareRatTensorReduced Ge),
        ("CompareRatTensorReducedGtTC", builtinDerivedFunction $ CompareRatTensorReduced Gt),
        ("ExistsIndexTC", builtinDerivedFunction $ QuantifyIndex Exists),
        ("ForallIndexTC", builtinDerivedFunction $ QuantifyIndex Forall),
        ("ForallInListTC", builtinDerivedFunction $ QuantifyInList Exists),
        ("ExistsInListTC", builtinDerivedFunction $ QuantifyInList Forall)
      ]
  where
    mkField :: (Name, DSLExpr DecidabilityBuiltin) -> GenericRecordField (Type DecidabilityBuiltin)
    mkField (name, typ) = (FieldName mempty name, fromDSL mempty _)

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
