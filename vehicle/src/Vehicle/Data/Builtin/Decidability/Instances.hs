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
import Vehicle.Prelude (Relevance (..))
import Vehicle.Syntax.AST.Visibility (Visibility (..))

decidabilityBuiltinInstances :: InstanceDatabase DecidabilityBuiltin
decidabilityBuiltinInstances = makeInstanceDatabase allInstances mempty

-- Manually declared here as we have no way of declaring them in the language
-- itself.

-- Also note that annoyingly because of a lack of first class records we have
-- to duplicate the context for both the candidate and the candidate's solution.

allInstances :: [InstanceCandidate DecidabilityBuiltin]
allInstances =
  mkCandidate
    <$> [ ( decTypeClass IsBoolType [tBool],
            tBool,
            False
          ),
          ( decTypeClass IsBoolType [tProp],
            tProp,
            False
          )
        ]
      <> [ ( decTypeClass HasBoolTensorLiterals [tTensorRaw @@ [tBool]],
             lamDims $ \ds ->
               explLam "bs" (tBoolTensor ds) $ \bs ->
                 bs,
             False
           ),
           ( decTypeClass HasBoolTensorLiterals [propIgnoreDims],
             decFunction BoolTensorToProp,
             False
           )
         ]
      <> dimsCandidate HasNot Not PropNot
      <> dimsCandidate HasAnd And PropAnd
      <> dimsCandidate HasOr Or PropOr
      <> dimsCandidate HasImplies Implies PropImplies
      <> dimsCandidate HasReduceAndTensor ReduceAndTensor PropAnd
      <> dimsCandidate HasReduceOrTensor ReduceOrTensor PropOr
      <> comparisonCandidates Le
      <> comparisonCandidates Lt
      <> comparisonCandidates Ge
      <> comparisonCandidates Gt
      <> comparisonCandidates Eq
      <> comparisonCandidates Ne
      <> quantifierCandidates Forall
      <> quantifierCandidates Exists
      <> [
           ------------------
           -- IsTensorType --
           ------------------
           ( isTensorType tBool,
             tTensorRaw @@ [tBool],
             False
           ),
           ( isTensorType tProp,
             lam "ds" Explicit Relevant tDims $ \_ds ->
               tProp,
             False
           ),
           ( isTensorType tNat,
             tTensorRaw @@ [tNat],
             False
           ),
           ( isTensorType tRat,
             tTensorRaw @@ [tRat],
             False
           ),
           ------------------
           -- IsVectorType --
           ------------------
           ( isVectorType tProp,
             lam "d" Explicit Relevant tDim $ \_d ->
               tProp,
             False
           ),
           ( forAllTypes $ \tElem ->
               isVectorType tElem,
             lamType $ \tElem ->
               tVectorRaw @@ [tElem],
             False
           )
         ]

type TempCandidate = (DSLExpr DecidabilityBuiltin, DSLExpr DecidabilityBuiltin, Bool)

decTypeClass :: DecidabilityBuiltinTypeClass -> NonEmpty (DSLExpr DecidabilityBuiltin) -> DSLExpr DecidabilityBuiltin
decTypeClass tc args = builtin (DecidabilityBuiltinTypeClass tc) @@ args

dimsCandidate ::
  DecidabilityBuiltinTypeClass ->
  BuiltinFunction ->
  DecidabilityBuiltinFunction ->
  [TempCandidate]
dimsCandidate tc standardOp typeOp =
  [ ( forAllDims $ \dims ->
        decTypeClass tc [tTensorRaw @@ [tBool], dims],
      lamDims $ \dims ->
        builtinFunction standardOp .@@@ [dims],
      False
    ),
    ( forAllDims $ \dims ->
        decTypeClass tc [propIgnoreDims, dims],
      lamDims $ \_dims ->
        decFunction typeOp,
      False
    )
  ]

nonDimsCandidate ::
  DecidabilityBuiltinTypeClass ->
  DSLExpr DecidabilityBuiltin ->
  DecidabilityBuiltinFunction ->
  [TempCandidate]
nonDimsCandidate tc standardOp typeOp =
  [ ( decTypeClass tc [tTensor tBool dimNil],
      standardOp,
      False
    ),
    ( decTypeClass tc [tProp],
      decFunction typeOp,
      False
    )
  ]

comparisonCandidates :: ComparisonOp -> [TempCandidate]
comparisonCandidates op =
  nonDimsCandidate (HasCompareIndex op) (builtinFunction $ CompareIndex op) (PropCompareIndex op)
    <> nonDimsCandidate (HasCompareNat op) (builtinFunction $ CompareNat op) (PropCompareNat op)
    <> dimsCandidate (HasCompareRatTensorPointwise op) (CompareRatTensorPointwise op) (PropCompareRatTensorPointwise op)
    <> nonDimsCandidate (HasCompareRatTensorReduced op) (builtinDerivedFunction $ CompareRatTensorReduced op) (PropCompareRatTensorPointwise op)

quantifierCandidates :: Quantifier -> [TempCandidate]
quantifierCandidates q =
  [ ( decTypeClass (HasQuantifyIndex q) [tTensor tBool dimNil],
      builtinDerivedFunction (QuantifyIndex q),
      False
    ),
    ( decTypeClass (HasQuantifyIndex q) [tProp],
      decFunction (PropQuantifyIndex q),
      False
    ),
    ( decTypeClass (HasQuantifyInList q) [tTensor tBool dimNil],
      builtinDerivedFunction (QuantifyInList q),
      False
    ),
    ( decTypeClass (HasQuantifyInList q) [tProp],
      decFunction (PropQuantifyInList q),
      False
    )
  ]
