module Vehicle.Data.Builtin.Loss.Instances
  ( lossBuiltinInstances,
  )
where

import Vehicle.Compile.Prelude (HasName (..), Identifier, Relevance (Irrelevant))
import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Core (InstanceCandidate (..), InstanceDatabase (..))
import Vehicle.Data.AST.Decl (InstancePriority)
import Vehicle.Data.AST.Record (FieldName (..))
import Vehicle.Data.Builtin.Core (BuiltinFunction (..))
import Vehicle.Data.Builtin.Loss
import Vehicle.Data.Code.DSL hiding (validParameterType)
import Vehicle.Data.DSL
import Vehicle.Data.DifferentiableLogic
import Vehicle.Libraries.StandardLibrary (differentiableTensorLogicIdent)

lossBuiltinInstances ::
  LossMode ->
  Identifier ->
  InstanceDatabase (LossBuiltin mode)
lossBuiltinInstances mode logic = makeInstanceDatabase (allInstances mode logic)

type TempCandidate mode =
  ( DSLExpr (LossBuiltin mode),
    DSLExpr (LossBuiltin mode),
    Maybe InstancePriority
  )

-- | Manually declared here as we currently have no way of declaring them in the language
-- itself.
allInstances :: LossMode -> Identifier -> [InstanceCandidate (LossBuiltin mode)]
allInstances mode dl =
  mkCandidate
    <$>
    ----------------------
    -- Boolean literals --
    ----------------------
    booleanLiteralCandidates dl
      ------------------------
      -- Boolean operations --
      ------------------------
      <> booleanUnaryOpCandidates dl hasNot Not PointwiseNegation
      <> booleanBinaryOpCandidates dl hasAnd And PointwiseConjunction TruthityElement
      <> booleanBinaryOpCandidates dl hasOr Or PointwiseDisjunction FalsityElement
      <> impliesCandidates dl
      <> booleanReductionOpCandidates dl hasReduceAnd ReduceAndTensor ReduceConjunction
      <> booleanReductionOpCandidates dl hasReduceOr ReduceOrTensor ReduceDisjunction
      <> ifCandidates
      <> existsCandidates
      ----------------------
      -- CompareRatTensor --
      ----------------------
      <> comparisonCandidates dl Le
      <> comparisonCandidates dl Lt
      <> comparisonCandidates dl Ge
      <> comparisonCandidates dl Gt
      <> comparisonCandidates dl Eq
      <> comparisonCandidates dl Ne
      ---------------
      -- Resources --
      ---------------
      <> networkInstances mode
      <> datasetInstances
      <> parameterInstances
      -----------
      -- Other --
      -----------
      <> maxGradientInstances

booleanLiteralCandidates :: Identifier -> [TempCandidate mode]
booleanLiteralCandidates dl =
  [ ( hasBoolLiterals tBool,
      lossCast FromBoolTensorToBoolTensor,
      Just 0
    ),
    ( hasBoolLiterals tRatWithoutGradients,
      lamDims $ \ds ->
        explLam "bs" (tBoolTensor ds) $ \bs ->
          builtinFunction WhereTensor
            .@@@ [withoutGradients, ds]
            @@ [constTensor (tRat .@@ [withoutGradients]) ds (logicField dl FalsityElement), bs, logicField dl TruthityElement],
      Nothing
    )
  ]

booleanUnaryOpCandidates ::
  Identifier ->
  ( DSLExpr (LossBuiltin mode) ->
    DSLExpr (LossBuiltin mode)
  ) ->
  BuiltinFunction ->
  TensorDifferentiableLogicField ->
  [TempCandidate mode]
booleanUnaryOpCandidates dl hasOp boolOp logicOp =
  [ ( hasOp tBool,
      builtinFunction boolOp,
      Nothing
    ),
    ( hasOp tRatWithGradients,
      logicField dl logicOp,
      Nothing
    )
  ]

booleanBinaryOpCandidates ::
  Identifier ->
  ( DSLExpr (LossBuiltin mode) ->
    DSLExpr (LossBuiltin mode) ->
    DSLExpr (LossBuiltin mode) ->
    DSLExpr (LossBuiltin mode)
  ) ->
  BuiltinFunction ->
  TensorDifferentiableLogicField ->
  TensorDifferentiableLogicField ->
  [TempCandidate mode]
booleanBinaryOpCandidates dl hasOp boolOp logicOp logicDefaultValue =
  [ ( hasOp tBool tBool tBool,
      builtinFunction boolOp,
      Nothing
    ),
    ( hasOp tBool tRatWithGradients tRatWithGradients,
      lamDims $ \dims ->
        explLam "x" (tBoolTensor dims) $ \x ->
          explLam "y" (tRatTensorWithGradients dims) $ \y ->
            builtinFunction WhereTensor .@@@ [withGradients, dims] @@ [y, x, logicField dl logicDefaultValue],
      Nothing
    ),
    ( hasOp tRatWithGradients tBool tRatWithGradients,
      lamDims $ \dims ->
        explLam "x" (tRatTensorWithGradients dims) $ \x ->
          explLam "y" (tBoolTensor dims) $ \y ->
            builtinFunction WhereTensor .@@@ [withGradients, dims] @@ [x, y, logicField dl logicDefaultValue],
      Nothing
    ),
    ( hasOp tRatWithGradients tRatWithGradients tRatWithGradients,
      logicField dl logicOp,
      Nothing
    )
  ]

impliesCandidates ::
  Identifier ->
  [TempCandidate mode]
impliesCandidates dl =
  [ ( hasImplies tBool tBool tBool,
      builtinFunction Implies,
      Nothing
    ),
    ( hasImplies tBool tRatWithGradients tRatWithGradients,
      lamDims $ \dims ->
        explLam "x" (tBoolTensor dims) $ \x ->
          explLam "y" (tRatTensorWithGradients dims) $ \y ->
            builtinFunction WhereTensor .@@@ [withGradients, dims] @@ [y, builtinFunction Not .@@@ [dims] @@ [x], logicField dl TruthityElement],
      Nothing
    ),
    ( hasImplies tRatWithGradients tBool tRatWithGradients,
      lamDims $ \dims ->
        explLam "x" (tRatTensorWithGradients dims) $ \x ->
          explLam "y" (tBoolTensor dims) $ \y ->
            builtinFunction WhereTensor .@@@ [withGradients, dims] @@ [x, builtinFunction Not .@@@ [dims] @@ [y], logicField dl TruthityElement],
      Nothing
    ),
    ( hasImplies tRatWithGradients tRatWithGradients tRatWithGradients,
      lamDims $ \dims ->
        explLam "x" (tRatTensorWithGradients dims) $ \x ->
          explLam "y" (tRatTensorWithGradients dims) $ \y ->
            logicField dl PointwiseDisjunction .@@@ [dims] @@ [logicField dl PointwiseNegation .@@@ [dims] @@ [x], y],
      Nothing
    )
  ]

booleanReductionOpCandidates ::
  Identifier ->
  (DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode)) ->
  BuiltinFunction ->
  TensorDifferentiableLogicField ->
  [TempCandidate mode]
booleanReductionOpCandidates dl hasOp boolOp logicOp =
  [ ( hasOp tBool,
      builtinFunction boolOp,
      Nothing
    ),
    ( hasOp tRatWithGradients,
      logicField dl logicOp,
      Nothing
    )
  ]

comparisonCandidates :: Identifier -> ComparisonOp -> [TempCandidate mode]
comparisonCandidates dl op =
  [ ( hasRatTensorComparison
        op
        tRatWithoutGradients
        tRatWithoutGradients
        tBool,
      builtinFunction (CompareRatTensor op),
      Nothing
    ),
    ( hasRatTensorComparison
        op
        tRatWithoutGradients
        tRatWithGradients
        tRatWithGradients,
      lamDims $ \_pDims ->
        logicField dl (PointwiseComparison op),
      Nothing
    ),
    ( hasRatTensorComparison
        op
        tRatWithGradients
        tRatWithoutGradients
        tRatWithGradients,
      lamDims $ \_pDims ->
        logicField dl (PointwiseComparison op),
      Nothing
    ),
    ( hasRatTensorComparison
        op
        tRatWithGradients
        tRatWithGradients
        tRatWithGradients,
      lamDims $ \_pDims ->
        logicField dl (PointwiseComparison op),
      Nothing
    )
  ]

ifCandidates :: [TempCandidate mode]
ifCandidates =
  [ ( hasIfRatTensor tBool,
      builtinFunction If,
      Nothing
    ),
    ( hasIfRatTensor tRatWithGradients,
      lossBuiltinFunction IfRatTensorWithGradients,
      Nothing
    )
  ]

existsCandidates :: [TempCandidate mode]
existsCandidates =
  [ ( hasExists tRatWithGradients,
      builtinFunction SearchRatTensor,
      Nothing
    ),
    ( hasExists tBool,
      builtinFunction (QuantifyRatTensor Exists),
      Nothing
    )
  ]

parameterInstances :: [TempCandidate mode]
parameterInstances =
  [ ( validParameterType (tBoolTensor dimNil),
      unitLit,
      Nothing
    ),
    ( forAllIrrelevantNat "n" $ \n ->
        validParameterType (tIndex n),
      irrelImplNatLam "n" $ const unitLit,
      Nothing
    ),
    ( validParameterType tNat,
      unitLit,
      Nothing
    ),
    ( validParameterType (tRatTensorWithoutGradients dimNil),
      unitLit,
      Nothing
    )
  ]

datasetInstances :: [TempCandidate mode]
datasetInstances =
  [ ( validDatasetType tNat,
      unitLit,
      Nothing
    ),
    ( forAllDim Irrelevant $ \d ->
        validDatasetType (tIndex d),
      lamDim $ const unitLit,
      Nothing
    ),
    ( forAllDims $ \dims ->
        validDatasetType (tNatTensor dims),
      lamDims $ const unitLit,
      Nothing
    ),
    ( forAllDims $ \dims ->
        validDatasetType (tRatTensorWithoutGradients dims),
      lamDims $ const unitLit,
      Nothing
    ),
    ( forAllTypes $ \t ->
        validDatasetType t
          .~~~> validDatasetType (tList t),
      lamType $ \t ->
        instLam "r" (validDatasetType t) $
          const unitLit,
      Nothing
    ),
    ( forAllTypes $ \t ->
        forAllDim Irrelevant $ \d ->
          validDatasetType t
            .~~~> validDatasetType (tVector t d),
      lamType $ \t ->
        lamDim $
          const $
            instLam "r" (validDatasetType t) $
              const
                unitLit,
      Nothing
    )
  ]

networkInstances :: LossMode -> [TempCandidate mode]
networkInstances mode =
  [ case mode of
      -- In train mode, where we differentiate with respect to the network weights,
      -- the output of a network always has gradients...
      Train ->
        ( forAllTypePairs $ \t1 t2 ->
            forAllGradients $ \g ->
              validNetworkIOType g t1
                .~~~> validNetworkIOType withGradients t2
                .~~~> validNetworkType (t1 ~> t2),
          lamType $ \t1 ->
            lamType $ \t2 ->
              implLam "g" tGradient $ \g ->
                instLam "r1" (validNetworkIOType g t1) $
                  const $
                    instLam "r2" (validNetworkIOType withGradients t2) $
                      const
                        unitLit,
          Nothing
        )
      -- In search mode, where we differentiate with respect to the quantified variables,
      -- a network application simply preserves gradients...
      Search ->
        ( forAllTypePairs $ \t1 t2 ->
            forAllGradients $ \g ->
              validNetworkIOType g t1
                .~~~> validNetworkIOType g t2
                .~~~> validNetworkType (t1 ~> t2),
          lamType $ \t1 ->
            lamType $ \t2 ->
              implLam "g" tGradient $ \g ->
                instLam "r1" (validNetworkIOType g t1) $
                  const $
                    instLam "r2" (validNetworkIOType g t2) $
                      const
                        unitLit,
          Nothing
        ),
    ( forAllGradients $ \g ->
        forAllDims $ \dims ->
          validNetworkIOType g (tTensor (tRat .@@ [g]) dims),
      implLam "g" tGradient $
        const $
          lamDims $
            const
              unitLit,
      Nothing
    )
  ]

maxGradientInstances :: [TempCandidate mode]
maxGradientInstances =
  [ ( maxGradients withGradients withGradients withGradients,
      unitLit,
      Nothing
    ),
    ( maxGradients withoutGradients withGradients withGradients,
      unitLit,
      Nothing
    ),
    ( maxGradients withGradients withoutGradients withGradients,
      unitLit,
      Nothing
    ),
    ( maxGradients withoutGradients withoutGradients withoutGradients,
      unitLit,
      Nothing
    )
  ]

logicField :: Identifier -> TensorDifferentiableLogicField -> DSLExpr builtin
logicField logic field = recordProj (freeVar differentiableTensorLogicIdent) (freeVar logic) (FieldName mempty $ nameOf field)
