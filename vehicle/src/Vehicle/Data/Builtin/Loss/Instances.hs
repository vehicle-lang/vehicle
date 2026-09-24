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
      <> booleanBinaryOpCandidates dl hasAnd And PointwiseConjunction (mixedConjunction dl)
      <> booleanBinaryOpCandidates dl hasOr Or PointwiseDisjunction (mixedDisjunction dl)
      <> impliesCandidates dl
      <> booleanReductionOpCandidates dl hasReduceAnd ReduceAndTensor ReduceConjunction
      <> booleanReductionOpCandidates dl hasReduceOr ReduceOrTensor ReduceDisjunction
      <> ifCandidates
      <> existsCandidates
      ----------------------
      -- CompareRatTensor --
      ----------------------
      <> pointwiseComparisonCandidates dl Le
      <> pointwiseComparisonCandidates dl Lt
      <> pointwiseComparisonCandidates dl Ge
      <> pointwiseComparisonCandidates dl Gt
      <> pointwiseComparisonCandidates dl Eq
      <> pointwiseComparisonCandidates dl Ne
      <> reducedComparisonCandidates dl Le ReduceConjunction
      <> reducedComparisonCandidates dl Lt ReduceConjunction
      <> reducedComparisonCandidates dl Ge ReduceConjunction
      <> reducedComparisonCandidates dl Gt ReduceConjunction
      <> reducedComparisonCandidates dl Eq ReduceConjunction
      <> reducedComparisonCandidates dl Ne ReduceDisjunction
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
      <> maxGradientTypeInstances

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

-- | The mixed forms of a binary boolean operation, where one side is a decidable `Bool` and
-- the other a loss value. The decidable side selects a branch, and which branch keeps the loss
-- differs between the operations, so the caller supplies the whole `where` rather than just a
-- constant for one branch: `or` has to keep the loss on the branch its condition rejects,
-- which no single default constant can express.
type MixedForm mode =
  DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode)

-- | `b and l` is `l` where the decidable side holds and false where it does not.
mixedConjunction :: Identifier -> MixedForm mode
mixedConjunction dl dims b l =
  builtinFunction WhereTensor .@@@ [withGradients, dims] @@ [l, b, logicField dl FalsityElement]

-- | `b or l` is true where the decidable side holds and `l` where it does not, so the
-- condition is negated to put the loss on the branch that is taken when `b` fails.
mixedDisjunction :: Identifier -> MixedForm mode
mixedDisjunction dl dims b l =
  builtinFunction WhereTensor
    .@@@ [withGradients, dims]
    @@ [l, builtinFunction Not .@@@ [dims] @@ [b], logicField dl TruthityElement]

booleanBinaryOpCandidates ::
  Identifier ->
  ( DSLExpr (LossBuiltin mode) ->
    DSLExpr (LossBuiltin mode) ->
    DSLExpr (LossBuiltin mode) ->
    DSLExpr (LossBuiltin mode)
  ) ->
  BuiltinFunction ->
  TensorDifferentiableLogicField ->
  MixedForm mode ->
  [TempCandidate mode]
booleanBinaryOpCandidates dl hasOp boolOp logicOp mixedForm =
  [ ( hasOp tBool tBool tBool,
      builtinFunction boolOp,
      Nothing
    ),
    -- Both operations commute, so the two mixed orders are the same expression.
    ( hasOp tBool tRatWithGradients tRatWithGradients,
      lamDims $ \dims ->
        explLam "x" (tBoolTensor dims) $ \x ->
          explLam "y" (tRatTensorWithGradients dims) $ \y ->
            mixedForm dims x y,
      Nothing
    ),
    ( hasOp tRatWithGradients tBool tRatWithGradients,
      lamDims $ \dims ->
        explLam "x" (tRatTensorWithGradients dims) $ \x ->
          explLam "y" (tBoolTensor dims) $ \y ->
            mixedForm dims y x,
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
      -- `x => y` keeps `y` where the antecedent holds and is vacuously true where it does not,
      -- so the condition is `x` itself, unnegated.
      lamDims $ \dims ->
        explLam "x" (tBoolTensor dims) $ \x ->
          explLam "y" (tRatTensorWithGradients dims) $ \y ->
            builtinFunction WhereTensor .@@@ [withGradients, dims] @@ [y, x, logicField dl TruthityElement],
      Nothing
    ),
    ( hasImplies tRatWithGradients tBool tRatWithGradients,
      -- `x => y` is `not x or y`: where the decidable consequent fails, the result is the
      -- negation of the loss.
      lamDims $ \dims ->
        explLam "x" (tRatTensorWithGradients dims) $ \x ->
          explLam "y" (tBoolTensor dims) $ \y ->
            mixedDisjunction dl dims y (logicField dl PointwiseNegation .@@@ [dims] @@ [x]),
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

reducedComparisonCandidates :: Identifier -> ComparisonOp -> TensorDifferentiableLogicField -> [TempCandidate mode]
reducedComparisonCandidates dl op reduceOp =
  [ ( hasReducedRatTensorComparison
        op
        tRatWithoutGradients
        tRatWithoutGradients
        tBool,
      builtinFunction (CompareRatTensor op) @@@ [dimNil],
      Nothing
    ),
    ( hasReducedRatTensorComparison
        op
        tRatWithoutGradients
        tRatWithGradients
        tRatWithGradients,
      lamDims $ \dims ->
        explLam "x" (tRatTensorWithoutGradients dims) $ \x ->
          explLam "y" (tRatTensorWithGradients dims) $ \y ->
            logicField dl reduceOp .@@@ [dims] @@ [logicField dl (PointwiseComparison op) .@@@ [dims] @@ [x, y]],
      Nothing
    ),
    ( hasReducedRatTensorComparison
        op
        tRatWithGradients
        tRatWithoutGradients
        tRatWithGradients,
      lamDims $ \dims ->
        explLam "x" (tRatTensorWithGradients dims) $ \x ->
          explLam "y" (tRatTensorWithoutGradients dims) $ \y ->
            logicField dl reduceOp .@@@ [dims] @@ [logicField dl (PointwiseComparison op) .@@@ [dims] @@ [x, y]],
      Nothing
    ),
    ( hasReducedRatTensorComparison
        op
        tRatWithGradients
        tRatWithGradients
        tRatWithGradients,
      lamDims $ \dims ->
        explLam "x" (tRatTensorWithGradients dims) $ \x ->
          explLam "y" (tRatTensorWithGradients dims) $ \y ->
            logicField dl reduceOp .@@@ [dims] @@ [logicField dl (PointwiseComparison op) .@@@ [dims] @@ [x, y]],
      Nothing
    )
  ]

pointwiseComparisonCandidates :: Identifier -> ComparisonOp -> [TempCandidate mode]
pointwiseComparisonCandidates dl op =
  [ ( hasPointwiseRatTensorComparison
        op
        tRatWithoutGradients
        tRatWithoutGradients
        tBool,
      lamDims $ \pDims ->
        builtinFunction (CompareRatTensor op) @@@ [pDims] .@@@ [dimNil],
      Nothing
    ),
    ( hasPointwiseRatTensorComparison
        op
        tRatWithoutGradients
        tRatWithGradients
        tRatWithGradients,
      logicField dl (PointwiseComparison op),
      Nothing
    ),
    ( hasPointwiseRatTensorComparison
        op
        tRatWithGradients
        tRatWithoutGradients
        tRatWithGradients,
      logicField dl (PointwiseComparison op),
      Nothing
    ),
    ( hasPointwiseRatTensorComparison
        op
        tRatWithGradients
        tRatWithGradients
        tRatWithGradients,
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
      builtinFunction (SearchRatTensor Exists),
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

maxGradientTypeInstances :: [TempCandidate mode]
maxGradientTypeInstances =
  [ -- Rational tensors join the gradients of their elements...
    ( forAllGradientTriples $ \g1 g2 g3 ->
        forAllDims $ \dims ->
          maxGradients g1 g2 g3
            .~~~> maxGradientTypes
              (tTensor (tRat .@@ [g1]) dims)
              (tTensor (tRat .@@ [g2]) dims)
              (tTensor (tRat .@@ [g3]) dims),
      implLam "g1" tGradient $ \g1 ->
        implLam "g2" tGradient $ \g2 ->
          implLam "g3" tGradient $ \g3 ->
            lamDims $
              const $
                instLam "r" (maxGradients g1 g2 g3) $
                  const unitLit,
      Nothing
    ),
    -- ...and every other type has to agree. The priority also settles the overlap when both
    -- branches already carry the same gradient.
    ( forAllTypes $ \t -> maxGradientTypes t t t,
      lamType $ const unitLit,
      Just 0
    )
  ]

logicField :: Identifier -> TensorDifferentiableLogicField -> DSLExpr builtin
logicField logic field = recordProj (freeVar differentiableTensorLogicIdent) (freeVar logic) (FieldName mempty $ nameOf field)
