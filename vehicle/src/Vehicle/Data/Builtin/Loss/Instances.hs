module Vehicle.Data.Builtin.Loss.Instances
  ( lossBuiltinInstances,
  )
where

import Vehicle.Compile.Prelude (HasName (..), Identifier, Relevance (Irrelevant), Visibility (..))
import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Core (InstanceCandidate (..), InstanceDatabase (..))
import Vehicle.Data.AST.Decl (InstancePriority)
import Vehicle.Data.AST.Record (FieldName (..))
import Vehicle.Data.Builtin.Core (BuiltinFunction (..))
import Vehicle.Data.Builtin.Loss
import Vehicle.Data.Code.DSL
import Vehicle.Data.DSL
import Vehicle.Data.DifferentiableLogic
import Vehicle.Libraries.Stdlib (differentiableTensorLogicIdent)

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
      <> joinGradientInstances

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
  [ ( hasIf tBool,
      builtinFunction If,
      Nothing
    ),
    ( hasIf tRatWithGradients,
      lossBuiltinFunction IfWhoseConditionHasGradients,
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

-- | The instances are split on the second argument rather than enumerating all four
-- combinations, so that the third argument is determined as soon as the second is known,
-- even if the first is not, i.e. `MaxGradients ?g1 WithGradients ?g3` solves `?g3` and
-- `MaxGradients ?g1 WithoutGradients ?g3` unifies `?g1` and `?g3`.
--
-- The default prefers no gradients, as unconstrained gradients are never needed.
maxGradientInstances :: [TempCandidate mode]
maxGradientInstances =
  [ ( forAllGradients $ \g ->
        maxGradients g withGradients withGradients,
      implLam "g" tGradient $ const unitLit,
      Nothing
    ),
    ( forAllGradients $ \g ->
        maxGradients g withoutGradients g,
      implLam "g" tGradient $ const unitLit,
      Just 0
    )
  ]

-- | The structural lifting of `MaxGradients` to types. As gradients are irrelevant, all
-- types which erase to the same type have the same representation and therefore the
-- solutions are all trivial.
joinGradientInstances :: [TempCandidate mode]
joinGradientInstances =
  [ ( forAllGradientTriples $ \g1 g2 g3 ->
        maxGradients g1 g2 g3
          .~~~> joinGradients (tRat .@@ [g1]) (tRat .@@ [g2]) (tRat .@@ [g3]),
      trivialSolution 4,
      Nothing
    ),
    ( joinGradients tBool tBool tBool,
      trivialSolution 0,
      Nothing
    ),
    ( joinGradients tNat tNat tNat,
      trivialSolution 0,
      Nothing
    ),
    ( joinGradients tUnit tUnit tUnit,
      trivialSolution 0,
      Nothing
    ),
    ( forAllIrrelevantNat "n" $ \n ->
        joinGradients (tIndex n) (tIndex n) (tIndex n),
      trivialSolution 1,
      Nothing
    ),
    ( forAllTypeTriples $ \t1 t2 t3 ->
        forAllDim Irrelevant $ \d ->
          joinGradients t1 t2 t3
            .~~~> joinGradients (tVector t1 d) (tVector t2 d) (tVector t3 d),
      trivialSolution 5,
      Nothing
    ),
    ( forAllTypeTriples $ \t1 t2 t3 ->
        forAllDims $ \ds ->
          joinGradients t1 t2 t3
            .~~~> joinGradients (tTensor t1 ds) (tTensor t2 ds) (tTensor t3 ds),
      trivialSolution 5,
      Nothing
    ),
    ( forAllTypeTriples $ \t1 t2 t3 ->
        joinGradients t1 t2 t3
          .~~~> joinGradients (tList t1) (tList t2) (tList t3),
      trivialSolution 4,
      Nothing
    )
  ]

-- | A solution that discards the given number of arguments (i.e. the instance's
-- telescope) and returns the unit literal.
trivialSolution :: Int -> DSLExpr (LossBuiltin mode)
trivialSolution n = foldr (\_ body -> lam "_" (Implicit False) Irrelevant type0 (const body)) unitLit (replicate n ())

logicField :: Identifier -> TensorDifferentiableLogicField -> DSLExpr builtin
logicField logic field = recordProj (freeVar differentiableTensorLogicIdent) (freeVar logic) (FieldName mempty $ nameOf field)
