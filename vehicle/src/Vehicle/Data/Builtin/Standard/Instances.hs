{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use id" #-}

module Vehicle.Data.Builtin.Standard.Instances
  ( standardBuiltinInstances,
  )
where

import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Core (InstanceCandidate (..), InstanceDatabase (..))
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.DSL
import Vehicle.Data.DSL
import Vehicle.Prelude

standardBuiltinInstances :: InstanceDatabase Builtin
standardBuiltinInstances = makeInstanceDatabase allInstances

-- Manually declared here as we have no way of declaring them in the language
-- itself.

-- Also note that annoyingly because of a lack of first class records we have
-- to duplicate the context for both the candidate and the candidate's solution.

allInstances :: [InstanceCandidate Builtin]
allInstances =
  mkCandidate
    <$> [
          -----------------------
          -- ValidPropertyType --
          -----------------------
          ( forAllDims $ \ds ->
              validPropertyType (tBoolTensor ds),
            lamDims $ \_ds ->
              tUnit,
            Nothing
          ),
          ( forAllTypes $ \tElem ->
              forAllDims $ \d ->
                validPropertyType tElem
                  .~~~> validPropertyType (tVector tElem d),
            lamType $ \tElem ->
              lamDim $ \_d ->
                instLam "r1" (validPropertyType tElem) $ \_inst ->
                  tUnit,
            Nothing
          ),
          ------------------------------------
          -- ValidNonInferableParameterType --
          ------------------------------------
          ( validNonInferableParameterType (tBoolTensor dimNil),
            unitLit,
            Nothing
          ),
          ( forAllIrrelevantNat "n" $ \n ->
              validNonInferableParameterType (tIndex n),
            irrelImplNatLam "n" $ \_n ->
              unitLit,
            Nothing
          ),
          ( validNonInferableParameterType tNat,
            unitLit,
            Nothing
          ),
          ( validNonInferableParameterType (tRatTensor dimNil),
            unitLit,
            Nothing
          ),
          ---------------------------------
          -- ValidInferableParameterType --
          ---------------------------------
          ( validInferableParameterType tNat,
            unitLit,
            Nothing
          ),
          ----------------------
          -- ValidDatasetType --
          ----------------------
          ( forAllTypes $ \t ->
              validDatasetListElementType t
                .~~~> validDatasetType (tList t),
            implLam "t" type0 $ \t ->
              instLam "r1" (validDatasetListElementType t) $ \_ ->
                tUnit,
            Nothing
          ),
          ( forAllTypes $ \t ->
              forAllDim Irrelevant $ \d ->
                validDatasetListElementType t
                  .~~~> validDatasetType (tVector t d),
            implLam "t" type0 $ \t ->
              lam "d" (Implicit False) Irrelevant tDim $ \_d ->
                instLam "r1" (validDatasetListElementType t) $ \_ ->
                  tUnit,
            Nothing
          ),
          ( forAllTypes $ \t ->
              forAllDims $ \ds ->
                validDatasetTensorElementType t
                  .~~~> validDatasetType (tTensor t ds),
            implLam "t" type0 $ \t ->
              lamDims $ \_ds ->
                instLam "r1" (validDatasetTensorElementType t) $ \_ ->
                  tUnit,
            Nothing
          ),
          -- List element types
          ( forAllTypes $ \t ->
              validDatasetListElementType t
                .~~~> validDatasetListElementType (tList t),
            implLam "t" type0 $ \t ->
              instLam "r1" (validDatasetListElementType t) $ \_ ->
                tUnit,
            Nothing
          ),
          ( forAllTypes $ \t ->
              forAllDim Irrelevant $ \d ->
                validDatasetListElementType t
                  .~~~> validDatasetListElementType (tVector t d),
            implLam "t" type0 $ \t ->
              lam "d" (Implicit False) Irrelevant tDim $ \_d ->
                instLam "r1" (validDatasetListElementType t) $ \_ ->
                  tUnit,
            Nothing
          ),
          ( forAllTypes $ \t ->
              forAllDims $ \ds ->
                validDatasetTensorElementType t
                  .~~~> validDatasetListElementType (tTensor t ds),
            implLam "t" type0 $ \t ->
              lamDims $ \_ds ->
                instLam "r1" (validDatasetTensorElementType t) $ \_ ->
                  tUnit,
            Nothing
          ),
          ( forAllIrrelevantNat "n" $ \n ->
              validDatasetListElementType (tIndex n),
            irrelImplNatLam "n" $ \_n ->
              tUnit,
            Nothing
          ),
          ( validDatasetListElementType tNat,
            tUnit,
            Nothing
          ),
          -- Element typs
          ( forAllIrrelevantNat "n" $ \n ->
              validDatasetTensorElementType (tIndex n),
            irrelImplNatLam "n" $ \_n ->
              tUnit,
            Nothing
          ),
          ( validDatasetTensorElementType tNat,
            tUnit,
            Nothing
          ),
          ( validDatasetTensorElementType tRat,
            tUnit,
            Nothing
          ),
          ----------------------
          -- ValidNetworkType --
          ----------------------
          -- ( forAllDims $ \ds1 ->
          --     forAllDims $ \ds2 ->
          --       validNetworkType (tRatTensor ds1 ~> tRatTensor ds2),
          --   lamDims $ \_ds1 ->
          --     lamDims $ \_ds2 ->
          --       tUnit,
          --   Nothing
          -- ),
          -------------------------
          -- ValidTensorLikeType --
          -------------------------
          ( forAllTypes $ \t ->
              forAllDims $ \ds ->
                validTensorLikeType (tTensor t ds),
            lamType $ \_t ->
              lamDim $ \_ds ->
                tUnit,
            Nothing
          ),
          -- ----------------
          -- HasRatLits --
          ----------------
          ( hasRatLits (tRatTensor dimNil),
            builtinCast (FromRat FromRatToRat),
            Nothing
          ),
          ----------------
          -- HasNatLits --
          ----------------
          ( forAllIrrelevantNat "n" $ \n ->
              hasNatLits (tIndex n),
            irrelImplNatLam "n" $ \n ->
              builtinCast (FromNat FromNatToIndex) .@@@ [n],
            Nothing
          ),
          ( hasNatLits tNat,
            builtinCast (FromNat FromNatToNat),
            Just 0
          ),
          ( hasNatLits (tRatTensor dimNil),
            builtinCast (FromNat FromNatToRat),
            Nothing
          ),
          ----------------
          -- HasVecLits --
          ----------------
          ( forAllTypes $ \t ->
              forAllDim Irrelevant $ \d ->
                forAllDims $ \ds ->
                  hasVecLits (tTensor t (dimCons d ds)) (tTensor t ds) d,
            implLam "t" type0 $ \t ->
              lamDim $ \d ->
                lamDims $ \ds ->
                  builtinFunction StackTensor @@@ [t, d] .@@@ [ds],
            Nothing
          ),
          ( forAllTypes $ \t ->
              forAllDim Irrelevant $ \d ->
                hasVecLits (tVector t d) t d,
            implLam "t" type0 $ \t ->
              lamDim $ \d ->
                builtinConstructor VectorLiteral @@@ [t, d],
            Nothing
          ),
          ( forAllTypes $ \t ->
              forAllDim Irrelevant $ \d ->
                hasVecLits (tList t) t d,
            implLam "t" type0 $ \t ->
              lamDim $ \d ->
                builtinCast FromVectorToList @@@ [t, d],
            Nothing
          ),
          ------------------
          -- IsTensorType --
          ------------------
          ( forAllDims $ \ds ->
              isTensorType tBool ds,
            lamDims $ \ds ->
              tTensor tBool ds,
            Nothing
          ),
          ( forAllDims $ \ds ->
              isTensorType tRat ds,
            lamDims $ \ds ->
              tTensor tRat ds,
            Nothing
          ),
          ( forAllIrrelevant "ds" tDims $ \ds ->
              forAllTypes $ \t ->
                isTensorType (tTensor t dimNil) ds,
            lamDims $ \ds ->
              implLam "t" type0 $ \t ->
                tTensor t ds,
            Nothing
          ),
          ------------
          -- HasNeg --
          ------------
          ( forAllDims $ \dims -> hasNeg (tRatTensor dims) (tRatTensor dims),
            lamDims $ \dims -> builtinFunction (Neg NegRatTensor) .@@@ [dims],
            Nothing
          ),
          ------------
          -- HasAt --
          ------------
          ( forAllTypes $ \tElem ->
              forAllDim Irrelevant $ \d ->
                hasAt (tVector tElem d) (tIndex d) tElem,
            lamType $ \tElem ->
              lamDim $ \d ->
                builtinFunction AtVector @@@ [tElem] .@@@ [d],
            Nothing
          ),
          ( forAllTypes $ \tElem ->
              forAllDim Irrelevant $ \d ->
                forAllDims $ \ds ->
                  hasAt (tTensor tElem (cons tDim d ds)) (tIndex d) (tTensor tElem ds),
            lamType $ \tElem ->
              lamDim $ \d ->
                lamDims $ \ds ->
                  builtinFunction AtTensor @@@ [tElem] .@@@ [d, ds],
            Nothing
          ),
          ------------
          -- HasForeach --
          ------------
          ( forAllTypes $ \tElem ->
              forAllDim Relevant $ \d ->
                hasForeach (tVector tElem d) (tIndex d) tElem,
            lamType $ \tElem ->
              lam "d" (Implicit False) Relevant tDim $ \d ->
                builtinFunction ForeachVector @@@ [tElem, d],
            Nothing
          ),
          ( forAllTypes $ \tElem ->
              forAllDim Relevant $ \d ->
                forAllDims $ \ds ->
                  hasForeach (tTensor tElem (cons tDim d ds)) (tIndex d) (tTensor tElem ds),
            lamType $ \tElem ->
              lam "d" (Implicit False) Relevant tDim $ \d ->
                lamDims $ \ds ->
                  builtinFunction ForeachTensor @@@ [tElem, d] .@@@ [ds],
            Nothing
          ),
          ------------
          -- HasMap --
          ------------
          ( hasMap tListRaw,
            builtinFunction MapList,
            Nothing
          ),
          ------------
          -- HasFold --
          ------------
          ( hasFold tListRaw,
            builtinFunction FoldList,
            Nothing
          )
        ]
      <> comparisonCandidates Le
      <> comparisonCandidates Lt
      <> comparisonCandidates Ge
      <> comparisonCandidates Gt
      <> comparisonCandidates Eq
      <> comparisonCandidates Ne
      <> quantifierCandidates Forall
      <> quantifierCandidates Exists
  where
    comparisonCandidates :: ComparisonOp -> [(DSLExpr Builtin, DSLExpr Builtin, Maybe InstancePriority)]
    comparisonCandidates op =
      [ ( forAll "n1" tNat $ \n1 ->
            forAll "n2" tNat $ \n2 ->
              hasCompare op (tIndex n1) (tIndex n2) (tBoolTensor dimNil),
          implLam "n1" tNat $ \n1 ->
            implLam "n2" tNat $ \n2 ->
              builtinFunction (CompareIndex op) @@@ [n1, n2],
          Nothing
        ),
        ( hasCompare op tNat tNat (tBoolTensor dimNil),
          builtinFunction (CompareNat op),
          Just 0
        ),
        -- We separate out the zero-dimensional tensor case so that we have a unique
        -- representation of comparisons over zero tensors. Otherwise, we end up
        -- having both pointwise and reduced comparisons.
        ( hasCompare op (tRatTensor dimNil) (tRatTensor dimNil) (tBoolTensor dimNil),
          builtinFunction (CompareRatTensorPointwise op) .@@@ [dimNil],
          Just 1
        ),
        ( forAllDim Irrelevant $ \d ->
            forAllDims $ \dims ->
              hasCompare op (tRatTensor (dimCons d dims)) (tRatTensor (dimCons d dims)) (tBoolTensor dimNil),
          lamDim $ \d ->
            lamDims $ \dims ->
              builtinDerivedFunction (CompareRatTensorReduced op) .@@@ [d, dims],
          Nothing
        )
      ]

    quantifierCandidates ::
      Quantifier ->
      [(DSLExpr Builtin, DSLExpr Builtin, Maybe InstancePriority)]
    quantifierCandidates q =
      [ ( forAllNat $ \n ->
            hasQuantifier q (tIndex n),
          lamDim $ \n ->
            builtinDerivedFunction (QuantifyIndex q) @@@ [n],
          Nothing
        ),
        ( forAllDims $ \ds ->
            hasQuantifier q (tRatTensor ds),
          lamDims $ \ds ->
            builtinFunction (QuantifyRatTensor q) @@@ [ds],
          Nothing
        )
      ]
