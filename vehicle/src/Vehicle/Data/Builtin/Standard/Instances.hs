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
            lamDims $ const tUnit,
            False
          ),
          ( forAllTypes $ \tElem ->
              forAllDims $ \d ->
                validPropertyType tElem
                  .~~~> validPropertyType (tVector tElem d),
            lamType $ \tElem ->
              lamDim $ \_d ->
                instLam "r1" (validPropertyType tElem) $ const tUnit,
            False
          ),
          ------------------------------------
          -- ValidNonInferableParameterType --
          ------------------------------------
          ( validNonInferableParameterType (tBoolTensor dimNil),
            unitLit,
            False
          ),
          ( forAllIrrelevantNat "n" $ \n ->
              validNonInferableParameterType (tIndex n),
            irrelImplNatLam "n" $ const unitLit,
            False
          ),
          ( validNonInferableParameterType tNat,
            unitLit,
            False
          ),
          ( validNonInferableParameterType (tRatTensor dimNil),
            unitLit,
            False
          ),
          ---------------------------------
          -- ValidInferableParameterType --
          ---------------------------------
          ( validInferableParameterType tNat,
            unitLit,
            False
          ),
          ----------------------
          -- ValidDatasetType --
          ----------------------
          ( forAllTypes $ \t ->
              validDatasetListElementType t
                .~~~> validDatasetType (tList t),
            implLam "t" type0 $ \t ->
              instLam "r1" (validDatasetListElementType t) $ const tUnit,
            False
          ),
          ( forAllTypes $ \t ->
              forAllDim Irrelevant $ \d ->
                validDatasetListElementType t
                  .~~~> validDatasetType (tVector t d),
            implLam "t" type0 $ \t ->
              lam "d" (Implicit False) Irrelevant tDim $ \_d ->
                instLam "r1" (validDatasetListElementType t) $ const tUnit,
            False
          ),
          ( forAllTypes $ \t ->
              forAllDims $ \ds ->
                validDatasetTensorElementType t
                  .~~~> validDatasetType (tTensor t ds),
            implLam "t" type0 $ \t ->
              lamDims $ \_ds ->
                instLam "r1" (validDatasetTensorElementType t) $ const tUnit,
            False
          ),
          -- List element types
          ( forAllTypes $ \t ->
              validDatasetListElementType t
                .~~~> validDatasetListElementType (tList t),
            implLam "t" type0 $ \t ->
              instLam "r1" (validDatasetListElementType t) $ const tUnit,
            False
          ),
          ( forAllTypes $ \t ->
              forAllDim Irrelevant $ \d ->
                validDatasetListElementType t
                  .~~~> validDatasetListElementType (tVector t d),
            implLam "t" type0 $ \t ->
              lam "d" (Implicit False) Irrelevant tDim $ \_d ->
                instLam "r1" (validDatasetListElementType t) $ const tUnit,
            False
          ),
          ( forAllTypes $ \t ->
              forAllDims $ \ds ->
                validDatasetTensorElementType t
                  .~~~> validDatasetListElementType (tTensor t ds),
            implLam "t" type0 $ \t ->
              lamDims $ \_ds ->
                instLam "r1" (validDatasetTensorElementType t) $ const tUnit,
            False
          ),
          ( forAllIrrelevantNat "n" $ \n ->
              validDatasetListElementType (tIndex n),
            irrelImplNatLam "n" $ const tUnit,
            False
          ),
          ( validDatasetListElementType tNat,
            tUnit,
            False
          ),
          -- Element typs
          ( forAllIrrelevantNat "n" $ \n ->
              validDatasetTensorElementType (tIndex n),
            irrelImplNatLam "n" $ const tUnit,
            False
          ),
          ( validDatasetTensorElementType tNat,
            tUnit,
            False
          ),
          ( validDatasetTensorElementType tRat,
            tUnit,
            False
          ),
          ----------------------
          -- ValidNetworkType --
          ----------------------
          ( forAllDims $ \ds1 ->
              forAllDims $ \ds2 ->
                validNetworkType (tRatTensor ds1 ~> tRatTensor ds2),
            lamDims $ \_ds1 ->
              lamDims $ const tUnit,
            False
          ),
          -------------------------
          -- ValidTensorLikeType --
          -------------------------
          ( forAllTypes $ \t ->
              forAllDims $ \ds ->
                validTensorLikeType (tTensor t ds),
            lamType $ \_t ->
              lamDim $ const tUnit,
            False
          ),
          -- ----------------
          -- HasRatLits --
          ----------------
          ( hasRatLits (tRatTensor dimNil),
            builtinCast FromRatToRat,
            False
          ),
          ----------------
          -- HasNatLits --
          ----------------
          ( forAllIrrelevantNat "n" $ \n ->
              hasNatLits (tIndex n),
            irrelImplNatLam "n" $ \n ->
              builtinCast FromNatToIndex .@@@ [n],
            False
          ),
          ( hasNatLits tNat,
            builtinCast FromNatToNat,
            True
          ),
          ( hasNatLits (tRatTensor dimNil),
            builtinCast FromNatToRat,
            False
          ),
          ----------------
          -- HasVecLits --
          ----------------
          ( forAllTypes $ \tElem ->
              hasVecLits (lamExplDim $ \_d -> tList tElem) tElem,
            implLam "tElem" type0 $ \tElem ->
              builtinCast FromVecToList @@@ [tElem],
            False
          ),
          ( forAllTypes $ \t ->
              hasVecLits (lamExplDim $ \d -> tVector t d) t,
            implLam "tElem" type0 $ \tElem ->
              builtinCast FromVecToTensor @@@ [tElem],
            False
          ),
          ( forAllTypes $ \tElem ->
              forAllDims $ \ds ->
                hasVecLits (lamExplDim $ \d -> tTensor tElem (dimCons d ds)) (tTensor tElem ds),
            implLam "tElem" type0 $ \tElem ->
              lamDims $ \ds ->
                builtinCast FromVecToTensor @@@ [tElem] .@@@ [ds],
            False
          ),
          ------------------
          -- IsTensorType --
          ------------------
          ( forAllDims $ \ds ->
              isTensorType tBool ds,
            lamDims $ \ds ->
              tTensor tBool ds,
            False
          ),
          ( forAllDims $ \ds ->
              isTensorType tRat ds,
            lamDims $ \ds ->
              tTensor tRat ds,
            False
          ),
          ( forAllIrrelevant "ds" tDims $ \ds ->
              forAllTypes $ \t ->
                isTensorType (tTensor t dimNil) ds,
            lamDims $ \ds ->
              implLam "t" type0 $ \t ->
                tTensor t ds,
            False
          ),
          ------------
          -- HasNeg --
          ------------
          ( forAllDims $ \dims -> hasNeg (tRatTensor dims) (tRatTensor dims),
            lamDims $ \dims -> builtinFunction (NegRatTensor) .@@@ [dims],
            False
          ),
          ------------
          -- HasAdd --
          ------------
          ( hasAdd tNat tNat tNat,
            builtinFunction (AddNat),
            True
          ),
          ( forAllDims $ \dims -> hasAdd (tRatTensor dims) (tRatTensor dims) (tRatTensor dims),
            lamDims $ \dims -> builtinFunction (AddRatTensor) .@@@ [dims],
            False
          ),
          ------------
          -- HasSub --
          ------------
          ( forAllDims $ \dims -> hasSub (tRatTensor dims) (tRatTensor dims) (tRatTensor dims),
            lamDims $ \dims -> builtinFunction (SubRatTensor) .@@@ [dims],
            False
          ),
          ------------
          -- HasMul --
          ------------
          ( hasMul tNat tNat tNat,
            builtinFunction (MulNat),
            True
          ),
          ( forAllDims $ \dims -> hasMul (tRatTensor dims) (tRatTensor dims) (tRatTensor dims),
            lamDims $ \dims -> builtinFunction (MulRatTensor) .@@@ [dims],
            False
          ),
          ------------
          -- HasDiv --
          ------------
          ( forAllDims $ \dims -> hasDiv (tRatTensor dims) (tRatTensor dims) (tRatTensor dims),
            lamDims $ \dims -> builtinFunction (DivRatTensor) .@@@ [dims],
            False
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
            False
          ),
          ( forAllTypes $ \tElem ->
              forAllDim Irrelevant $ \d ->
                forAllDims $ \ds ->
                  hasAt (tTensor tElem (cons tDim d ds)) (tIndex d) (tTensor tElem ds),
            lamType $ \tElem ->
              lamDim $ \d ->
                lamDims $ \ds ->
                  builtinFunction AtTensor @@@ [tElem] .@@@ [d, ds],
            False
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
            False
          ),
          ( forAllTypes $ \tElem ->
              forAllDim Relevant $ \d ->
                forAllDims $ \ds ->
                  hasForeach (tTensor tElem (cons tDim d ds)) (tIndex d) (tTensor tElem ds),
            lamType $ \tElem ->
              lam "d" (Implicit False) Relevant tDim $ \d ->
                lamDims $ \ds ->
                  builtinFunction ForeachTensor @@@ [tElem, d] .@@@ [ds],
            False
          ),
          ------------
          -- HasMap --
          ------------
          ( hasMap tListRaw,
            builtinFunction MapList,
            True
          ),
          ------------
          -- HasFold --
          ------------
          ( hasFold tListRaw,
            builtinFunction FoldList,
            False
          )
        ]
      <> quantifierCandidates Forall
      <> quantifierCandidates Exists
      -----------------
      -- Comparisons --
      -----------------
      <> [ ( hasCompare tNat tNat (tBoolTensor dimNil),
             builtinInstance Nothing "natHasCompare",
             True
           ),
           -- We separate out the zero-dimensional tensor case so that we have a unique
           -- representation of comparisons over zero tensors. Otherwise, we end up
           -- having both pointwise and reduced comparisons.
           ( hasCompare (tRatTensor dimNil) (tRatTensor dimNil) (tBoolTensor dimNil),
             builtinInstance Nothing "zeroDimRatTensorHasCompare",
             False
           ),
           ( forAllDim Irrelevant $ \d ->
               forAllDims $ \dims ->
                 hasCompare (tRatTensor (dimCons d dims)) (tRatTensor (dimCons d dims)) (tBoolTensor dimNil),
             lamDim $ \d ->
               lamDims $ \dims ->
                 builtinInstance Nothing "nonZeroDimRatTensorHasCompare" .@@@ [d, dims],
             False
           ),
           ( forAll "d1" tNat $ \n1 ->
               forAll "d2" tNat $ \n2 ->
                 hasCompare (tIndex n1) (tIndex n2) (tBoolTensor dimNil),
             lamDim $ \d1 ->
               lamDim $ \d2 ->
                 builtinInstance Nothing "indexHasCompare" .@@@ [d1, d2],
             False
           )
         ]
  where
    quantifierCandidates ::
      Quantifier ->
      [(DSLExpr Builtin, DSLExpr Builtin, Bool)]
    quantifierCandidates q =
      [ ( forAllNat $ \n ->
            hasQuantifier q (tIndex n),
          lamDim $ \n ->
            builtinDerivedFunction (QuantifyIndex q) @@@ [n],
          False
        ),
        ( forAllDims $ \ds ->
            hasQuantifier q (tRatTensor ds),
          lamDims $ \ds ->
            builtinFunction (if q == Forall then ForallRatTensor else ExistsRatTensor) @@@ [ds],
          False
        )
      ]
