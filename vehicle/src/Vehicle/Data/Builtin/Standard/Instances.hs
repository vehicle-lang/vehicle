{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use id" #-}

module Vehicle.Data.Builtin.Standard.Instances
  ( standardBuiltinInstances,
  )
where

import Data.HashMap.Strict (HashMap)
import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Core (InstanceCandidate (..), InstanceDatabase (..), InstanceSearchDepth)
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.DSL
import Vehicle.Data.DSL
import Vehicle.Prelude

standardBuiltinInstances :: InstanceDatabase Builtin
standardBuiltinInstances = makeInstanceDatabase allInstances searchDepth

searchDepth :: HashMap Builtin InstanceSearchDepth
searchDepth =
  [ (TypeClass HasVecLits, 1)
  ]

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
            False
          ),
          ( forAllTypes $ \tElem ->
              forAllDims $ \d ->
                validPropertyType tElem
                  .~~~> validPropertyType (tVector tElem d),
            lamType $ \tElem ->
              lamDim $ \_d ->
                instLam "r1" (validPropertyType tElem) $ \_inst ->
                  tUnit,
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
            irrelImplNatLam "n" $ \_n ->
              unitLit,
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
              instLam "r1" (validDatasetListElementType t) $ \_ ->
                tUnit,
            False
          ),
          ( forAllTypes $ \t ->
              forAllDim Irrelevant $ \d ->
                validDatasetListElementType t
                  .~~~> validDatasetType (tVector t d),
            implLam "t" type0 $ \t ->
              lam "d" (Implicit False) Irrelevant tDim $ \_d ->
                instLam "r1" (validDatasetListElementType t) $ \_ ->
                  tUnit,
            False
          ),
          ( forAllTypes $ \t ->
              forAllDims $ \ds ->
                validDatasetTensorElementType t
                  .~~~> validDatasetType (tTensor t ds),
            implLam "t" type0 $ \t ->
              lamDims $ \_ds ->
                instLam "r1" (validDatasetTensorElementType t) $ \_ ->
                  tUnit,
            False
          ),
          -- List element types
          ( forAllTypes $ \t ->
              validDatasetListElementType t
                .~~~> validDatasetListElementType (tList t),
            implLam "t" type0 $ \t ->
              instLam "r1" (validDatasetListElementType t) $ \_ ->
                tUnit,
            False
          ),
          ( forAllTypes $ \t ->
              forAllDim Irrelevant $ \d ->
                validDatasetListElementType t
                  .~~~> validDatasetListElementType (tVector t d),
            implLam "t" type0 $ \t ->
              lam "d" (Implicit False) Irrelevant tDim $ \_d ->
                instLam "r1" (validDatasetListElementType t) $ \_ ->
                  tUnit,
            False
          ),
          ( forAllTypes $ \t ->
              forAllDims $ \ds ->
                validDatasetTensorElementType t
                  .~~~> validDatasetListElementType (tTensor t ds),
            implLam "t" type0 $ \t ->
              lamDims $ \_ds ->
                instLam "r1" (validDatasetTensorElementType t) $ \_ ->
                  tUnit,
            False
          ),
          ( forAllIrrelevantNat "n" $ \n ->
              validDatasetListElementType (tIndex n),
            irrelImplNatLam "n" $ \_n ->
              tUnit,
            False
          ),
          ( validDatasetListElementType tNat,
            tUnit,
            False
          ),
          -- Element typs
          ( forAllIrrelevantNat "n" $ \n ->
              validDatasetTensorElementType (tIndex n),
            irrelImplNatLam "n" $ \_n ->
              tUnit,
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
              lamDims $ \_ds2 ->
                tUnit,
            False
          ),
          ----------------
          -- HasRatLits --
          ----------------
          ( hasRatLits (tRatTensor dimNil),
            builtinCast (FromRat FromRatToRat),
            False
          ),
          ----------------
          -- HasNatLits --
          ----------------
          ( forAllIrrelevantNat "n" $ \n ->
              hasNatLits (tIndex n),
            irrelImplNatLam "n" $ \n ->
              builtinCast (FromNat FromNatToIndex) .@@@ [n],
            False
          ),
          ( hasNatLits tNat,
            builtinCast (FromNat FromNatToNat),
            True
          ),
          ( hasNatLits (tRatTensor dimNil),
            builtinCast (FromNat FromNatToRat),
            False
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
            False
          ),
          ( forAllTypes $ \t ->
              forAllDim Irrelevant $ \d ->
                hasVecLits (tVector t d) t d,
            implLam "t" type0 $ \t ->
              lamDim $ \d ->
                builtinConstructor VectorLiteral @@@ [t, d],
            False
          ),
          ( forAllTypes $ \t ->
              forAllDim Irrelevant $ \d ->
                hasVecLits (tList t) t d,
            implLam "t" type0 $ \t ->
              lamDim $ \d ->
                builtinCast FromVectorToList @@@ [t, d],
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
            lamDims $ \dims -> builtinFunction (Neg NegRatTensor) .@@@ [dims],
            False
          ),
          ------------
          -- HasAdd --
          ------------
          ( hasAdd tNat tNat tNat,
            builtinFunction (Add AddNat),
            True
          ),
          ( forAllDims $ \dims -> hasAdd (tRatTensor dims) (tRatTensor dims) (tRatTensor dims),
            lamDims $ \dims -> builtinFunction (Add AddRatTensor) .@@@ [dims],
            False
          ),
          ------------
          -- HasSub --
          ------------
          ( forAllDims $ \dims -> hasSub (tRatTensor dims) (tRatTensor dims) (tRatTensor dims),
            lamDims $ \dims -> builtinFunction (Sub SubRatTensor) .@@@ [dims],
            False
          ),
          ------------
          -- HasMul --
          ------------
          ( hasMul tNat tNat tNat,
            builtinFunction (Mul MulNat),
            True
          ),
          ( forAllDims $ \dims -> hasMul (tRatTensor dims) (tRatTensor dims) (tRatTensor dims),
            lamDims $ \dims -> builtinFunction (Mul MulRatTensor) .@@@ [dims],
            False
          ),
          ------------
          -- HasDiv --
          ------------
          ( forAllDims $ \dims -> hasDiv (tRatTensor dims) (tRatTensor dims) (tRatTensor dims),
            lamDims $ \dims -> builtinFunction (Div DivRatTensor) .@@@ [dims],
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
      <> comparisonCandidates Le
      <> comparisonCandidates Lt
      <> comparisonCandidates Ge
      <> comparisonCandidates Gt
      <> comparisonCandidates Eq
      <> comparisonCandidates Ne
      <> quantifierCandidates Forall
      <> quantifierCandidates Exists
  where
    comparisonCandidates :: ComparisonOp -> [(DSLExpr Builtin, DSLExpr Builtin, Bool)]
    comparisonCandidates op =
      [ ( forAll "n1" tNat $ \n1 ->
            forAll "n2" tNat $ \n2 ->
              hasCompare op (tIndex n1) (tIndex n2) (tBoolTensor dimNil),
          implLam "n1" tNat $ \n1 ->
            implLam "n2" tNat $ \n2 ->
              builtinFunction (CompareIndex op) @@@ [n1, n2],
          False
        ),
        ( hasCompare op tNat tNat (tBoolTensor dimNil),
          builtinFunction (CompareNat op),
          True
        ),
        ( forAllDims $ \dims ->
            hasCompare op (tRatTensor dims) (tRatTensor dims) (tBoolTensor dimNil),
          lamDims $ \dims ->
            builtinDerivedFunction (CompareRatTensorReduced op) .@@@ [dims],
          False
        )
      ]

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
            builtinFunction (QuantifyRatTensor q) @@@ [ds],
          False
        )
      ]
