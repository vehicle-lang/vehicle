{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use id" #-}

module Vehicle.Compile.Type.Constraint.InstanceBuiltins
  ( standardBuiltinInstances,
  )
where

import Data.Bifunctor (Bifunctor (..))
import Data.HashMap.Strict qualified as Map
import Vehicle.Compile.Prelude (developerError)
import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Core (InstanceCandidate (..), InstanceDatabase (..))
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.DSL
import Vehicle.Data.DSL hiding (builtin)
import Vehicle.Libraries.StandardLibrary.Definitions

standardBuiltinInstances :: InstanceDatabase Builtin
standardBuiltinInstances = do
  let tcAndCandidates = fmap (second (: []) . extractHeadFromInstanceCandidate) allInstances
  let instances = Map.fromListWith (<>) tcAndCandidates
  let defaults = Map.mapMaybe findDefault instances
  InstanceDatabase instances defaults

--------------------------------------------------------------------------------
-- Builtin instances

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
          ( validPropertyType tBool,
            unitLit,
            False
          ),
          ( forAllTypes $ \t ->
              forAllIrrelevantNat "n" $ \n ->
                validPropertyType t
                  .~~~> validPropertyType (tVector t n),
            implLam "t" type0 $ \t1 ->
              irrelImplNatLam "n" $ \_n ->
                instLam "t" (validPropertyType t1) $ \_add ->
                  tUnit,
            False
          ),
          ------------------------------------
          -- ValidNonInferableParameterType --
          ------------------------------------
          ( validNonInferableParameterType tBool,
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
          ( validNonInferableParameterType tRat,
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
              validDatasetElementType t
                .~~~> validDatasetType (tList t),
            implLam "t" type0 $ \t ->
              instLam "r1" (validDatasetElementType t) $ \_ ->
                tUnit,
            False
          ),
          ( forAllTypes $ \t ->
              forAllIrrelevantNat "n" $ \n ->
                validDatasetElementType t
                  .~~~> validDatasetType (tVector t n),
            implLam "t" type0 $ \t ->
              irrelImplNatLam "n" $ \_n ->
                instLam "r1" (validDatasetElementType t) $ \_ ->
                  tUnit,
            False
          ),
          ( forAllTypes $ \t ->
              validDatasetElementType t
                .~~~> validDatasetElementType (tList t),
            implLam "t" type0 $ \t ->
              instLam "r1" (validDatasetElementType t) $ \_ ->
                tUnit,
            False
          ),
          ( forAllTypes $ \t ->
              forAllIrrelevantNat "n" $ \n ->
                validDatasetElementType t
                  .~~~> validDatasetElementType (tVector t n),
            implLam "t" type0 $ \t ->
              irrelImplNatLam "n" $ \_n ->
                instLam "r1" (validDatasetElementType t) $ \_ ->
                  tUnit,
            False
          ),
          ( validDatasetElementType tBool,
            tUnit,
            False
          ),
          ( forAllIrrelevantNat "n" $ \n ->
              validDatasetElementType (tIndex n),
            irrelImplNatLam "n" $ \_n ->
              tUnit,
            False
          ),
          ( validDatasetElementType tNat,
            tUnit,
            False
          ),
          ( validDatasetElementType tRat,
            tUnit,
            False
          ),
          ----------------------
          -- ValidNetworkType --
          ----------------------
          ( forAllTypePairs $ \t1 t2 ->
              validNetworkTensorType t1
                .~~~> validNetworkTensorType t2
                .~~~> validNetworkType (t1 ~> t2),
            implTypeDoubleLam $ \t1 t2 ->
              instLam "r1" (validNetworkTensorType t1) $ \_ ->
                instLam "r2" (validNetworkTensorType t2) $ \_ ->
                  tUnit,
            False
          ),
          ( forAllTypes $ \t ->
              forAllIrrelevantNat "n1" $ \n1 ->
                forAllIrrelevantNat "n2" $ \n2 ->
                  validNetworkTensorType (tVector t n1)
                    .~~~> validNetworkTensorType (tVector (tVector t n1) n2),
            implLam "t" type0 $ \t ->
              irrelImplNatLam "n1" $ \n1 ->
                irrelImplNatLam "n2" $ \_n2 ->
                  instLam "r1" (validNetworkTensorType (tVector t n1)) $ \_ ->
                    tUnit,
            False
          ),
          ( forAllIrrelevantNat "n" $ \n ->
              validNetworkTensorType (tVector tRat n),
            irrelImplNatLam "n" $ \_n ->
              tUnit,
            False
          ),
          ----------------
          -- HasRatLits --
          ----------------
          ( hasRatLits tRat,
            builtin (FromRat FromRatToRat),
            False
          ),
          ----------------
          -- HasNatLits --
          ----------------
          ( forAllIrrelevantNat "n" $ \n ->
              hasNatLits (tIndex n),
            irrelImplNatLam "n" $ \n ->
              builtin (FromNat FromNatToIndex) .@@@ [n],
            False
          ),
          ( hasNatLits tNat,
            builtin (FromNat FromNatToNat),
            True
          ),
          ( hasNatLits tRat,
            builtin (FromNat FromNatToRat),
            False
          ),
          ----------------
          -- HasVecLits --
          ----------------
          ( forAllIrrelevantNat "n" $ \n ->
              hasVecLits n (tVectorFunctor n),
            irrelImplNatLam "n" $ \n ->
              free StdVectorToVector .@@@ [n],
            False
          ),
          ( forAllIrrelevantNat "n" $ \n ->
              hasVecLits n tListRaw,
            irrelImplNatLam "n" $ \n ->
              free StdVectorToList .@@@ [n],
            True
          ),
          ------------
          -- HasNeg --
          ------------
          ( hasNeg tRat tRat,
            builtin (Neg NegRat),
            False
          ),
          ------------
          -- HasAdd --
          ------------
          ( hasAdd tNat tNat tNat,
            builtin (Add AddNat),
            True
          ),
          ( hasAdd tRat tRat tRat,
            builtin (Add AddRat),
            False
          ),
          ( forAllTypeTriples $ \t1 t2 t3 ->
              forAllIrrelevantNat "n" $ \n ->
                hasAdd t1 t2 t3
                  ~~~> hasAdd (tVector t1 n) (tVector t2 n) (tVector t3 n),
            implTypeTripleLam $ \t1 t2 t3 ->
              irrelImplNatLam "n" $ \n ->
                instLam "add" (hasAdd t1 t2 t3) $ \add ->
                  free StdAddVector @@@ [t1, t2, t3] .@@@ [n] @@@@ [add],
            False
          ),
          ------------
          -- HasSub --
          ------------
          ( hasSub tRat tRat tRat,
            builtin (Sub SubRat),
            False
          ),
          ( forAllTypeTriples $ \t1 t2 t3 ->
              forAllIrrelevantNat "n" $ \n ->
                hasSub t1 t2 t3
                  ~~~> hasSub (tVector t1 n) (tVector t2 n) (tVector t3 n),
            implTypeTripleLam $ \t1 t2 t3 ->
              irrelImplNatLam "n" $ \n ->
                instLam "sub" (hasSub t1 t2 t3) $ \sub ->
                  free StdSubVector @@@ [t1, t2, t3] .@@@ [n] @@@@ [sub],
            False
          ),
          ------------
          -- HasMul --
          ------------
          ( hasMul tNat tNat tNat,
            builtin (Mul MulNat),
            False
          ),
          ( hasMul tRat tRat tRat,
            builtin (Mul MulRat),
            False
          ),
          ------------
          -- HasDiv --
          ------------
          ( hasDiv tRat tRat tRat,
            builtin (Div DivRat),
            False
          ),
          ------------
          -- HasMap --
          ------------
          ( hasMap tListRaw,
            builtin MapList,
            True
          ),
          ( forAllIrrelevantNat "n" $ \n -> hasMap (tVectorFunctor n),
            irrelImplNatLam "n" $ \n -> builtin MapVector .@@@ [n],
            False
          ),
          ------------
          -- HasFold --
          ------------
          ( hasFold tListRaw,
            builtin FoldList,
            True
          ),
          ( forAllIrrelevantNat "n" $ \n -> hasFold (tVectorFunctor n),
            irrelImplNatLam "n" $ \n -> builtin FoldVector .@@@ [n],
            False
          )
        ]
      <> orderCandidates Le
      <> orderCandidates Lt
      <> orderCandidates Ge
      <> orderCandidates Gt
      <> eqCandidates Eq StdEqualsVector
      <> eqCandidates Neq StdNotEqualsVector
      <> quantifierCandidates Forall StdForallIndex
      <> quantifierCandidates Exists StdExistsIndex
  where
    orderCandidates :: OrderOp -> [(StandardDSLExpr, StandardDSLExpr, Bool)]
    orderCandidates op =
      [ ( forAll "n1" tNat $ \n1 ->
            forAll "n2" tNat $ \n2 ->
              hasOrd op (tIndex n1) (tIndex n2),
          implLam "n1" tNat $ \n1 ->
            implLam "n2" tNat $ \n2 ->
              builtin (Order OrderIndex op) @@@ [n1, n2],
          False
        ),
        ( hasOrd op tNat tNat,
          builtin (Order OrderNat op),
          True
        ),
        ( hasOrd op tRat tRat,
          builtin (Order OrderRat op),
          False
        )
      ]

    eqCandidates :: EqualityOp -> StdLibFunction -> [(StandardDSLExpr, StandardDSLExpr, Bool)]
    eqCandidates op vectorOp =
      [ ( forAll "n1" tNat $ \n1 ->
            forAll "n2" tNat $ \n2 ->
              hasEq op (tIndex n1) (tIndex n2),
          implLam "n1" tNat $ \n1 ->
            implLam "n2" tNat $ \n2 ->
              builtin (Equals EqIndex op) @@@ [n1, n2],
          False
        ),
        ( hasEq op tNat tNat,
          builtin (Equals EqNat op),
          True
        ),
        ( hasEq op tRat tRat,
          builtin (Equals EqRat op),
          False
        ),
        ( forAll "t1" type0 $ \t1 ->
            forAll "t2" type0 $ \t2 ->
              forAllIrrelevantNat "n" $ \n ->
                hasEq op t1 t2
                  ~~~> hasEq op (tVector t1 n) (tVector t2 n),
          implLam "t1" type0 $ \t1 ->
            implLam "t2" type0 $ \t2 ->
              irrelImplNatLam "n" $ \n ->
                instLam "eq" (hasEq op t1 t2) $ \eq ->
                  free vectorOp @@@ [t1, t2] .@@@ [n] @@@@ [eq],
          False
        )
      ]

    quantifierCandidates ::
      Quantifier ->
      StdLibFunction ->
      [(StandardDSLExpr, StandardDSLExpr, Bool)]
    quantifierCandidates q indexOp =
      [ ( hasQuantifier q tRat,
          builtin (Quantifier q),
          False
        ),
        ( forAllNat $ \n ->
            hasQuantifier q (tIndex n),
          implLam "n" tNat $ \n ->
            free indexOp @@ [n],
          False
        ),
        ( forAllTypes $ \t ->
            forAllNat $ \n ->
              hasQuantifier q t
                ~~~> hasQuantifier q (tVector t n),
          implLam "t1" type0 $ \t ->
            irrelImplNatLam "n" $ \_n ->
              instLam "quant" (hasQuantifier q t) $ \quant -> quant,
          -- THIS IS A BUG (see #837)
          False
        )
      ]

type StandardDSLExpr = DSLExpr Builtin

builtin :: BuiltinFunction -> StandardDSLExpr
builtin = builtinFunction

findDefault :: [InstanceCandidate builtin] -> Maybe (InstanceCandidate builtin)
findDefault instances = do
  let defaultInstances = filter defaultInstance instances
  case defaultInstances of
    [] -> Nothing
    [inst] -> Just inst
    _ -> developerError "Multiple default instances found"
