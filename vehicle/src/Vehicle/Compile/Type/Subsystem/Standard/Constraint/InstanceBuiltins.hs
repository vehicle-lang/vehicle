{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Compile.Type.Subsystem.Standard.Constraint.InstanceBuiltins
  ( standardBuiltinInstances,
  )
where

import Data.Bifunctor (Bifunctor (..))
import Data.HashMap.Strict qualified as Map
import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Core (InstanceCandidateDatabase)
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Compile.Type.Subsystem.Standard.Interface
import Vehicle.Expr.DSL hiding (builtin)
import Vehicle.Libraries.StandardLibrary

standardBuiltinInstances :: InstanceCandidateDatabase StandardBuiltin
standardBuiltinInstances = do
  let tcAndCandidates = fmap (second (: []) . extractHeadFromInstanceCandidate) candidates
  Map.fromListWith (<>) tcAndCandidates

--------------------------------------------------------------------------------
-- Builtin instances

-- Manually declared here as we have no way of declaring them in the language
-- itself.

-- Also note that annoyingly because of a lack of first class records we have
-- to duplicate the context for both the candidate and the candidate's solution.

candidates :: [StandardInstanceCandidate]
candidates =
  mkCandidate
    <$> [
          ----------------
          -- HasRatLits --
          ----------------
          ( hasRatLits tRat,
            builtin (FromRat FromRatToRat)
          ),
          ----------------
          -- HasNatLits --
          ----------------
          ( forAllIrrelevantNat "n" $ \n ->
              hasNatLits (tIndex n),
            irrelImplNatLam "n" $ \n ->
              builtin (FromNat FromNatToIndex) .@@@ [n]
          ),
          ( hasNatLits tNat,
            builtin (FromNat FromNatToNat)
          ),
          ( hasNatLits tInt,
            builtin (FromNat FromNatToInt)
          ),
          ( hasNatLits tRat,
            builtin (FromNat FromNatToRat)
          ),
          ----------------
          -- HasVecLits --
          ----------------
          ( forAllIrrelevantNat "n" $ \n ->
              hasVecLits n (tVectorFunctor n),
            irrelImplNatLam "n" $ \n ->
              free StdVectorToVector .@@@ [n]
          ),
          ( forAllIrrelevantNat "n" $ \n ->
              hasVecLits n tListRaw,
            irrelImplNatLam "n" $ \n ->
              free StdVectorToList .@@@ [n]
          ),
          ------------
          -- HasNeg --
          ------------
          ( hasNeg tInt tInt,
            builtin (Neg NegInt)
          ),
          ( hasNeg tRat tRat,
            builtin (Neg NegRat)
          ),
          ------------
          -- HasAdd --
          ------------
          ( hasAdd tNat tNat tNat,
            builtin (Add AddNat)
          ),
          ( hasAdd tInt tInt tInt,
            builtin (Add AddInt)
          ),
          ( hasAdd tRat tRat tRat,
            builtin (Add AddRat)
          ),
          ( forAllTypeTriples $ \t1 t2 t3 ->
              forAllIrrelevantNat "n" $ \n ->
                hasAdd t1 t2 t3
                  ~~~> hasAdd (tVector t1 n) (tVector t2 n) (tVector t3 n),
            implTypeTripleLam $ \t1 t2 t3 ->
              irrelImplNatLam "n" $ \n ->
                instLam "add" (hasAdd t1 t2 t3) $ \add ->
                  free StdAddVector @@@ [t1, t2, t3] .@@@ [n] @@@@ [add]
          ),
          ------------
          -- HasSub --
          ------------
          ( hasSub tInt tInt tInt,
            builtin (Sub SubInt)
          ),
          ( hasSub tRat tRat tRat,
            builtin (Sub SubRat)
          ),
          ( forAllTypeTriples $ \t1 t2 t3 ->
              forAllIrrelevantNat "n" $ \n ->
                hasSub t1 t2 t3
                  ~~~> hasSub (tVector t1 n) (tVector t2 n) (tVector t3 n),
            implTypeTripleLam $ \t1 t2 t3 ->
              irrelImplNatLam "n" $ \n ->
                instLam "sub" (hasSub t1 t2 t3) $ \sub ->
                  free StdSubVector @@@ [t1, t2, t3] .@@@ [n] @@@@ [sub]
          ),
          ------------
          -- HasMul --
          ------------
          ( hasMul tNat tNat tNat,
            builtin (Mul MulNat)
          ),
          ( hasMul tInt tInt tInt,
            builtin (Mul MulInt)
          ),
          ( hasMul tRat tRat tRat,
            builtin (Mul MulRat)
          ),
          ------------
          -- HasDiv --
          ------------
          ( hasDiv tRat tRat tRat,
            builtin (Div DivRat)
          ),
          ------------
          -- HasMap --
          ------------
          ( hasMap tListRaw,
            builtin MapList
          ),
          ( forAllIrrelevantNat "n" $ \n -> hasMap (tVectorFunctor n),
            irrelImplNatLam "n" $ \n -> builtin MapVector .@@@ [n]
          ),
          ------------
          -- HasFold --
          ------------
          ( hasFold tListRaw,
            builtin (Fold FoldList)
          ),
          ( forAllIrrelevantNat "n" $ \n -> hasFold (tVectorFunctor n),
            irrelImplNatLam "n" $ \n -> builtin (Fold FoldVector) .@@@ [n]
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
    orderCandidates :: OrderOp -> [(StandardDSLExpr, StandardDSLExpr)]
    orderCandidates op =
      [ ( forAll "n1" tNat $ \n1 ->
            forAll "n2" tNat $ \n2 ->
              hasOrd op (tIndex n1) (tIndex n2),
          implLam "n1" tNat $ \n1 ->
            implLam "n2" tNat $ \n2 ->
              builtin (Order OrderIndex op) @@@ [n1, n2]
        ),
        ( hasOrd op tNat tNat,
          builtin (Order OrderNat op)
        ),
        ( hasOrd op tInt tInt,
          builtin (Order OrderInt op)
        ),
        ( hasOrd op tRat tRat,
          builtin (Order OrderRat op)
        )
      ]

    eqCandidates :: EqualityOp -> StdLibFunction -> [(StandardDSLExpr, StandardDSLExpr)]
    eqCandidates op vectorOp =
      [ ( forAll "n1" tNat $ \n1 ->
            forAll "n2" tNat $ \n2 ->
              hasEq op (tIndex n1) (tIndex n2),
          implLam "n1" tNat $ \n1 ->
            implLam "n2" tNat $ \n2 ->
              builtin (Equals EqIndex op) @@@ [n1, n2]
        ),
        ( hasEq op tNat tNat,
          builtin (Equals EqNat op)
        ),
        ( hasEq op tInt tInt,
          builtin (Equals EqInt op)
        ),
        ( hasEq op tRat tRat,
          builtin (Equals EqRat op)
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
                  free vectorOp @@@ [t1, t2] .@@@ [n] @@@@ [eq]
        )
      ]

    quantifierCandidates ::
      Quantifier ->
      StdLibFunction ->
      [(StandardDSLExpr, StandardDSLExpr)]
    quantifierCandidates q indexOp =
      [ ( hasQuantifier q tRat,
          builtin (Quantifier q)
        ),
        ( forAllNat $ \n ->
            hasQuantifier q (tIndex n),
          implLam "n" tNat $ \n ->
            free indexOp @@ [n]
        ),
        ( forAllTypes $ \t ->
            forAllNat $ \n ->
              hasQuantifier q t
                ~~~> hasQuantifier q (tVector t n),
          implLam "t1" type0 $ \t ->
            irrelImplNatLam "n" $ \n ->
              instLam "quant" (hasQuantifier q t) $ \quant ->
                builtin (Quantifier q) @@@ [t, n] @@@@ [quant]
        )
      ]

builtin :: BuiltinFunction -> StandardDSLExpr
builtin = builtinFunction
