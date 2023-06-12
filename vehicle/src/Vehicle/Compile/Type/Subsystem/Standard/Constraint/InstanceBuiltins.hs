{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
module Vehicle.Compile.Type.Subsystem.Standard.Constraint.InstanceBuiltins
  ( builtinInstances,
    findTypeClassOfCandidate,
  )
where

import Data.Bifunctor (Bifunctor (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Expr.DSL hiding (builtin)
import Vehicle.Expr.Normalisable
import Vehicle.Libraries.StandardLibrary

builtinInstances :: HashMap TypeClass [Provenance -> InstanceCandidate]
builtinInstances = do
  let tcAndCandidates = fmap (second (: []) . processCandidate) candidates
  HashMap.fromListWith (<>) tcAndCandidates

findTypeClassOfCandidate :: StandardExpr -> Either StandardExpr TypeClass
findTypeClassOfCandidate = \case
  Pi _ binder body
    | not (isExplicit binder) -> findTypeClassOfCandidate body
  App _ (Builtin _ (CType (StandardTypeClass tc))) _ -> Right tc
  expr -> Left expr

--------------------------------------------------------------------------------
-- Builtin instances

-- Manually declared here as we have no way of declaring them in the language
-- itself.

-- Also note that annoyingly because of a lack of first class records we have
-- to duplicate the context for both the candidate and the candidate's solution.

candidates :: [Provenance -> InstanceCandidate]
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
          ( forAllNat $ \n ->
              hasNatLits (tIndex n),
            implLam "n" tNat $ \n ->
              builtin (FromNat FromNatToIndex) @@@ [n]
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
          ( forAll "n" tNat $ \n ->
              hasVecLits n (tVectorFunctor n),
            implLam "n" tNat $ \n ->
              free StdVectorToVector @@@ [n]
          ),
          ( forAll "n" tNat $ \n ->
              hasVecLits n tListRaw,
            implLam "n" tNat $ \n ->
              free StdVectorToList @@@ [n]
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
              forAllNat $ \n ->
                hasAdd t1 t2 t3
                  ~~~> hasAdd (tVector t1 n) (tVector t2 n) (tVector t3 n),
            implTypeTripleLam $ \t1 t2 t3 ->
              implLam "n" tNat $ \n ->
                instLam "add" (hasAdd t1 t2 t3) $ \add ->
                  free StdAddVector @@@ [t1, t2, t3, n] @@@@ [add]
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
              forAllNat $ \n ->
                hasSub t1 t2 t3
                  ~~~> hasSub (tVector t1 n) (tVector t2 n) (tVector t3 n),
            implTypeTripleLam $ \t1 t2 t3 ->
              implLam "n" tNat $ \n ->
                instLam "sub" (hasSub t1 t2 t3) $ \sub ->
                  free StdSubVector @@@ [t1, t2, t3, n] @@@@ [sub]
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
          ( hasDiv tNat tNat tRat,
            explLam "x" tNat $ \x ->
              explLam "y" tNat $ \y ->
                builtin (Div DivRat)
                  @@ [ builtin (FromNat FromNatToRat) @@ [x] .@@@@ [unitLit],
                       builtin (FromNat FromNatToRat) @@ [y] .@@@@ [unitLit]
                     ]
          ),
          ( hasDiv tRat tRat tRat,
            builtin (Div DivRat)
          ),
          ------------
          -- HasMap --
          ------------
          ( hasMap tListRaw,
            free StdMapList
          ),
          ( forAllNat $ \n -> hasMap (tVectorFunctor n),
            implLam "n" tNat $ \n -> free StdMapVector @@@ [n]
          ),
          ------------
          -- HasFold --
          ------------
          ( hasFold tListRaw,
            builtin (Fold FoldList)
          ),
          ( forAllNat $ \n -> hasFold (tVectorFunctor n),
            implLam "n" tNat $ \n ->
              implLam "A" type0 $ \a ->
                implLam "B" type0 $ \b ->
                  explLam "f" (a ~> b ~> b) $ \f ->
                    builtin (Fold FoldVector) @@@ [n, a, b] @@ [implLam "m" tNat (const f)]
          )
        ]
      <> orderCandidates Le
      <> orderCandidates Lt
      <> orderCandidates Ge
      <> orderCandidates Gt
      <> eqCandidates Eq StdEqualsVector
      <> eqCandidates Neq StdNotEqualsVector
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
              forAllNat $ \n ->
                hasEq op t1 t2
                  ~~~> hasEq op (tVector t1 n) (tVector t2 n),
          implLam "t1" type0 $ \t1 ->
            implLam "t2" type0 $ \t2 ->
              implLam "n" tNat $ \n ->
                instLam "eq" (hasEq op t1 t2) $ \eq ->
                  free vectorOp @@@ [t1, t2] @@@ [n] @@@@ [eq]
        )
      ]

mkCandidate :: (StandardDSLExpr, StandardDSLExpr) -> Provenance -> InstanceCandidate
mkCandidate (expr, solution) p = do
  let expr' = fromDSL p expr
  let solution' = fromDSL p solution
  InstanceCandidate expr' solution'

processCandidate :: (Provenance -> InstanceCandidate) -> (TypeClass, Provenance -> InstanceCandidate)
processCandidate candidate = do
  let expr = candidateExpr (candidate mempty)
  case findTypeClassOfCandidate expr of
    Right tc -> (tc, candidate)
    Left subexpr -> do
      let candidateDoc = prettyVerbose subexpr
      let problemDoc = prettyVerbose subexpr
      developerError $
        "Invalid builtin instance candidate:"
          <+> candidateDoc
          <> line
          <> "Problematic subexpr:"
            <+> problemDoc

builtin :: BuiltinFunction -> StandardDSLExpr
builtin = builtinFunction
