{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
module Vehicle.Compile.Type.Constraint.InstanceBuiltins
  ( declaredCandidates,
    findTypeClassOfCandidate,
  )
where

import Data.Bifunctor (Bifunctor (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Type.Constraint (InstanceCandidate (..))
import Vehicle.Expr.DSL
import Vehicle.Libraries.StandardLibrary.Names (StdLibFunction (..))

declaredCandidates :: HashMap TypeClass [InstanceCandidate]
declaredCandidates = do
  let tcAndCandidates = fmap (second (: []) . processCandidate) candidates
  HashMap.fromListWith (<>) tcAndCandidates

findTypeClassOfCandidate :: CheckedExpr -> Either CheckedExpr TypeClass
findTypeClassOfCandidate = \case
  Pi _ binder body
    | not (isExplicit binder) -> findTypeClassOfCandidate body
  App _ (Builtin _ (Constructor (TypeClass tc))) _ -> Right tc
  expr -> Left expr

--------------------------------------------------------------------------------
-- Builtin instances

-- Manually declared here as we have no way of declaring them in the language
-- itself.

-- Also note that annoyingly because of a lack of first class records we have
-- to duplicate the context for both the candidate and the candidate's solution.

candidates :: [InstanceCandidate]
candidates =
  mkCandidate
    <$> [
          ----------------
          -- HasRatLits --
          ----------------
          ( hasRatLits (tAnnRat constant),
            builtin (FromRat FromRatToRat)
          ),
          ------------
          -- HasEq --
          ------------
          ( forAll "n1" tNat $ \n1 ->
              forAll "n2" tNat $ \n2 ->
                hasEq Eq constant (tIndex n1) (tIndex n2),
            forAll "n1" tNat $ \n1 ->
              forAll "n2" tNat $ \n2 ->
                builtin (Equals EqIndex Eq) @@@ [n1, n2]
          ),
          ( hasEq Eq constant tNat tNat,
            builtin (Equals EqNat Eq)
          ),
          ( hasEq Eq constant tInt tInt,
            builtin (Equals EqInt Eq)
          ),
          ( forAllLinearityTriples $ \l1 l2 l3 ->
              maxLinearity l1 l2 l3
                .~~~> hasEq Eq l3 (tAnnRat l1) (tAnnRat l2),
            forAllLinearityTriples $ \l1 l2 l3 ->
              maxLinearity l1 l2 l3
                .~~~> builtin (Equals EqRat Eq)
          ),
          ( forAll "t1" type0 $ \t1 ->
              forAll "t2" type0 $ \t2 ->
                forAll "l" tLin $ \l ->
                  forAllNat $ \n ->
                    hasEq Eq l t1 t2
                      ~~~> hasEq Eq l (tVector t1 n) (tVector t2 n),
            forAll "t1" type0 $ \t1 ->
              forAll "t2" type0 $ \t2 ->
                forAll "l" tLin $ \l ->
                  forAllNat $ \n ->
                    forAllInstance "eq" (hasEq Eq l t1 t2) $ \eq ->
                      free (identifierOf StdEqualsVector) @@@ [t1, t2] .@@@ [l] @@@ [n] @@@@ [eq]
          ),
          ------------
          -- HasNotEq --
          ------------
          ( forAll "n1" tNat $ \n1 ->
              forAll "n2" tNat $ \n2 ->
                hasEq Neq constant (tIndex n1) (tIndex n2),
            forAll "n1" tNat $ \n1 ->
              forAll "n2" tNat $ \n2 ->
                builtin (Equals EqIndex Neq) @@@ [n1, n2]
          ),
          ( hasEq Neq constant tNat tNat,
            builtin (Equals EqNat Neq)
          ),
          ( hasEq Neq constant tInt tInt,
            builtin (Equals EqInt Neq)
          ),
          ( forAllLinearityTriples $ \l1 l2 l3 ->
              maxLinearity l1 l2 l3
                .~~~> hasEq Neq l3 (tAnnRat l1) (tAnnRat l2),
            forAllLinearityTriples $ \l1 l2 l3 ->
              maxLinearity l1 l2 l3
                .~~~> builtin (Equals EqRat Neq)
          ),
          ( forAll "t1" type0 $ \t1 ->
              forAll "t2" type0 $ \t2 ->
                forAll "l" tLin $ \l ->
                  forAllNat $ \n ->
                    hasEq Neq l t1 t2
                      ~~~> hasEq Neq l (tVector t1 n) (tVector t2 n),
            forAll "t1" type0 $ \t1 ->
              forAll "t2" type0 $ \t2 ->
                forAll "l" tLin $ \l ->
                  forAllNat $ \n ->
                    forAllInstance "eq" (hasEq Neq l t1 t2) $ \eq ->
                      free (identifierOf StdNotEqualsVector) .@@@ [l] @@@ [t1, t2, n] @@@@ [eq]
          ),
          ------------
          -- HasNot --
          ------------
          ( forAllIrrelevant "l" tLin $ \l ->
              forAllIrrelevant "p1" tPol $ \p1 ->
                forAllIrrelevant "p2" tPol $ \p2 ->
                  negPolarity p1 p2
                    .~~~> hasNot (tAnnBool l p1) (tAnnBool l p2),
            forAllIrrelevant "l" tLin $ \_l ->
              forAllIrrelevant "p1" tPol $ \p1 ->
                forAllIrrelevant "p2" tPol $ \p2 ->
                  negPolarity p1 p2
                    .~~~> builtin Not
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
          ( forAllLinearityTriples $ \l1 l2 l3 ->
              maxLinearity l1 l2 l3
                .~~~> hasAdd (tAnnRat l1) (tAnnRat l2) (tAnnRat l3),
            forAllLinearityTriples $ \l1 l2 l3 ->
              maxLinearity l1 l2 l3
                .~~~> builtin (Add AddRat)
          ),
          ( forAllTypeTriples $ \t1 t2 t3 -> forAllNat $ \n ->
              hasAdd t1 t2 t3
                ~~~> hasAdd (tVector t1 n) (tVector t2 n) (tVector t3 n),
            forAllTypeTriples $ \t1 t2 t3 -> forAllNat $ \n ->
              forAllInstance "add" (hasAdd t1 t2 t3) $ \add ->
                app
                  (free (identifierOf StdAddVector))
                  [ (Implicit True, Relevant, t1),
                    (Implicit True, Relevant, t2),
                    (Implicit True, Relevant, t3),
                    (Implicit True, Relevant, n),
                    (Instance True, Relevant, add)
                  ]
          ),
          ------------
          -- HasSub --
          ------------
          ( hasSub tInt tInt tInt,
            builtin (Sub SubInt)
          ),
          ( forAllLinearityTriples $ \l1 l2 l3 ->
              maxLinearity l1 l2 l3
                .~~~> hasSub (tAnnRat l1) (tAnnRat l2) (tAnnRat l3),
            forAllLinearityTriples $ \l1 l2 l3 ->
              maxLinearity l1 l2 l3
                .~~~> builtin (Sub SubRat)
          ),
          ( forAllTypeTriples $ \t1 t2 t3 -> forAllNat $ \n ->
              hasSub t1 t2 t3
                ~~~> hasSub (tVector t1 n) (tVector t2 n) (tVector t3 n),
            forAllTypeTriples $ \t1 t2 t3 -> forAllNat $ \n ->
              forAllInstance "sub" (hasSub t1 t2 t3) $ \sub ->
                app
                  (free (identifierOf StdSubVector))
                  [ (Implicit True, Relevant, t1),
                    (Implicit True, Relevant, t2),
                    (Implicit True, Relevant, t3),
                    (Implicit True, Relevant, n),
                    (Instance True, Relevant, sub)
                  ]
          ),
          ------------
          -- HasMap --
          ------------
          ( hasMap tListRaw,
            builtin (Map MapList)
          ),
          ( forAllNat $ \n -> hasMap (lam "A" Explicit Relevant type0 (\a -> tVector a n)),
            forAllNat $ \n -> app (builtin (Map MapVector)) [(Implicit True, Relevant, n)]
          ),
          ------------
          -- HasFold --
          ------------
          ( hasFold tListRaw,
            builtin (Fold FoldList)
          ),
          ( forAllNat $ \n -> hasFold (lam "A" Explicit Relevant type0 (\a -> tVector a n)),
            forAllNat $ \n -> app (builtin (Fold FoldVector)) [(Implicit True, Relevant, n)]
          )
        ]

mkCandidate :: (DSLExpr, DSLExpr) -> InstanceCandidate
mkCandidate (expr, solution) = do
  let expr' = fromDSL mempty expr
  let solution' = fromDSL mempty solution
  InstanceCandidate mempty expr' solution'

processCandidate :: InstanceCandidate -> (TypeClass, InstanceCandidate)
processCandidate candidate = case findTypeClassOfCandidate (candidateExpr candidate) of
  Right tc -> (tc, candidate)
  Left expr -> do
    let candidateDoc = prettyVerbose (candidateExpr candidate)
    let problemDoc = prettyVerbose expr
    developerError $
      "Invalid builtin instance candidate:"
        <+> candidateDoc
          <> line
          <> "Problematic expr:"
        <+> problemDoc
