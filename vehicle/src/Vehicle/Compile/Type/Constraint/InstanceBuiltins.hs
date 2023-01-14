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

findTypeClassOfCandidate :: CheckedExpr -> Maybe TypeClass
findTypeClassOfCandidate = \case
  Pi _ binder body
    | not (isExplicit binder) -> findTypeClassOfCandidate body
  App _ (Builtin _ (Constructor (TypeClass tc))) _ -> Just tc
  _ -> Nothing

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
          ( forAllNat $ \n1 ->
              forAllNat $ \n2 ->
                hasEq Eq (tIndex n1) (tIndex n2) (tAnnBool constant unquantified),
            forAllNat $ \n1 ->
              forAllNat $ \n2 ->
                app
                  (builtin (Equals EqIndex Eq))
                  [ (Implicit True, Relevant, n1),
                    (Implicit True, Relevant, n2)
                  ]
          ),
          ( hasEq Eq tNat tNat (tAnnBool constant unquantified),
            builtin (Equals EqNat Eq)
          ),
          ( hasEq Eq tInt tInt (tAnnBool constant unquantified),
            builtin (Equals EqInt Eq)
          ),
          ( forAllLinearityTriples $ \l1 l2 l3 ->
              maxLinearity l1 l2 l3
                .~~~> hasEq Eq (tAnnRat l1) (tAnnRat l2) (tAnnBool l3 unquantified),
            forAllLinearityTriples $ \l1 l2 l3 ->
              maxLinearity l1 l2 l3
                .~~~> builtin (Equals EqRat Eq)
          ),
          ( forAllLinearityTriples $ \l1 l2 l3 ->
              forAllPolarityTriples $ \p1 p2 p3 ->
                maxLinearity l1 l2 l3
                  .~~~> eqPolarity Eq p1 p2 p3
                  .~~~> hasEq Eq (tAnnBool l1 p1) (tAnnBool l2 p2) (tAnnBool l3 p3),
            forAllLinearityTriples $ \l1 l2 l3 ->
              forAllPolarityTriples $ \p1 p2 p3 ->
                maxLinearity l1 l2 l3
                  .~~~> eqPolarity Eq p1 p2 p3
                  .~~~> free (identifierOf StdEqualsBool)
          ),
          ( forAllTypeTriples $ \t1 t2 t3 ->
              forAllNat $ \n ->
                hasEq Eq t1 t2 t3
                  ~~~> hasEq Eq (tVector t1 n) (tVector t2 n) t3,
            forAllTypeTriples $ \t1 t2 t3 ->
              forAllNat $ \n ->
                forAllInstance "eq" (hasEq Eq t1 t2 t3) $ \eq ->
                  app
                    (free (identifierOf StdEqualsVector))
                    [ (Implicit True, Relevant, t1),
                      (Implicit True, Relevant, t2),
                      (Implicit True, Relevant, t3),
                      (Implicit True, Relevant, n),
                      (Instance True, Relevant, eq)
                    ]
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
  Nothing -> developerError $ "Invalid builtin instance candidate" <+> prettyVerbose (candidateExpr candidate)
  Just tc -> (tc, candidate)
