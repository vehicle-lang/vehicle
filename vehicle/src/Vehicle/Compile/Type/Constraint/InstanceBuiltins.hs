{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
module Vehicle.Compile.Type.Constraint.InstanceBuiltins
  ( hasAddCandidates,
    hasMapCandidates,
  )
where

import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint (InstanceCandidate (..))
import Vehicle.Expr.DSL
import Vehicle.Libraries.StandardLibrary.Names (StdLibFunction (..))

--------------------------------------------------------------------------------
-- Builtin instances

-- Manually declared here as we have no way of declaring them in the language
-- itself.

-- Also note that annoyingly because of a lack of first class records we have
-- to duplicate the context for both the candidate and the candidate's solution.

hasAddCandidates :: [InstanceCandidate]
hasAddCandidates =
  mkCandidate
    <$> [ ( hasAdd tNat tNat tNat,
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
          ( forAllTypeTriples $ \t1 t2 t3 -> forAll "n" tNat $ \n ->
              hasAdd t1 t2 t3
                ~~~> hasAdd (tVector t1 n) (tVector t2 n) (tVector t3 n),
            forAllTypeTriples $ \t1 t2 t3 -> forAll "n" tNat $ \n -> forAllInstance "add" (hasAdd t1 t2 t3) $ \add ->
              app
                (free (identifierOf StdAddVector))
                [ (Implicit True, Relevant, t1),
                  (Implicit True, Relevant, t2),
                  (Implicit True, Relevant, t3),
                  (Implicit True, Relevant, n),
                  (Instance True, Relevant, add)
                ]
          )
        ]

hasMapCandidates :: [InstanceCandidate]
hasMapCandidates =
  mkCandidate
    <$> [ ( hasMap tListRaw,
            builtin (Map MapList)
          ),
          ( forAll "n" tNat $ \n -> hasMap (lam "A" Explicit Relevant type0 (\a -> tVector a n)),
            forAll "n" tNat $ \n -> app (builtin (Map MapVector)) [(Implicit True, Relevant, n)]
          )
        ]

--------------------------------------------------------------------------------
-- Helper functions

mkCandidate :: (DSLExpr, DSLExpr) -> InstanceCandidate
mkCandidate (expr, solution) = do
  let expr' = fromDSL mempty expr
  let solution' = fromDSL mempty solution
  InstanceCandidate mempty expr' solution'
