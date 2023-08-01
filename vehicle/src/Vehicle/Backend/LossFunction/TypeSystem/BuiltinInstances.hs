module Vehicle.Backend.LossFunction.TypeSystem.BuiltinInstances
  ( lossBuiltinInstances,
  )
where

import Data.Bifunctor (Bifunctor (..))
import Data.HashMap.Strict qualified as Map
import Vehicle.Backend.LossFunction.Logics (DifferentialLogicImplementation (..), NotTranslation (..))
import Vehicle.Backend.LossFunction.TypeSystem.Core
import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Compile.Type.Subsystem.Standard.Interface
import Vehicle.Expr.DSL

lossBuiltinInstances ::
  DifferentialLogicImplementation ->
  InstanceCandidateDatabase LossBuiltin
lossBuiltinInstances logic = do
  let candidates = mkCandidates logic
  let tcAndCandidates = fmap (second (: []) . extractHeadFromInstanceCandidate) candidates
  Map.fromListWith (<>) tcAndCandidates

--------------------------------------------------------------------------------
-- Builtin instances

-- Manually declared here as we have no way of declaring them in the language
-- itself.

-- Also note that annoyingly because of a lack of first class records we have
-- to duplicate the context for both the candidate and the candidate's solution.

mkCandidates :: DifferentialLogicImplementation -> [InstanceCandidate LossBuiltin]
mkCandidates DifferentialLogicImplementation {..} =
  mkCandidate
    <$> ratOpCandidates (hasRatEq Eq) compileEq
      <> ratOpCandidates (hasRatEq Neq) compileNeq
      <> ratOpCandidates (hasRatOrder Le) compileLe
      <> ratOpCandidates (hasRatOrder Lt) compileLt
      <> ratOpCandidates (hasRatOrder Ge) compileGe
      <> ratOpCandidates (hasRatOrder Gt) compileGt
      <> boolOp2Candidates hasAnd And compileAnd
      <> boolOp2Candidates hasOr Or compileOr
      <> boolOp2Candidates hasImplies Implies compileImplies
      <> ( case compileNot of
             TryToEliminate -> []
             UnaryNot notFn -> boolOp1Candidates hasNot Not notFn
         )
      <> quantifierCandidates
  where
    boolOp2Candidates ::
      (LossDSLExpr -> LossDSLExpr -> LossDSLExpr -> LossDSLExpr) ->
      BuiltinFunction ->
      LossDSLExpr ->
      [(LossDSLExpr, LossDSLExpr)]
    boolOp2Candidates tc boolOp lossOp =
      [ ( tc tBool tBool tBool,
          builtinFunction boolOp
        ),
        ( tc tLoss tBool tLoss,
          explLam "x" tLoss $ \x ->
            explLam "y" tBool $ \y ->
              lossOp @@ [x, boolToLoss y]
        ),
        ( tc tBool tLoss tLoss,
          explLam "x" tBool $ \x ->
            explLam "y" tLoss $ \y ->
              lossOp @@ [boolToLoss x, y]
        ),
        ( tc tLoss tLoss tLoss,
          lossOp
        )
      ]

    boolOp1Candidates ::
      (LossDSLExpr -> LossDSLExpr -> LossDSLExpr) ->
      BuiltinFunction ->
      LossDSLExpr ->
      [(LossDSLExpr, LossDSLExpr)]
    boolOp1Candidates tc boolOp lossOp =
      [ ( tc tBool tBool,
          builtinFunction boolOp
        ),
        ( tc tLoss tLoss,
          lossOp
        )
      ]

    ratOpCandidates ::
      (LossDSLExpr -> LossDSLExpr -> LossDSLExpr -> LossDSLExpr) ->
      LossDSLExpr ->
      [(LossDSLExpr, LossDSLExpr)]
    ratOpCandidates tc lossOp =
      [ ( tc tRat tRat tLoss,
          lossOp
        )
      ]

    quantifierCandidates :: [(LossDSLExpr, LossDSLExpr)]
    quantifierCandidates =
      [ ( forAll "t" type0 $ \t ->
            hasQuant Forall (t ~> tLoss) tLoss,
          compileForall
        ),
        ( forAll "t" type0 $ \t ->
            hasQuant Exists (t ~> tLoss) tLoss,
          compileExists
        )
      ]

    boolToLoss :: LossDSLExpr -> LossDSLExpr
    boolToLoss e = ite tLoss e compileTrue compileFalse
