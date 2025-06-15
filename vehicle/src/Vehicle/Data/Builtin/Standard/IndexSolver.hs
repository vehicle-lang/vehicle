module Vehicle.Data.Builtin.Standard.IndexSolver
  ( solveIndexConstraint,
    solveDefaultIndexConstraints,
  )
where

import Control.Monad (forM)
import Control.Monad.Except (MonadError (..))
import Data.Maybe (mapMaybe)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta (MetaSet)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Monad.Class
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Value

--------------------------------------------------------------------------------
-- Solve index constraints

solveIndexConstraint ::
  (MonadTypeChecker Builtin m) =>
  WithContext (InstanceConstraint Builtin) ->
  m ()
solveIndexConstraint constraint = do
  normConstraint <- substMetas constraint
  logDebug MaxDetail $ "Forced:" <+> prettyFriendly normConstraint

  let args = mapMaybe getExplicitArg $ goalSpine $ instanceGoal $ objectIn normConstraint
  progress <- solveInDomain normConstraint args
  case progress of
    Nothing -> do
      let solution = Builtin mempty (BuiltinConstructor UnitLiteral)
      instantiateInstanceConstraintSolution normConstraint solution
    Just metas -> do
      let blockedConstraint = blockConstraintOn normConstraint metas
      addAuxiliaryInstanceConstraints [blockedConstraint]

-- | Function signature for constraints solved by type class resolution.
-- This should eventually be refactored out so all are solved by instance
-- search.
solveInDomain ::
  forall m.
  (MonadTypeChecker Builtin m) =>
  WithContext (InstanceConstraint Builtin) ->
  [VType Builtin] ->
  m (Maybe MetaSet)
solveInDomain _ [_, typ@VMeta {}] = return $ blockOnMetas [typ]
solveInDomain c [value, typ] = case toTypeValue typ of
  VNatType {} -> return Nothing
  VRatTensorType INil {} -> return Nothing
  VIndexType size -> case value of
    VMeta {} -> return $ blockOnMetas [value]
    INatLiteral n -> do
      (sizeBlockingMetas, sizeLowerBound) <- findLowerBound ctx value size
      if n < sizeLowerBound
        then return Nothing
        else
          if not (MetaSet.null sizeBlockingMetas)
            then return $ Just sizeBlockingMetas
            else throwError $ TypingError $ FailedIndexConstraintTooBig ctx n sizeLowerBound
    _ -> malformedConstraintError c
  _ -> malformedConstraintError c
  where
    ctx = contextOf c
solveInDomain c _ = malformedConstraintError c

blockOnMetas :: [Value Builtin] -> Maybe MetaSet
blockOnMetas args = do
  let metas = mapMaybe getNMeta args
  if null metas
    then Nothing
    else Just (MetaSet.fromList metas)

findLowerBound ::
  forall m.
  (MonadTypeChecker Builtin m) =>
  ConstraintContext Builtin ->
  VType Builtin ->
  VType Builtin ->
  m (MetaSet, Int)
findLowerBound ctx value indexSize = go indexSize
  where
    go :: VType Builtin -> m (MetaSet, Int)
    go = \case
      VMeta m _ ->
        return (MetaSet.singleton m, 0)
      INatLiteral n ->
        return (mempty, n)
      VFreeVar {} ->
        return (mempty, 0)
      VBuiltin (BuiltinFunction (Add AddNat)) [argExpr -> e1, argExpr -> e2] -> do
        (m1, b1) <- go e1
        (m2, b2) <- go e2
        return (m1 <> m2, b1 + b2)
      _ -> throwError $ TypingError $ FailedIndexConstraintUnknown ctx value indexSize

--------------------------------------------------------------------------------
-- Default index constraints

solveDefaultIndexConstraints ::
  (MonadTypeChecker Builtin m) =>
  [WithContext (InstanceConstraint Builtin)] ->
  m Bool
solveDefaultIndexConstraints defaultableConstraints = do
  results <- forM defaultableConstraints solveDefaultIndexConstraint
  return $ or results

solveDefaultIndexConstraint ::
  (MonadTypeChecker Builtin m) =>
  WithContext (InstanceConstraint Builtin) ->
  m Bool
solveDefaultIndexConstraint (WithContext constraint ctx) = do
  case instanceGoal constraint of
    (InstanceGoal [] NatInDomainConstraint [n, argExpr -> toTypeValue -> VIndexType size]) -> do
      let succN = fromNatValue $ case argExpr n of
            INatLiteral x -> VNatLiteral (x + 1)
            n' -> VNatAdd (Op2Args n' (INatLiteral 1))

      let constraintInfo = (ctx, instanceOrigin constraint)
      newSizeConstraint <- createInstanceUnification constraintInfo size succN
      addUnificationConstraints [newSizeConstraint]
      return True
    _ -> return False
