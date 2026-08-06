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
import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta (MetaSet)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Monad.Class
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Builtin.Standard.Normalise ()
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Code.Interface
import Vehicle.Data.Variable.Bound.Context.Generic (namedBoundCtxOf)

--------------------------------------------------------------------------------
-- Solve index constraints

solveIndexConstraint ::
  (MonadTypeChecker Builtin m) =>
  WithContext (InstanceConstraint Builtin) ->
  m ()
solveIndexConstraint constraint = do
  let args = mapMaybe getExplicitArg $ goalSpine $ instanceGoal $ objectIn constraint
  progress <- solveInDomain constraint args
  case progress of
    Nothing -> do
      let solution = Builtin mempty (BuiltinConstructor UnitLiteral)
      instantiateInstanceConstraintSolution constraint solution
    Just metas -> do
      let blockedConstraint = blockConstraintOn constraint metas
      addAuxiliaryInstanceConstraints [blockedConstraint]

-- | Function signature for constraints solved by type class resolution.
-- This should eventually be refactored out so all are solved by instance
-- search.
solveInDomain ::
  forall m.
  (MonadTypeChecker Builtin m) =>
  WithContext (InstanceConstraint Builtin) ->
  [ThunkWithMetas Builtin] ->
  m (Maybe MetaSet)
solveInDomain c [value, typ] = do
  let nameCtx = namedBoundCtxOf $ contextOf c
  (forcedType, blockingMetas1) <- forceThunkWithMetas nameCtx typ
  case forcedType of
    INatType {} -> return Nothing
    ITensorType tElem dims -> do
      (forcedTElem, blockingMetas2) <- forceThunkWithMetas nameCtx tElem
      (forcedDims, blockingMetas3) <- forceThunkWithMetas nameCtx dims
      case (forcedTElem, forcedDims) of
        (IRatType, IDimNil) -> return Nothing
        _ -> return $ blockOnMetas (blockingMetas2 <> blockingMetas3)
    IIndexType size -> do
      (forcedValue, blockingMetas2) <- forceThunkWithMetas nameCtx value
      case forcedValue of
        INatLiteral n -> do
          (sizeLowerBound, sizeBlockingMetas) <- findLowerBound (contextOf c) (Forced forcedValue) size
          if n < sizeLowerBound
            then return Nothing
            else
              if not (MetaSet.null sizeBlockingMetas)
                then return $ Just sizeBlockingMetas
                else throwError $ TypingError $ FailedIndexConstraintTooBig ctx n sizeLowerBound
        _ -> return $ blockOnMetas blockingMetas2
    _ -> return $ blockOnMetas blockingMetas1
  where
    ctx = contextOf c
solveInDomain c _ = malformedConstraintError c

blockOnMetas :: MetaSet -> Maybe MetaSet
blockOnMetas metas = do
  if MetaSet.null metas
    then Nothing
    else Just metas

findLowerBound ::
  forall m.
  (MonadTypeChecker Builtin m) =>
  ConstraintContext Builtin ->
  ThunkWithMetas Builtin ->
  ThunkWithMetas Builtin ->
  m (Int, MetaSet)
findLowerBound ctx value size = go size
  where
    go :: ThunkWithMetas Builtin -> m (Int, MetaSet)
    go s = do
      (forcedSize, blockingMetas) <- forceThunkWithMetas (namedBoundCtxOf ctx) s
      case forcedSize of
        INatLiteral n ->
          return (n, mempty)
        VBuiltin (BuiltinFunction (Add AddNat)) [e1, e2] -> do
          (b1, m1) <- go $ argExpr e1
          (b2, m2) <- go $ argExpr e2
          return (b1 + b2, m1 <> m2)
        VFreeVar {} -> do
          -- A parameter can be any value... so assume the worst.
          return (0, mempty)
        _
          | MetaSet.null blockingMetas ->
              throwError $ TypingError $ FailedIndexConstraintUnknown ctx value size
          | otherwise -> return (0, blockingMetas)

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
    (InstanceGoal [] (Right NatInDomainConstraint) [value, typ]) -> do
      (forcedValue, _) <- forceThunkWithMetas (namedBoundCtxOf ctx) $ argExpr value
      (forcedType, _) <- forceThunkWithMetas (namedBoundCtxOf ctx) $ argExpr typ
      case forcedType of
        IIndexType size -> do
          let succN = Forced $ case forcedValue of
                INatLiteral x -> INatLiteral (x + 1)
                n' -> mkExpr accessAddNat (Op2Args (Forced n') (Forced $ INatLiteral 1))

          let constraintInfo = (ctx, instanceOrigin constraint)
          newSizeConstraint <- createInstanceUnification constraintInfo size succN
          addUnificationConstraints [newSizeConstraint]
          return True
        _ -> return False
    _ -> return False
