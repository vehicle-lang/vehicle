module Vehicle.Compile.Type.Constraint.IndexSolver where

import Control.Monad.Except (MonadError (..))
import Data.Maybe (mapMaybe)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta (MetaSet)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Monad
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value
import Vehicle.Syntax.Builtin

solveIndexConstraint ::
  forall m.
  (TCM Builtin m) =>
  WithContext (InstanceConstraint Builtin) ->
  m ()
solveIndexConstraint constraint = do
  normConstraint@(WithContext (Resolve _ meta _ expr) ctx) <- substMetas constraint
  logDebug MaxDetail $ "Forced:" <+> prettyFriendly normConstraint

  case expr of
    VBuiltin _ args -> do
      progress <- solveInDomain normConstraint (mapMaybe getExplicitArg args)
      case progress of
        Nothing -> do
          let solution = IUnitLiteral (provenanceOf ctx)
          solveMeta meta solution (boundContext ctx)
        Just metas -> do
          let blockedConstraint = blockConstraintOn (mapObject InstanceConstraint normConstraint) metas
          addConstraints [blockedConstraint]
    _ -> compilerDeveloperError $ "Malformed instance goal" <+> prettyFriendly normConstraint

--------------------------------------------------------------------------------
-- InDomain

-- | Function signature for constraints solved by type class resolution.
-- This should eventually be refactored out so all are solved by instance
-- search.
solveInDomain ::
  forall m.
  (TCM Builtin m) =>
  WithContext (InstanceConstraint Builtin) ->
  [WHNFType Builtin] ->
  m (Maybe MetaSet)
solveInDomain c [value, typ] = case typ of
  (getNMeta -> Just {}) -> return $ blockOnMetas [typ]
  INatType {} -> return Nothing
  IRatType {} -> return Nothing
  IIndexType _ size -> case value of
    VMeta {} -> return $ blockOnMetas [value]
    INatLiteral _ n -> do
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

blockOnMetas :: [WHNFValue Builtin] -> Maybe MetaSet
blockOnMetas args = do
  let metas = mapMaybe getNMeta args
  if null metas
    then Nothing
    else Just (MetaSet.fromList metas)

findLowerBound ::
  forall m.
  (TCM Builtin m) =>
  ConstraintContext Builtin ->
  WHNFType Builtin ->
  WHNFType Builtin ->
  m (MetaSet, Int)
findLowerBound ctx value indexSize = go indexSize
  where
    go :: WHNFType Builtin -> m (MetaSet, Int)
    go = \case
      VMeta m _ ->
        return (MetaSet.singleton m, 0)
      INatLiteral _ n ->
        return (mempty, n)
      VFreeVar {} ->
        return (mempty, 0)
      IAdd AddNat e1 e2 -> do
        (m1, b1) <- go e1
        (m2, b2) <- go e2
        return (m1 <> m2, b1 + b2)
      _ -> throwError $ TypingError $ FailedIndexConstraintUnknown ctx value indexSize
