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
import Vehicle.Compile.Type.Meta.Substitution
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Subsystem.Standard.Interface
import Vehicle.Expr.Normalised

solveIndexConstraint ::
  forall builtin m.
  (TCM builtin m, HasStandardTypes builtin) =>
  WithContext (InstanceConstraint builtin) ->
  m ()
solveIndexConstraint constraint = do
  normConstraint@(WithContext (Resolve _ meta _ expr) ctx) <- substMetas constraint
  logDebug MaxDetail $ "Forced:" <+> prettyFriendly normConstraint

  case expr of
    VBuiltin _ args -> do
      progress <- solveInDomain normConstraint (mapMaybe getExplicitArg args)
      case progress of
        Nothing -> do
          let solution = UnitLiteral (provenanceOf ctx)
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
  forall builtin m.
  (TCM builtin m, HasStandardTypes builtin) =>
  WithContext (InstanceConstraint builtin) ->
  [VType builtin] ->
  m (Maybe MetaSet)
solveInDomain c [value, typ] = case typ of
  (getNMeta -> Just {}) -> return $ blockOnMetas [typ]
  VNatType {} -> return Nothing
  VIntType {} -> return Nothing
  VRatType {} -> return Nothing
  VIndexType size -> case size of
    (getNMeta -> Just {}) -> return $ blockOnMetas [size]
    VNatLiteral m -> case value of
      (getNMeta -> Just {}) -> return $ blockOnMetas [value]
      VNatLiteral n
        | m > n -> return Nothing
        | otherwise -> throwError $ TypingError $ FailedIndexConstraintTooBig ctx n m
      _ -> malformedConstraintError c
    _ -> throwError $ TypingError $ FailedIndexConstraintUnknown ctx value size
  _ -> malformedConstraintError c
  where
    ctx = contextOf c
solveInDomain c _ = malformedConstraintError c

blockOnMetas :: [Value builtin] -> Maybe MetaSet
blockOnMetas args = do
  let metas = mapMaybe getNMeta args
  if null metas
    then Nothing
    else Just (MetaSet.fromList metas)
