{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Vehicle.Compile.Type.ConstraintSolver.Unification
  ( solveUnificationConstraint
  ) where

import Control.Monad (when)
import Control.Monad.Except (MonadError (..), throwError)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List (intersect)
import Data.Maybe (catMaybes)
import Data.Traversable (for)

import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.ConstraintSolver.Core (unify, unify, blockOnReductionBlockingMetasOrThrowError)
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.MetaMap qualified as MetaMap (member, toList)
import Vehicle.Compile.Type.MetaSet qualified as MetaSet (singleton)
import Vehicle.Compile.Type.Monad
import Vehicle.Language.Print (prettyVerbose)
import Vehicle.Compile.Normalise.NormExpr
import Vehicle.Compile.Normalise.Quote (adjustDBIndices, Quote (..))


--------------------------------------------------------------------------------
-- Unification algorithm

pattern (:~:) :: a -> b -> (a, b)
pattern x :~: y = (x,y)

solveUnificationConstraint :: TCM m
                           => WithContext UnificationConstraint
                           -> m ConstraintProgress
solveUnificationConstraint (WithContext (Unify e1 e2) ctx) = do
  let c = WithContext (Unify e1 e2) ctx
  let ctxSize = length (boundContext ctx)

  progress <- case (e1, e2) of

    -----------------------
    -- Rigid-rigid cases --
    -----------------------

    VUniverse _ l1 :~: VUniverse _ l2 ->
      solveEq c l1 l2

    -- We ASSUME that all terms here are in normal form, so there
    -- will never be an unreduced redex.
    VLam _ _binder1 _env1 _body1 :~: VLam _ _binder2 _env2 _body2
      -> compilerDeveloperError "unification of type-level lambdas not yet supported"
      -- | visibilityMatches binder1 binder2 -> return $ Progress [unify ctx body1 body2]
      -- | otherwise                         -> throwError $ FailedUnificationConstraints [c]

    VLVec _ es1 args1 :~: VLVec _ es2 args2
      | length es1 /= length es2 -> throwError $ FailedUnificationConstraints [c]
      | otherwise                -> do
        let elemEqs = zipWith (unify ctx) es1 es2
        argsEqs <- solveArgs c (args1, args2)
        return $ Progress $ elemEqs <> argsEqs

    VPi _ binder1 body1 :~: VPi _ binder2 body2
      | visibilityMatches binder1 binder2 -> do
        -- !!TODO!! Block until binders are solved
        -- One possible implementation, blocked metas = set of sets where outer is conjunction and inner is disjunction
        -- BOB: this effectively blocks until the binders are solved, because we usually just try to eagerly solve problems
        let binderConstraint = unify ctx (typeOf binder1) (typeOf binder2)
        let bodyConstraint   = unify ctx body1 body2
        return $ Progress [binderConstraint, bodyConstraint]

      | otherwise -> throwError $ FailedUnificationConstraints [c]

    VBuiltin _ op1 args1 :~: VBuiltin _ op2 args2 ->
      solveSimpleApplication c op1 op2 args1 args2

    VVar _ v1 args1 :~: VVar _ v2 args2 ->
      solveSimpleApplication c v1 v2 args1 args2

    VLiteral _ l1 :~: VLiteral _ l2 ->
      solveEq c l1 l2

    ---------------------
    -- Flex-flex cases --
    ---------------------

    VMeta _ i args1 :~: VMeta _ j args2 -> do
      let deps1 = getNormMetaDependencies args1
      let deps2 = getNormMetaDependencies args2
      -- If the meta-variables are equal then simply discard the constraint
      -- as it doesn't tell us anything.
      if i == j then
        if deps1 == deps2
          then return $ Progress mempty
          else compilerDeveloperError $
            -- Used to have a case for this, e.g. see commit cc02a9d for details,
            -- but I don't think its necessary as we always replace the meta-variable
            -- with a new one in order to update the context.
            "Found the same meta-variable" <+> pretty i <+> "with different dependencies:" <+>
            prettyVerbose deps1 <+> prettyVerbose deps2

      -- If the meta-variables are different then we have more
      -- flexibility as to how the arguments can relate to each other. In
      -- particular they can be re-arranged, and therefore we calculate the
      -- non-positional intersection of their arguments.
      else do
        meta1Type <- getMetaType i
        meta2Type <- getMetaType j
        typeEq <- unify ctx <$> whnfNBE ctxSize meta1Type <*> whnfNBE ctxSize meta2Type

        meta1Origin <- getMetaProvenance i
        meta2Origin <- getMetaProvenance j
        let newOrigin = meta1Origin <> meta2Origin

        let jointContext = deps1 `intersect` deps2

        newMeta <- createMetaWithRestrictedDependencies newOrigin meta1Type jointContext

        solveMeta i newMeta ctxSize
        solveMeta j newMeta ctxSize

        return $ Progress [typeEq]

    ----------------------
    -- Flex-rigid cases --
    ----------------------

    -- ?X e1 e2 e3 =?= ?Y x y z

    -- ==> ?Y := \x. \y. \z. ?X e1 e2 e3

    VMeta _ i args :~: _ -> do
      let deps = getNormMetaDependencies args

      -- Check that 'args' is a pattern and try to calculate a substitution
      -- that renames the variables in 'e2' to ones available to meta `i`
      case getArgPattern deps of
        -- This constraint is stuck because it is not pattern; shelve
        -- it for now and hope that another constraint allows us to
        -- progress.
        -- TODO need to check with Bob what this is stuck on, presumably i?
        Nothing -> return $ Stuck $ MetaSet.singleton i

        Just argPattern -> do
          metasInE2 <- metasInWithDependencies e2

          -- If `i` is inside the term we're trying to unify it with then error.
          -- Unsure if this should be a user or a developer error.
          when (i `MetaMap.member` metasInE2) $
            compilerDeveloperError $
              "Meta variable" <+> pretty i <+> "found in own solution" <+>
              squotes (prettyVerbose e2)

          -- Restrict any arguments to each sub-meta on the RHS to those of i.
          newMetasSolved <- or <$> for (MetaMap.toList metasInE2) (\(j, jDeps) -> do
            jOrigin <- getMetaProvenance j
            jType <- getMetaType j

            let sharedDependencies = deps `intersect` jDeps
            if sharedDependencies == jDeps
              then return False
              else do
                meta <- createMetaWithRestrictedDependencies jOrigin jType sharedDependencies
                solveMeta j meta ctxSize
                return True)

          finalE2 <- if newMetasSolved then substMetas e2 else return e2

          let solution = adjustDBIndices 0 (`IntMap.lookup` argPattern) finalE2
          unnormSolution <- quote solution
          solveMeta i unnormSolution ctxSize
          return $ Progress mempty

    _t :~: VMeta{} ->
      -- this is the mirror image of the previous case, so just swap the
      -- problem over.
      solveUnificationConstraint (WithContext (Unify e2 e1) ctx)

    -- Catch-all
    _ -> blockOnReductionBlockingMetasOrThrowError [e1,e2] (FailedUnificationConstraints [c])

  return progress

solveEq :: (TCM m, Eq a)
        => WithContext UnificationConstraint
        -> a
        -> a
        -> m ConstraintProgress
solveEq c v1 v2
  | v1 /= v2  = throwError $ FailedUnificationConstraints [c]
  | otherwise = do
    logDebug MaxDetail "solved-trivially"
    return $ Progress mempty

solveArg :: TCM m
         => WithContext UnificationConstraint
         -> (NormArg, NormArg)
         -> m (Maybe (WithContext Constraint))
solveArg c (arg1, arg2)
  | not (visibilityMatches arg1 arg2) = throwError $ FailedUnificationConstraints [c]
  | isInstance arg1                   = return Nothing
  | otherwise                         = return $ Just $ unify (contextOf c) (argExpr arg1) (argExpr arg2)

solveArgs :: TCM m
          => WithContext UnificationConstraint
          -> ([NormArg], [NormArg])
          -> m [WithContext Constraint]
solveArgs c (args1, args2)= catMaybes <$> traverse (solveArg c) (zip args1 args2)

solveSimpleApplication :: (TCM m, Eq a)
                       => WithContext UnificationConstraint
                       -> a -> a
                       -> [NormArg] -> [NormArg]
                       -> m ConstraintProgress
solveSimpleApplication constraint fun1 fun2 args1 args2 = do
  if fun1 /= fun2 || length args1 /= length args2 then
    throwError $ FailedUnificationConstraints [constraint]
  else if null args1 then do
    logDebug MaxDetail "solved-trivially"
    return $ Progress mempty
  else do
    newConstraints <- solveArgs constraint (args1, args2)
    return $ Progress newConstraints

createMetaWithRestrictedDependencies :: TCM m
                                     => Provenance
                                     -> CheckedType
                                     -> [DBIndex]
                                     -> m CheckedExpr
createMetaWithRestrictedDependencies p metaType newDependencies = do
  meta <- freshExprMeta p metaType (length newDependencies)
  let substitution = IntMap.fromAscList (zip [0..] newDependencies)
  return $ substAll 0 (`IntMap.lookup` substitution) (unnormalised meta)

--------------------------------------------------------------------------------
-- Argument patterns

type ArgPattern = IntMap Int

-- | TODO: explain what this means:
-- [i2 i4 i1] --> [2 -> 2, 4 -> 1, 1 -> 0]
getArgPattern :: [DBIndex] -> Maybe ArgPattern
getArgPattern args = go (length args - 1) IntMap.empty args
  where
    go :: Int -> IntMap Int -> [DBIndex] -> Maybe ArgPattern
    go _ revMap [] = Just revMap
    -- TODO: we could eta-reduce arguments too, if possible
    go i revMap (j : restArgs) =
      if IntMap.member j revMap then
        -- TODO: mark 'j' as ambiguous, and remove ambiguous entries before returning;
        -- but then we should make sure the solution is well-typed
        Nothing
      else
        go (i-1) (IntMap.insert j i revMap) restArgs
