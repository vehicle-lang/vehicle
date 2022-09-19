{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Vehicle.Compile.Type.ConstraintSolver.Unification
  ( solveUnificationConstraint
  ) where

import Control.Monad (when)
import Control.Monad.Except (MonadError(..), throwError)
import Data.List (intersect)
import Data.Maybe (catMaybes)
import Data.Traversable (for)
import Data.IntMap qualified as IntMap

import Vehicle.Language.AST
import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.MetaSet qualified as MetaSet (singleton)
import Vehicle.Compile.Type.MetaMap qualified as MetaMap (member, toList)
import Vehicle.Language.Print (prettyVerbose)
import Vehicle.Compile.Type.ConstraintSolver.Core

--------------------------------------------------------------------------------
-- Unification algorithm

passName :: Doc a
passName = "unification"

pattern (:~:) :: a -> b -> (a, b)
pattern x :~: y = (x,y)

solveUnificationConstraint :: MonadMeta m
                           => ConstraintContext
                           -> UnificationConstraint
                           -> m ConstraintProgress
-- Errors
solveUnificationConstraint ctx pair@(Unify (e1, e2)) = do
  let c = UC ctx pair

  progress <- case (toHead e1, toHead e2) of

    ----------------------
    -- Impossible cases --
    ----------------------

    (Let{}, _) :~: _           -> unexpectedExprError passName "Let"
    _          :~: (Let{}, _)  -> unexpectedExprError passName "Let"

    (Hole{}, _) :~: _           -> unexpectedExprError passName "Holes"
    _           :~: (Hole{}, _) -> unexpectedExprError passName "Holes"

    -----------------------
    -- Rigid-rigid cases --
    -----------------------

    (Universe _ l1, []) :~: (Universe _ l2, []) -> do
      solveEq c l1 l2
      return $ Progress mempty

    -- We ASSUME that all terms here are in normal form, so there
    -- will never be an unreduced redex.
    (Lam _ binder1 body1, []) :~: (Lam _ binder2 body2, [])
      | visibilityMatches binder1 binder2 -> return $ Progress [unify c body1 body2]
      | otherwise                         -> throwError $ FailedConstraints [c]

    (LVec _ es1, args1) :~: (LVec _ es2, args2)
      -- TODO more informative error message
      | length es1 /= length es2 || length args1 /= length args2 ->
        throwError $ FailedConstraints [c]
      -- TODO need to try and unify `LVec` with `Cons`s.
      | otherwise -> do
        let elemConstraints = zipWith (unify c) es1 es2
        argConstraints <- solveArgs c (args1, args2)
        return $ Progress $ elemConstraints <> argConstraints

    (Pi _ binder1 body1, []) :~: (Pi _ binder2 body2, [])
      | visibilityMatches binder1 binder2 -> do
          -- !!TODO!! Block until binders are solved
          -- One possible implementation, blocked metas = set of sets where outer is conjunction and inner is disjunction
          -- BOB: this effectively blocks until the binders are solved, because we usually just try to eagerly solve problems
          let binderConstraint = unify c (typeOf binder1) (typeOf binder2)
          let bodyConstraint   = unify c body1 body2
          return $ Progress [binderConstraint, bodyConstraint]

      | otherwise -> throwError $ FailedConstraints [c]

    (Builtin _ op1, args1) :~: (Builtin _ op2, args2) ->
      solveSimpleApplication c op1 op2 args1 args2

    (Var _ v1, args1) :~: (Var _ v2, args2) ->
      solveSimpleApplication c v1 v2 args1 args2

    (Literal _ l1, args1) :~: (Literal _ l2, args2) ->
      solveSimpleApplication c l1 l2 args1 args2

    ---------------------
    -- Flex-flex cases --
    ---------------------

    (Meta _ i, args1) :~: (Meta _ j, args2)
      -- If the meta-variables are equal then simply discard the constraint
      -- as it doesn't tell us anything.
      | i == j -> if args1 == args2
        then return $ Progress mempty
        else compilerDeveloperError $
          -- Used to have a case for this, e.g. see commit cc02a9d for details,
          -- but I don't think its necessary as we always replace the meta-variable
          -- with a new one in order to update the context.
          "Found the same meta-variable" <+> pretty i <+> "unequal contexts:" <+>
          prettyVerbose args1 <+> prettyVerbose args2

      -- Finally if the meta-variables are different then we have much more
      -- flexibility as to how the arguments can relate to each other. In
      -- particular they can be re-arranged, and therefore we calculate the
      -- non-positional intersection of their arguments. Then proceed as above
      -- for each of the meta-variables in turn.
      | otherwise -> do
        MetaInfo meta1Origin meta1Type _ <- getMetaInfo i
        MetaInfo meta2Origin meta2Type _ <- getMetaInfo j

        let jointContext = args1 `intersect` args2
        meta <- createMetaWithRestrictedContext (meta1Origin <> meta2Origin) meta1Type jointContext

        metaSolved i meta
        metaSolved j meta

        return $ Progress [unify c meta1Type meta2Type]

    ----------------------
    -- Flex-rigid cases --
    ----------------------

    -- ?X e1 e2 e3 =?= ?Y x y z

    -- ==> ?Y := \x. \y. \z. ?X e1 e2 e3

    (Meta _ i, args) :~: _ -> do

      -- Check that 'args' is a pattern and try to calculate a substitution
      -- that renames the variables in 'e2' to ones available to meta `i`
      case getArgPattern args of
        Nothing -> do
          -- This constraint is stuck because it is not pattern; shelve
          -- it for now and hope that another constraint allows us to
          -- progress.
          -- TODO need to check with Bob what this is stuck on, presumably i?
          return $ Stuck $ MetaSet.singleton i

        Just argPattern -> do
          let forwardSubst = patternToSubst argPattern
          let metasInE2 = metasInWithArgs e2

          -- If `i` is inside the term we're trying to unify it with then error.
          -- Unsure if this should be a user or a developer error.
          when (i `MetaMap.member` metasInE2) $
            compilerDeveloperError $
              "Meta variable" <+> pretty i <+> "found in own solution" <+>
              squotes (prettyVerbose e2)

          -- Restrict any arguments to each sub-meta on the RHS to those of i.
          newMetasSolved <- or <$> for (MetaMap.toList metasInE2) (\(j, jArgs) -> do
            MetaInfo jOrigin jType _ <- getMetaInfo j

            let sharedContext = args `intersect` jArgs
            if sharedContext == jArgs
              then return False
              else do
                meta <- createMetaWithRestrictedContext jOrigin jType sharedContext
                metaSolved j meta
                return True)

          finalE2 <- if newMetasSolved then substMetas e2 else return e2

          case substAll forwardSubst finalE2 of
            Nothing       -> return $ Stuck $ MetaSet.singleton i
            Just defnBody -> do
              metaSolved i defnBody
              return $ Progress mempty

    _t :~: (Meta{}, _) ->
      -- this is the mirror image of the previous case, so just swap the
      -- problem over.
      solveUnificationConstraint ctx (Unify (e2, e1))

    -- Catch-all
    _ -> blockOnReductionBlockingMetasOrThrowError [e1,e2] (FailedConstraints [c])

  return progress

solveEq :: (MonadMeta m, Eq a)
        => Constraint
        -> a
        -> a
        -> m ()
solveEq c v1 v2
  | v1 /= v2  = throwError $ FailedConstraints [c]
  | otherwise = logDebug MaxDetail "solved-trivially"

solveArg :: MonadMeta m
         => Constraint
         -> (CheckedArg, CheckedArg)
         -> m (Maybe Constraint)
solveArg c (arg1, arg2)
  | not (visibilityMatches arg1 arg2) = throwError $ FailedConstraints [c]
  | isInstance arg1                   = return Nothing
  | otherwise                         = return $ Just $ unify c (argExpr arg1) (argExpr arg2)

solveArgs :: MonadMeta m
          => Constraint
          -> ([CheckedArg], [CheckedArg])
          -> m [Constraint]
solveArgs c (args1, args2)= catMaybes <$> traverse (solveArg c) (zip args1 args2)

solveSimpleApplication :: (MonadMeta m, Eq a)
                       => Constraint
                       -> a -> a
                       -> [CheckedArg] -> [CheckedArg]
                       -> m ConstraintProgress
solveSimpleApplication constraint fun1 fun2 args1 args2 = do
  if fun1 /= fun2 || length args1 /= length args2 then
    throwError $ FailedConstraints [constraint]
  else if null args1 then do
    logDebug MaxDetail "solved-trivially"
    return $ Progress mempty
  else do
    newConstraints <- solveArgs constraint (args1, args2)
    return $ Progress newConstraints

createMetaWithRestrictedContext :: MonadMeta m => Provenance -> CheckedType -> [CheckedArg] -> m CheckedExpr
createMetaWithRestrictedContext p metaType newContext = do
  let sharedExprs = fmap argExpr newContext
  let sharedArgsCtx = fmap (\arg -> (Nothing, arg, Nothing)) sharedExprs
  let sharedArgsSubst = IntMap.fromAscList (zip [0..] sharedExprs)
  meta <- freshExprMeta p metaType sharedArgsCtx
  case substAll sharedArgsSubst meta of
    Just e  -> return e
    Nothing -> compilerDeveloperError $ "Malfomed substitution" <+> prettyVerbose sharedArgsSubst