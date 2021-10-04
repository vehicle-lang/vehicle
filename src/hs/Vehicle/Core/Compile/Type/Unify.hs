{-# LANGUAGE OverloadedLists #-}

module Vehicle.Core.Compile.Type.Unify
  ( solveUnificationConstraint
  ) where

import Control.Monad (when)
import Control.Monad.Except (MonadError(..), throwError)
import Data.List (intersect)

import Vehicle.Prelude
import Vehicle.Core.AST
import Vehicle.Core.Compile.Type.Core
import Vehicle.Core.Compile.Type.Meta
import Vehicle.Core.Compile.Type.Normalise (whnf)
import Vehicle.Core.MetaSet qualified as MetaSet (singleton)


--------------------------------------------------------------------------------
-- Error handling

unexpectedCase :: Provenance -> Doc ann -> a
unexpectedCase p expr = developerError $
  expr <+> "should not exist during unification" <+> pretty p

--------------------------------------------------------------------------------
-- Unification algorithm

pattern (:~:) :: a -> b -> (a, b)
pattern x :~: y = (x,y)

solveUnificationConstraint :: MonadConstraintSolving m
                           => Constraint
                           -> UnificationPair
                           -> m ConstraintProgress
-- Errors
solveUnificationConstraint constraint@(Constraint constraintCtx _) (e1, e2) = do
  whnfE1 <- whnf (declContext constraint) e1
  whnfE2 <- whnf (declContext constraint) e2
  let p = prov constraint

  progress <- case (decomposeApp whnfE1, decomposeApp whnfE2) of
    ----------------------
    -- Impossible cases --
    ----------------------
    (Let{}, _) :~: _           -> unexpectedCase p "Let bindings"
    _          :~: (Let{}, _)  -> unexpectedCase p "Let bindings"

    (Hole{}, _) :~: _           -> unexpectedCase p "Holes"
    _           :~: (Hole{}, _) -> unexpectedCase p "Holes"

    -----------------------
    -- Rigid-rigid cases --
    -----------------------
    (Type l1, [])   :~: (Type l2, []) -> do
      solveEq constraint l1  l2;
      return Progress
        { newConstraints = mempty
        , solvedMetas    = mempty
        }
    {-
    (Ann _ e1 t1, Ann _ e2 t2) -> return
        [ Unify p ctx (exprs : history) mempty (e1, e2)
        , Unify p ctx (exprs : history) mempty (t1, t2)
        ]
    -}
    -- We ASSUME that all terms here are in normal form, so there
    -- will never be an unreduced redex.
    ((Lam _ binder1 body1, []) :~: (Lam _ binder2 body2, []))
      | vis binder1 /= vis binder2 -> throwError $ FailedConstraints [constraint]
      | otherwise -> return Progress
        { newConstraints = [Constraint constraintCtx (Unify (body1, body2))]
        , solvedMetas    = mempty
        }

    (Seq _ es1, []) :~: (Seq _ es2, [])
      -- TODO more informative error message
      | length es1 /= length es2 -> throwError $ FailedConstraints [constraint]
      -- TODO need to try and unify `Seq` with `Cons`s.
      | otherwise -> return Progress
        { newConstraints = zipWith (curry (Constraint constraintCtx . Unify)) es1 es2
        , solvedMetas    = mempty
        }

    (Pi _ binder1 body1, []) :~: (Pi _ binder2 body2, [])
      | vis binder1 /= vis binder2 -> throwError $ FailedConstraints [constraint]
      | otherwise -> do
          -- !!TODO!! Block until binders are solved
          -- One possible implementation, blocked metas = set of sets where outer is conjunction and inner is disjunction
          -- BOB: this effectively blocks until the binders are solved, because we usually just try to eagerly solve problems
          let binderConstraint = Constraint constraintCtx (Unify (binderType binder1, binderType binder2))
          let bodyConstraint   = Constraint constraintCtx (Unify (body1, body2))
          return Progress
            { newConstraints = [binderConstraint, bodyConstraint]
            , solvedMetas    = mempty
            }

    (Builtin _ op1, args1) :~: (Builtin _ op2, args2) ->
      solveSimpleApplication constraint op1 op2 args1 args2

    (Var _ v1, args1) :~: (Var _ v2, args2) ->
      solveSimpleApplication constraint v1 v2 args1 args2

    (Literal _ l1, args1) :~: (Literal _ l2, args2) ->
      solveSimpleApplication constraint l1 l2 args1 args2

    ---------------------
    -- Flex-flex cases --
    ---------------------

    (Meta _ i, args1) :~: (Meta _ j, args2)
      -- If the expressions are exactly equal then simply discard the constraint
      -- as it doesn't tell us anything.
      | i == j && args1 == args2 ->
        return Progress
          { newConstraints = mempty
          , solvedMetas    = mempty
          }

      -- If the meta-variables are equal but their arguments are not
      -- then the meta-variables can only depend on the set of arguments
      -- that are shared in the same position by both of them. Calculate
      -- this set of arguments and create new meta-variable that only
      -- depends on these, and set the old meta-variable to equal that one.
      | i == j && args1 /= args2 -> do
        when (length args1 /= length args2) $
          developerError "Identical meta variables have different numbers of arguments"

        let sharedArgs = positionalIntersection args1 args2
        let sharedArgsCtx = map (\(Arg _ _ e) -> (Machine, e)) sharedArgs
        (_metaName, meta) <- freshMetaWith sharedArgsCtx p

        let abstractedMeta = abstractOver args1 meta
        metaSolved p i abstractedMeta

        return Progress
          { newConstraints = mempty
          , solvedMetas    = MetaSet.singleton i
          }

      -- Then the metas are not equal.
      -- If the first meta has no arguments then simply set equal to the second meta.
      --
      -- Note: this case and the one below it are not strictly necessary as the
      -- final case handles it, but short-circuiting here avoids 1) generating new metas
      -- 2) performing extra unification passes and 3) often makes the process more understandable.
      | null args1 -> do
        metaSolved p i whnfE2
        return Progress
          { newConstraints = mempty
          , solvedMetas    = MetaSet.singleton i
          }

      -- Likewise if the second meta has no arguments then simply set equal to the first meta.
      | null args2 -> do
        metaSolved p j whnfE1
        return Progress
          { newConstraints = mempty
          , solvedMetas    = MetaSet.singleton j
          }

      -- Finally if the meta-variables are different then we have much more
      -- flexibility as to how the arguments can relate to each other. In
      -- particular they can be re-arranged, and therefore we calculate the
      -- non-positional intersection of their arguments. Then proceed as above
      -- for each of the meta-variables in turn.
      | otherwise -> do
        let sharedArgs = args1 `intersect` args2
        let sharedArgsCtx = map (\(Arg _ _ e) -> (Machine, e)) sharedArgs

        (_metaName, meta) <- freshMetaWith sharedArgsCtx p

        let abstractedMeta1 = abstractOver args1 meta
        metaSolved p i abstractedMeta1

        let abstractedMeta2 = abstractOver args2 meta
        metaSolved p j abstractedMeta2

        return Progress
          { newConstraints = mempty
          , solvedMetas    = MetaSet.singleton i <> MetaSet.singleton j
          }

    ----------------------
    -- Flex-rigid cases --
    ----------------------

    -- ?X e1 e2 e3 =?= ?Y x y z

    -- ==> ?Y := \x. \y. \z. ?X e1 e2 e3

    (Meta _ i, args) :~: _ -> do
      -- Check that 'args' is a pattern
      case patternOfArgs args of
        Nothing -> do
          -- This constraint is stuck because it is not pattern; shelve
          -- it for now and hope that another constraint allows us to
          -- progress.

          -- TODO need to check with Bob what this is stuck on, presumably i?
          return Stuck
        Just subst -> do
          -- 'subst' is a renaming that renames the variables in 'e2' to
          -- ones bound by the metavariable
          case substAll subst whnfE2 of
            Nothing -> do
              return Stuck
            Just defnBody -> do
              -- TODO: fail if 'Meta _ i' occurs in 'e2'
              metaSolved p i (abstractOver args defnBody)
              return Progress
                { newConstraints = mempty
                , solvedMetas    = MetaSet.singleton i
                }


    _t :~: (Meta _ _i, _args) ->
      -- this is the mirror image of the previous case, so just swap the
      -- problem over.
      solveUnificationConstraint constraint (whnfE2, whnfE1)

    -- Catch-all
    _ -> throwError $ FailedConstraints [constraint]

  return progress

abstractOver :: [CheckedArg] -> CheckedExpr -> CheckedExpr
abstractOver args body = foldr argToLam body args
  where
    argToLam :: CheckedArg -> CheckedExpr -> CheckedExpr
    argToLam (Arg _ v argE) = Lam mempty (Binder mempty v Machine argE)

solveEq :: (MonadConstraintSolving m, Eq a)
        => Constraint
        -> a
        -> a
        -> m ()
solveEq c v1 v2
  | v1 /= v2  = throwError $ FailedConstraints [c]
  | otherwise = do
    return ()

solveArg :: MonadConstraintSolving m
         => Constraint
         -> (CheckedArg, CheckedArg)
         -> m Constraint
solveArg c (arg1, arg2)
  | vis arg1 /= vis arg2 = throwError $ FailedConstraints [c]
  | otherwise = return $ Constraint
    (ConstraintContext (prov c) mempty (variableContext c))
    (Unify (argExpr arg1 , argExpr arg2))

solveSimpleApplication :: (MonadConstraintSolving m, Eq a)
                       => Constraint
                       -> a -> a
                       -> [CheckedArg] -> [CheckedArg]
                       -> m ConstraintProgress
solveSimpleApplication constraint fun1 fun2 args1 args2 = do
  solveEq constraint fun1 fun2
  if length args1 /= length args2 then
    throwError $ FailedConstraints [constraint]
  else if null args1 then do
    logDebug "solved-trivially"
    return Progress
      { newConstraints = mempty
      , solvedMetas    = mempty
      }
  else do
    newConstraints <- traverse (solveArg constraint) (zip args1 args2)
    return Progress
      { newConstraints = newConstraints
      , solvedMetas    = mempty
      }

positionalIntersection :: Eq a => [a] -> [a] -> [a]
positionalIntersection [] _       = []
positionalIntersection _ []       = []
positionalIntersection (x : xs) (y : ys)
 | x == y    = x : positionalIntersection xs ys
 | otherwise = positionalIntersection xs ys