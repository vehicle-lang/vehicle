{-# LANGUAGE OverloadedLists #-}

module Vehicle.Compile.Type.Unify
  ( solveUnificationConstraint
  ) where

import Control.Monad (when)
import Control.Monad.Except (MonadError(..), throwError)
import Data.List (intersect)

import Vehicle.Language.AST
import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.MetaSet qualified as MetaSet (singleton)
import Vehicle.Compile.Type.WeakHeadNormalForm


--------------------------------------------------------------------------------
-- Error handling

unexpectedCase :: MonadCompile m => Provenance -> Doc () -> m a
unexpectedCase p expr = compilerDeveloperError $
  expr <+> "should not exist during unification" <+> pretty p

--------------------------------------------------------------------------------
-- Unification algorithm

pattern (:~:) :: a -> b -> (a, b)
pattern x :~: y = (x,y)

solveUnificationConstraint :: MonadConstraintSolving m
                           => ConstraintContext
                           -> UnificationConstraint
                           -> m ConstraintProgress
-- Errors
solveUnificationConstraint ctx (Unify (e1, e2)) = do
  whnfE1 <- whnfWithMetas (varContext ctx) e1
  whnfE2 <- whnfWithMetas (varContext ctx) e2
  let constraint = UC ctx (Unify (whnfE1, whnfE2))
  let p = provenanceOf constraint

  progress <- case (toHead whnfE1, toHead whnfE2) of

    ----------------------
    -- Impossible cases --
    ----------------------

    (Let{}, _) :~: _           -> unexpectedCase p "Let bindings"
    _          :~: (Let{}, _)  -> unexpectedCase p "Let bindings"

    (Hole{}, _) :~: _           -> unexpectedCase p "Holes"
    _           :~: (Hole{}, _) -> unexpectedCase p "Holes"

    -------------------
    -- Special cases --
    -------------------

    -- Try to unify with LSeq and Cons
    (LSeq ann dict1 es, []) :~: (Builtin _ Cons, args2) ->
      case (dict1, es, args2) of
        (PrimDict _ (IsContainerExpr _ t1 _), x1 : xs1, [t2, x2, xs2]) -> do
          let typeConstraint = UC ctx (Unify (t1, argExpr t2))
          let headConstraint = UC ctx (Unify (x1, argExpr x2))
          let tailConstraint = UC ctx (Unify (LSeq ann dict1 xs1, argExpr xs2))
          return Progress
            { newConstraints = [typeConstraint, headConstraint, tailConstraint]
            , solvedMetas    = mempty
            }
        _ -> throwError $ FailedConstraints [constraint]

    -- mirror image of the previous case, so just swap the problem over.
    (Builtin _ Cons, _) :~: (LSeq{}, []) ->
      solveUnificationConstraint ctx (Unify (whnfE2, whnfE1))

    -- If a tensor is unified with a non-tensor then it must be a 0 dimensional
    -- tensor.
    (Builtin _ (ContainerType Tensor), [tElem, tDims]) :~: (Builtin _ op, _)
      | op /= ContainerType Tensor -> do
          let emptyDims = mkTensorDims (inserted (annotationOf tDims)) []
          let elemConstraint = UC ctx (Unify (argExpr tElem, whnfE2))
          let dimsConstraint = UC ctx (Unify (argExpr tDims, emptyDims))
          return Progress
            { newConstraints = [elemConstraint, dimsConstraint]
            , solvedMetas    = mempty
            }

    -- Mirror image of the previous case, so just swap the problem over.
    (Builtin _ op, _) :~: (Builtin _ (ContainerType Tensor), [_tElem, _tDims])
      | op /= ContainerType Tensor -> solveUnificationConstraint ctx (Unify (whnfE2, whnfE1))

    -----------------------
    -- Rigid-rigid cases --
    -----------------------

    (Type _ l1, [])   :~: (Type _ l2, []) -> do
      solveEq constraint l1 l2
      return Progress
        { newConstraints = mempty
        , solvedMetas    = mempty
        }

    (PrimDict _ t1, []) :~: (PrimDict _ t2, []) -> do
      let newConstraint = UC ctx (Unify (t1, t2))
      return Progress
        { newConstraints = [newConstraint]
        , solvedMetas    = mempty
        }

    -- We ASSUME that all terms here are in normal form, so there
    -- will never be an unreduced redex.
    (Lam _ binder1 body1, []) :~: (Lam _ binder2 body2, [])
      | visibilityOf binder1 /= visibilityOf binder2 ->
        throwError $ FailedConstraints [constraint]
      | otherwise -> return Progress
        { newConstraints = [UC ctx (Unify (body1, body2))]
        , solvedMetas    = mempty
        }

    (LSeq _ dict1 es1, []) :~: (LSeq _ dict2 es2, [])
      -- TODO more informative error message
      | length es1 /= length es2 ->
        throwError $ FailedConstraints [constraint]
      -- TODO need to try and unify `LSeq` with `Cons`s.
      | otherwise -> do
        let dictConstraint  = UC ctx (Unify (dict1, dict2))
        let elemConstraints = zipWith (curry (UC ctx . Unify)) es1 es2
        return Progress
          { newConstraints = dictConstraint : elemConstraints
          , solvedMetas    = mempty
          }


    (Pi _ binder1 body1, []) :~: (Pi _ binder2 body2, [])
      | visibilityOf binder1 /= visibilityOf binder2 ->
        throwError $ FailedConstraints [constraint]
      | otherwise -> do
          -- !!TODO!! Block until binders are solved
          -- One possible implementation, blocked metas = set of sets where outer is conjunction and inner is disjunction
          -- BOB: this effectively blocks until the binders are solved, because we usually just try to eagerly solve problems
          let binderConstraint = UC ctx (Unify (typeOf binder1, typeOf binder2))
          let bodyConstraint   = UC ctx (Unify (body1, body2))
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
          compilerDeveloperError "Identical meta variables have different numbers of arguments"

        let sharedArgs = positionalIntersection args1 args2
        let sharedArgsCtx = map (\arg -> (Nothing, argExpr arg, Nothing)) sharedArgs
        -- Bit worrying here that we have to arbitrarily pick meta i's origin.
        metaOrigin <- getMetaOrigin i
        (_metaName, meta) <- freshMetaWith metaOrigin sharedArgsCtx

        let abstractedMeta = abstractOver args1 meta
        metaSolved p i abstractedMeta

        return Progress
          { newConstraints = mempty
          , solvedMetas    = MetaSet.singleton i
          }

      -- Finally if the meta-variables are different then we have much more
      -- flexibility as to how the arguments can relate to each other. In
      -- particular they can be re-arranged, and therefore we calculate the
      -- non-positional intersection of their arguments. Then proceed as above
      -- for each of the meta-variables in turn.
      | otherwise -> do
        let sharedArgs = args1 `intersect` args2
        let sharedArgsCtx = map (\arg -> (Nothing, argExpr arg, Nothing)) sharedArgs
        -- Bit worrying here that we have to arbitrarily pick meta i's origin.
        metaOrigin <- getMetaOrigin i
        (_metaName, meta) <- freshMetaWith metaOrigin sharedArgsCtx

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
      solveUnificationConstraint ctx (Unify (whnfE2, whnfE1))

    -- Catch-all
    _ -> do
      logDebug MaxDetail $ pretty (show $ boundContext constraint)
      throwError $ FailedConstraints [constraint]

  return progress

abstractOver :: [CheckedArg] -> CheckedExpr -> CheckedExpr
abstractOver args body = foldr argToLam body args
  where
    argToLam :: CheckedArg -> CheckedExpr -> CheckedExpr
    argToLam (Arg ann v argE) = Lam ann (Binder ann v Nothing argE)

solveEq :: (MonadConstraintSolving m, Eq a)
        => Constraint
        -> a
        -> a
        -> m ()
solveEq c v1 v2
  | v1 /= v2  = throwError $ FailedConstraints [c]
  | otherwise = logDebug MaxDetail "solved-trivially"

solveArg :: MonadConstraintSolving m
         => Constraint
         -> (CheckedArg, CheckedArg)
         -> m Constraint
solveArg c (arg1, arg2)
  | visibilityOf arg1 /= visibilityOf arg2 = throwError $ FailedConstraints [c]
  | otherwise = return $ UC
    (ConstraintContext (provenanceOf c) mempty (variableContext c))
    (Unify (argExpr arg1 , argExpr arg2))

solveSimpleApplication :: (MonadConstraintSolving m, Eq a)
                       => Constraint
                       -> a -> a
                       -> [CheckedArg] -> [CheckedArg]
                       -> m ConstraintProgress
solveSimpleApplication constraint fun1 fun2 args1 args2 = do
  if fun1 /= fun2 || length args1 /= length args2 then
    throwError $ FailedConstraints [constraint]
  else if null args1 then do
    logDebug MaxDetail "solved-trivially"
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