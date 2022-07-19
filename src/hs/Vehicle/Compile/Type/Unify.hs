{-# LANGUAGE OverloadedLists #-}

module Vehicle.Compile.Type.Unify
  ( solveUnificationConstraint
  ) where

import Control.Monad (when, forM_)
import Control.Monad.Except (MonadError(..), throwError)
import Data.List (intersect)
import Data.Maybe (catMaybes)

import Vehicle.Language.AST
import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.MetaSet qualified as MetaSet (singleton)
import Vehicle.Compile.Type.MetaMap qualified as MetaMap (member, toList)
import Vehicle.Language.Print (prettyVerbose)


--------------------------------------------------------------------------------
-- Error handling

unexpectedCase :: MonadCompile m => Provenance -> Doc () -> m a
unexpectedCase p expr = compilerDeveloperError $
  expr <+> "should not exist during unification" <+> pretty p

--------------------------------------------------------------------------------
-- Unification algorithm

pattern (:~:) :: a -> b -> (a, b)
pattern x :~: y = (x,y)

solveUnificationConstraint :: MonadMeta m
                           => ConstraintContext
                           -> UnificationConstraint
                           -> m ConstraintProgress
-- Errors
solveUnificationConstraint ctx c@(Unify (e1, e2)) = do
  let constraint = UC ctx c
  let p = provenanceOf constraint

  progress <- case (toHead e1, toHead e2) of

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
    (LSeq ann (x : xs), [tSeqElem, tCont, tc1, tc2]) :~: (Builtin _ Cons, [tElem, y, ys]) -> do
      let typeConstraint = UC ctx (Unify (argExpr tCont, ListType p (argExpr tElem)))
      let headConstraint = UC ctx (Unify (x, argExpr y))
      let tailConstraint = UC ctx (Unify (App ann (LSeq ann xs) [tSeqElem, tCont, tc1, tc2], argExpr ys))
      return Progress
        { newConstraints = [typeConstraint, headConstraint, tailConstraint]
        , solvedMetas    = mempty
        }

    -- mirror image of the previous case, so just swap the problem over.
    (Builtin _ Cons, _) :~: (LSeq{}, _) ->
      solveUnificationConstraint ctx (Unify (e2, e1))

    -- If a tensor is unified with a non-tensor then it must be a 0 dimensional
    -- tensor.
    (Builtin _ Tensor, [tElem, tDims]) :~: (Builtin _ op, _)
      | op /= Tensor -> do
          let emptyDims = mkTensorDims (inserted (provenanceOf tDims)) []
          let elemConstraint = UC ctx (Unify (argExpr tElem, e2))
          let dimsConstraint = UC ctx (Unify (argExpr tDims, emptyDims))
          return Progress
            { newConstraints = [elemConstraint, dimsConstraint]
            , solvedMetas    = mempty
            }

    -- Mirror image of the previous case, so just swap the problem over.
    (Builtin _ op, _) :~: (Builtin _ Tensor, [_tElem, _tDims])
      | op /= Tensor -> solveUnificationConstraint ctx (Unify (e2, e1))

    -----------------------
    -- Rigid-rigid cases --
    -----------------------

    (Universe _ l1, []) :~: (Universe _ l2, []) -> do
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

    (LSeq _ es1, args1) :~: (LSeq _ es2, args2)
      -- TODO more informative error message
      | length es1 /= length es2 || length args1 /= length args2 ->
        throwError $ FailedConstraints [constraint]
      -- TODO need to try and unify `LSeq` with `Cons`s.
      | otherwise -> do
        let elemConstraints = zipWith (curry (UC ctx . Unify)) es1 es2
        argConstraints <- solveArgs constraint (args1, args2)
        return Progress
          { newConstraints = elemConstraints <> argConstraints
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
        MetaInfo meta1Origin meta1Type _ <- getMetaInfo i
        MetaInfo meta2Origin meta2Type _ <- getMetaInfo j

        when (length args1 /= length args2) $
          compilerDeveloperError "Identical meta variables have different numbers of arguments"

        let sharedArgs = positionalIntersection args1 args2
        let sharedArgsCtx = map (\arg -> (Nothing, argExpr arg, Nothing)) sharedArgs
        let sharedOrigin = meta1Origin <> meta2Origin
        let sharedTypeConstraint = UC ctx (Unify (meta1Type, meta2Type))

        meta <- freshExprMeta sharedOrigin meta1Type sharedArgsCtx
        metaSolved i meta

        return Progress
          { newConstraints = [sharedTypeConstraint]
          , solvedMetas    = MetaSet.singleton i
          }

      -- Finally if the meta-variables are different then we have much more
      -- flexibility as to how the arguments can relate to each other. In
      -- particular they can be re-arranged, and therefore we calculate the
      -- non-positional intersection of their arguments. Then proceed as above
      -- for each of the meta-variables in turn.
      | otherwise -> do
        MetaInfo meta1Origin meta1Type _ <- getMetaInfo i
        MetaInfo meta2Origin meta2Type _ <- getMetaInfo j

        let sharedArgs = args1 `intersect` args2
        let sharedArgsCtx = map (\arg -> (Nothing, argExpr arg, Nothing)) sharedArgs
        let sharedOrigin = meta1Origin <> meta2Origin
        let sharedTypeConstraint = UC ctx (Unify (meta1Type, meta2Type))

        meta <- freshExprMeta sharedOrigin meta1Type sharedArgsCtx
        metaSolved i meta
        metaSolved j meta

        return Progress
          { newConstraints = [sharedTypeConstraint]
          , solvedMetas    = MetaSet.singleton i <> MetaSet.singleton j
          }

    ----------------------
    -- Flex-rigid cases --
    ----------------------

    -- ?X e1 e2 e3 =?= ?Y x y z

    -- ==> ?Y := \x. \y. \z. ?X e1 e2 e3

    (Meta _ i, args) :~: _ -> do

      -- Check that 'args' is a pattern and try to calculate a substitution
      -- that renames the variables in 'e2' to ones available to meta `i`
      case patternOfArgs args of
        Nothing -> do
          -- This constraint is stuck because it is not pattern; shelve
          -- it for now and hope that another constraint allows us to
          -- progress.
          -- TODO need to check with Bob what this is stuck on, presumably i?
          return Stuck

        Just subst -> do
          let metasInE2 = metasInWithArgs e2

          -- If `i` is inside the term we're trying to unify it with then error.
          -- Unsure if this should be a user or a developer error.
          when (i `MetaMap.member` metasInE2) $
            compilerDeveloperError $
              "Meta variable" <+> pretty i <+> "found in own solution" <+>
              squotes (prettyVerbose e2)

          -- Restrict any arguments to each sub-meta on the RHS to those of i.
          forM_ (MetaMap.toList metasInE2) $ \(j, jArgs) -> do
            MetaInfo jOrigin jType _ <- getMetaInfo j
            let sharedArgs = args `intersect` jArgs
            let sharedArgsCtx = map (\arg -> (Nothing, argExpr arg, Nothing)) sharedArgs
            when (sharedArgs /= jArgs) $ do
              meta <- freshExprMeta jOrigin jType sharedArgsCtx
              metaSolved j meta

          case substAll subst e2 of
            Nothing -> do
              return Stuck
            Just defnBody -> do
              -- TODO: fail if 'Meta _ i' occurs in 'e2'
              metaSolved i defnBody
              return Progress
                { newConstraints = mempty
                , solvedMetas    = MetaSet.singleton i
                }

    _t :~: (Meta{}, _) ->
      -- this is the mirror image of the previous case, so just swap the
      -- problem over.
      solveUnificationConstraint ctx (Unify (e2, e1))

    -- Catch-all
    _ -> do
      throwError $ FailedConstraints [constraint]

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
  | visibilityOf arg1 /= visibilityOf arg2 = throwError $ FailedConstraints [c]
  | visibilityOf arg1 == Instance = return Nothing
  | otherwise = return $ Just $ UC
    (ConstraintContext (provenanceOf c) mempty (variableContext c))
    (Unify (argExpr arg1 , argExpr arg2))

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
    return Progress
      { newConstraints = mempty
      , solvedMetas    = mempty
      }
  else do
    newConstraints <- solveArgs constraint (args1, args2)
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

