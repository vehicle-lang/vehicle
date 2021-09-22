
module Vehicle.Core.Compile.Type.Unify
  ( solveUnificationConstraints
  ) where

import Control.Monad (when, foldM)
import Control.Monad.Except (MonadError(..), throwError)
import Control.Monad.Writer (MonadWriter(..), runWriterT)
import Control.Monad.State  (gets)
import Control.Monad.Reader (MonadReader(..), runReaderT)
import Data.List (intersect)
import Data.IntSet qualified as IntSet
import Prettyprinter ( (<+>), Pretty(pretty), Doc )

import Vehicle.Prelude
import Vehicle.Core.Print.Core ()
import Vehicle.Core.AST
import Vehicle.Core.Compile.Type.Core
import Vehicle.Core.Compile.Type.Meta hiding (metaSolved)
import Vehicle.Core.Compile.Type.Meta qualified as Meta (metaSolved)
import Vehicle.Core.Compile.Type.WHNF (whnf)


--------------------------------------------------------------------------------
-- Error handling

unexpectedCase :: Provenance -> Doc ann -> a
unexpectedCase p expr = developerError $
  expr <+> "should not exist during unification" <+> pretty p

--------------------------------------------------------------------------------
-- Unification algorithm

type MonadUnify m =
  -- The current meta context
  ( MonadMeta m
  -- The error thrown
  , MonadError TypingError m
  -- Allows logging during unification
  , MonadLogger m
  )

-- | Tries to solve the current set of unification constraints
-- returning whether or not it makes any progress.
solveUnificationConstraints :: MonadUnify m => m Bool
solveUnificationConstraints = do
  constraints <- getUnificationConstraints
  (unsolvedConstraints, _) <- loop (constraints, Nothing)
  setUnificationConstraints unsolvedConstraints
  return $ length unsolvedConstraints < length constraints
  where
    loop :: MonadUnify m
         => ([UnificationConstraint], Maybe MetaSet)
         -> m ([UnificationConstraint], Maybe MetaSet)
    -- Exit if we have solved all constraints
    loop = \case
      ([], _) -> return ([], Nothing)

      -- Exit if we have not succeeded in solving any metas in the last pass
      (constraints, Just metasSolved)
        | IntSet.null metasSolved -> do
          logDebug "Unable to make progress"
          return (constraints, Nothing)

      -- Otherwise make another pass
      (constraints, metasSolved) -> do
        logDebug "Starting new unification pass"
        logDebug $ "current-constraints:" <+> pretty constraints

        let constraints1 = foldM examineConstraint [] constraints
        let constraints2 = runReaderT constraints1 metasSolved
        (unsolvedConstraints, newMetasSolved) <- runWriterT constraints2

        metaSubst <- getMetaSubstitution
        logDebug $ "current-solution:" <+> pretty metaSubst <> "\n"

        loop (unsolvedConstraints, Just newMetasSolved)



type MonadUnifyPass m =
  ( MonadUnify m
  -- The list of metas that we have made progess on so far in this pass
  , MonadWriter MetaSet m
  -- The list of metas that were made progress on in the previous pass.
  -- None represents that we don't know which metas were made progress on
  -- in the previous pass.
  , MonadReader (Maybe MetaSet) m
  )

examineConstraint :: MonadUnifyPass m
                  => [UnificationConstraint]
                  -> UnificationConstraint
                  -> m [UnificationConstraint]
examineConstraint queue constraint = do
  progressedMetas <- ask
  if isBlocked progressedMetas constraint
    then return (constraint : queue)
    else do
      subst <- gets currentSubstitution
      let updatedConstraint = substMetas subst constraint
      newConstraints <- solveConstraint updatedConstraint
      return (newConstraints <> queue)

pattern (:~:) :: a -> b -> (a, b)
pattern x :~: y = (x,y)

solveConstraint :: MonadUnifyPass m => UnificationConstraint -> m [UnificationConstraint]
-- Errors
solveConstraint constraint@(Unify p ctx history _ exprs@(e1, e2)) = do
  whnfE1 <- whnf e1
  whnfE2 <- whnf e2
  logDebug $ "trying" <+> pretty whnfE1 <+> "~" <+> pretty whnfE2

  case (decomposeApp whnfE1, decomposeApp whnfE2) of
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
    (Type    l1, [])   :~: (Type    l2, [])   -> do solveEq constraint l1  l2; return []
    {-
    (Ann _ e1 t1, Ann _ e2 t2) -> return
        [ Unify p ctx (exprs : history) mempty (e1, e2)
        , Unify p ctx (exprs : history) mempty (t1, t2)
        ]
    -}
    -- We ASSUME that all terms here are in normal form, so there
    -- will never be an unreduced redex.
    ((Lam _ binder1 body1, []) :~: (Lam _ binder2 body2, []))
      | vis binder1 /= vis binder2 -> throwError $ UnificationFailure constraint
      | otherwise -> return
        [ Unify p (addBinderToCtx binder1 binder2 ctx) (exprs : history) mempty (body1, body2) ]

    (Seq _ es1, []) :~: (Seq _ es2, [])
      -- TODO more informative error message
      | length es1 /= length es2 -> throwError $ UnificationFailure constraint
      -- TODO need to try and unify `Seq` with `Cons`s.
      | otherwise -> return $ zipWith (curry (Unify p ctx (exprs : history) mempty)) es1 es2

    (Pi _ binder1 body1, []) :~: (Pi _ binder2 body2, [])
      | vis binder1 /= vis binder2 -> throwError $ UnificationFailure constraint
      | otherwise -> do
          -- !!TODO!! Block until binders are solved
          -- One possible implementation, blocked metas = set of sets where outer is conjunction and inner is disjunction
          -- BOB: this effectively blocks until the binders are solved, because we usually just try to eagerly solve problems
          blocked1 <- solveConstraint (Unify p ctx (exprs : history) mempty (binderType binder1, binderType binder2))
          blocked2 <- solveConstraint (Unify p (addBinderToCtx binder1 binder2 ctx) (exprs : history) mempty (body1, body2))
          return (blocked1 ++ blocked2)

    (Builtin _ op1, args1) :~: (Builtin _ op2, args2) -> do
      solveEq constraint op1 op2
      if length args1 /= length args2 then
        throwError $ UnificationFailure constraint
      else
        traverse (solveArg constraint) (zip args1 args2)

    (Var _ v1, args1) :~: (Var _ v2, args2) -> do
      solveEq constraint v1 v2
      if length args1 /= length args2 then
        throwError $ UnificationFailure constraint
      else
        traverse (solveArg constraint) (zip args1 args2)

    (Literal _ l1, args1) :~: (Literal _ l2, args2) -> do
      solveEq constraint l1 l2
      if length args1 /= length args2 then
        throwError $ UnificationFailure constraint
      else
        traverse (solveArg constraint) (zip args1 args2)

    ---------------------
    -- Flex-flex cases --
    ---------------------
    (Meta _ i, args1) :~: (Meta _ j, args2)
      -- If the expressions are exactly equal then simply discard the constraint
      -- as it doesn't tell us anything.
      | i == j && args1 == args2 ->
        return []

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

        return []

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

        return []

    ----------------------
    -- Flex-rigid cases --
    ----------------------

    -- ?X e1 e2 e3 =?= ?Y x y z

    -- ==> ?Y := \x. \y. \z. ?X e1 e2 e3

    (Meta _ i, args) :~: _ -> do
      -- Check that 'args' is a pattern
      case patternOfArgs args of
        Nothing ->
          -- This constraint is stuck because it is not pattern; shelve
          -- it for now and hope that another constraint allows us to
          -- progress.
          return [constraint]
        Just subst -> do
          -- 'subst' is a renaming that renames the variables in 'e2' to
          -- ones bound by the metavariable
          case substAll subst whnfE2 of
            Nothing ->
              return [constraint]
            Just defnBody -> do
              -- TODO: fail if 'Meta _ i' occurs in 'e2'
              metaSolved p i (abstractOver args defnBody)
              return []

    _t :~: (Meta _ _i, _args) ->
      -- this is the mirror image of the previous case, so just swap the
      -- problem over.
      solveConstraint (constraint { unifExprs = (whnfE2, whnfE1) })

    -- Catch-all
    _ -> throwError $ UnificationFailure (constraint { unifExprs = (whnfE1, whnfE2) })

abstractOver :: [CheckedArg] -> CheckedExpr -> CheckedExpr
abstractOver args body = foldr argToLam body args
  where
    argToLam :: CheckedArg -> CheckedExpr -> CheckedExpr
    argToLam (Arg _ v argE) = Lam mempty (Binder mempty v Machine argE)

isBlocked :: Maybe MetaSet          -- Set of metas solved in the last pass
          -> UnificationConstraint  -- Unification constraint
          -> Bool
isBlocked Nothing            _                             = False
isBlocked (Just solvedMetas) (Unify _ _ _ blockingMetas _) =
  -- A constraint is blocked if it is blocking on at least one meta
  -- and none of the metas it is blocking on have been solved in the last pass.
  not (IntSet.null blockingMetas) && IntSet.disjoint solvedMetas blockingMetas

solveEq :: (MonadUnify m, Eq a)
        => UnificationConstraint
        -> a
        -> a
        -> m ()
solveEq c v1 v2
  | v1 /= v2  = throwError $ UnificationFailure c
  | otherwise = do
    logDebug "solved-trivially"
    return ()

solveArg :: MonadUnify m
         => UnificationConstraint
         -> (CheckedArg, CheckedArg)
         -> m UnificationConstraint
solveArg c@(Unify p ctx history _metas es) (arg1, arg2)
  | vis arg1 /= vis arg2 = throwError $ UnificationFailure c
  | otherwise = return $ Unify p ctx (es : history) mempty (argExpr arg1 , argExpr arg2)

positionalIntersection :: Eq a => [a] -> [a] -> [a]
positionalIntersection [] _       = []
positionalIntersection _ []       = []
positionalIntersection (x : xs) (y : ys)
 | x == y    = x : positionalIntersection xs ys
 | otherwise = positionalIntersection xs ys

metaSolved :: MonadUnifyPass m
           => Provenance
           -> Meta
           -> CheckedExpr
           -> m ()
metaSolved p m e = do
  tell (IntSet.singleton m)
  Meta.metaSolved p m e