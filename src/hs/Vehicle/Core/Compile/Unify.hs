
module Vehicle.Core.Compile.Unify
  ( UnificationConstraint(..)
  , UnificationError(..)
  , TypeClassConstraint(..)
  , solve
  , makeConstraint
  ) where

import Control.Monad.Except (MonadError(..), Except, throwError)
import Control.Monad.Writer (MonadWriter(..), runWriterT)
import Control.Monad.State  (MonadState(..), modify, runStateT)
import Control.Monad.Reader (MonadReader(..), runReaderT)
import Control.Monad (foldM)
import Data.IntMap qualified as IntMap
import Data.IntSet qualified as IntSet
import Prettyprinter ( (<+>), Pretty(pretty), Doc )

import Vehicle.Prelude
import Vehicle.Core.Print.Core ()
import Vehicle.Core.Print.Frontend (prettyFrontend)
import Vehicle.Core.AST
import Vehicle.Core.Compile.DSL (piType)
import Vehicle.Core.Compile.Normalise (whnf)

data TypeClassConstraint = Meta `Has` CheckedExpr
  deriving (Show)

instance Pretty TypeClassConstraint where
  pretty (m `Has` e) = "?" <> pretty m <+> "~" <+> pretty e

instance MetaSubstitutable TypeClassConstraint where
  substM (m `Has` e) = (m `Has`) <$> substM e

--------------------------------------------------------------------------------
-- Definitions

-- The context in which a unification occurs.
data UnificationContext = UnificationContext
  { sharedContext :: [Name]        -- The context before any unification starts
  , splitContext :: [(Name, Name)] -- The context after unification.
  } deriving (Show)

addToCtx :: CheckedBinder -> CheckedBinder -> UnificationContext -> UnificationContext
addToCtx b1 b2 UnificationContext{..} = UnificationContext
  { sharedContext = sharedContext
  , splitContext = (binderName b1, binderName b2) : splitContext
  }

-- | A pair of expressions should be equal
type UnificationPair = (CheckedExpr, CheckedExpr)

-- | A sequence of attempts at unification
type UnificationHistory = [UnificationPair]

-- | Represents that the two contained expressions should be equal.
data UnificationConstraint
  = Unify
    { unifyProv     :: Provenance         -- The location of the code that gave rise to the constraint
    , unifyCtxt     :: UnificationContext -- The context of the constraint is being solved in
    , unifHistory   :: UnificationHistory -- The history, i.e. unification path that has lead to this.
    , unifBlockedOn :: MetaSet            -- The meta-variables that the constraint is blocked on
    , unifExprs     :: UnificationPair    -- The expressions to unify
    }
  deriving Show

instance HasProvenance UnificationConstraint where
  prov (Unify p _ _ _ _) = p

instance Pretty UnificationConstraint where
  pretty (Unify _ _ _ metas (e1, e2)) =
    pretty metas <+> prettyFrontend e1 <+> "~" <+> prettyFrontend e2

instance MetaSubstitutable UnificationConstraint where
  substM (Unify p ctx metas history es) =
    Unify p ctx metas history <$> substM es

makeConstraint :: Provenance -> [(Name, CheckedExpr)] -> CheckedExpr -> CheckedExpr -> UnificationConstraint
makeConstraint p ctx e1 e2 = Unify p (UnificationContext (map fst ctx) []) [] mempty (e1, e2)

--------------------------------------------------------------------------------
-- Error handling

-- | Errors thrown during unification
newtype UnificationError
  = UnificationFailure UnificationConstraint

instance MeaningfulError UnificationError where
  details (UnificationFailure constraint) = UError $ UserError
    { provenance = prov constraint
    , problem    = "Could not solve the constraint:" <+> pretty constraint
    , fix        = "Try adding more type annotations"
    }

unexpectedCase :: Provenance -> Doc ann -> a
unexpectedCase p expr = developerError $
  expr <+> "should not exist during unification" <+> pretty p

unexpectedMeta :: Provenance -> Meta -> CheckedExpr -> CheckedExpr -> a
unexpectedMeta p i new old = developerError $
  "meta-variable" <+> pretty i <+> "already assigned" <+> pretty old <+>
  "and should have been substituted out but it is still present and" <+>
  "was assigned again to" <+> pretty new <+>
  pretty p

--------------------------------------------------------------------------------
-- Unification algorithm

type MonadUnify m =
  -- The error thrown
  ( MonadError UnificationError m
  -- The current meta substitution
  , MonadState MetaSubstitution m
  -- The list of metas that we have made progess on so far in this pass
  , MonadWriter MetaSet m
  -- The list of metas that were made progress on in the previous pass.
  -- None represents that we don't know which metas were made progress on
  -- in the previous pass.
  , MonadReader (Maybe MetaSet) m
  )

type UnificationProblem = ([UnificationConstraint], MetaSubstitution)

isBlocked :: Maybe MetaSet          -- Set of metas solved in the last pass
          -> UnificationConstraint  -- Unification constraint
          -> Bool
isBlocked Nothing            _                             = False
isBlocked (Just solvedMetas) (Unify _ _ _ blockingMetas _) =
  -- A constraint is blocked if it is blocking on at least one meta
  -- and none of the metas it is blocking on have been solved in the last pass.
  not (IntSet.null blockingMetas) && IntSet.disjoint solvedMetas blockingMetas

solve :: UnificationProblem -> Except UnificationError UnificationProblem
solve problem = fst <$> go (problem, Nothing)
  where
    go :: (UnificationProblem, Maybe MetaSet)
        -> Except UnificationError (UnificationProblem, Maybe MetaSet)
    -- Exit if we have solved all constraints
    go input@(([], _), _) =
      return input

    -- Exit if we have not succeeded in solving any metas in the last pass
    go input@((_, _), Just metasSolvedLastPass)
      | IntSet.null metasSolvedLastPass = return input

    -- Otherwise make another pass
    go ((constraints, metaSubst), metasSolvedLastPass) = do
      let z = foldM solvePass [] constraints
      let y = runStateT z metaSubst
      let x = runReaderT y metasSolvedLastPass
      let w = runWriterT x
      ((unsolvedConstraints, newMetaSubst), metasSolved) <- w
      go ((unsolvedConstraints, newMetaSubst), Just metasSolved)


solvePass :: MonadUnify m
          => [UnificationConstraint]
          -> UnificationConstraint
          -> m [UnificationConstraint]
solvePass queue constraint = do
  progressedMetas <- ask
  if isBlocked progressedMetas constraint
    then return (constraint : queue)
    else do
      subst <- get
      let updatedConstraint = substMetas subst constraint
      newConstraints <- solveConstraint updatedConstraint
      return (newConstraints <> queue)

pattern (:~:) :: a -> b -> (a, b)
pattern x :~: y = (x,y)

solveConstraint :: MonadUnify m => UnificationConstraint -> m [UnificationConstraint]
-- Errors
solveConstraint constraint@(Unify p ctx history _ exprs@(e1, e2)) = do
  whnfE1 <- whnf e1
  whnfE2 <- whnf e2
  -- trace (layoutAsString $ "UNIFY: " <+> pretty whnfE1 <+> " ~ " <+> pretty whnfE2) $ return ()
  case (decomposeApp whnfE1, decomposeApp whnfE2) of
    (Let{}, _) :~: _           -> unexpectedCase p "Let bindings"
    _          :~: (Let{}, _)  -> unexpectedCase p "Let bindings"

    (Hole{}, _) :~: _           -> unexpectedCase p "Holes"
    _           :~: (Hole{}, _) -> unexpectedCase p "Holes"

    -- Rigid-rigid
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
      | visibility binder1 /= visibility binder2 -> throwError $ UnificationFailure constraint
      | otherwise -> return
        [ Unify p (addToCtx binder1 binder2 ctx) (exprs : history) mempty (body1, body2) ]

    (Seq _ es1, []) :~: (Seq _ es2, [])
      -- TODO more informative error message
      | length es1 /= length es2 -> throwError $ UnificationFailure constraint
      -- TODO need to try and unify `Seq` with `Cons`s.
      | otherwise                -> return $
                                    zipWith (curry (Unify p ctx (exprs : history) mempty)) es1 es2

    (Pi _ binder1 body1, []) :~: (Pi _ binder2 body2, [])
      | visibility binder1 /= visibility binder2 -> throwError $ UnificationFailure constraint
      | otherwise -> do
          -- !!TODO!! Block until binders are solved
          -- One possible implementation, blocked metas = set of sets where outer is conjunction and inner is disjunction
          -- BOB: this effectively blocks until the binders are solved, because we usually just try to eagerly solve problems
          blocked1 <- solveConstraint (Unify p ctx (exprs : history) mempty (binderType binder1, binderType binder2))
          blocked2 <- solveConstraint (Unify p (addToCtx binder1 binder2 ctx) (exprs : history) mempty (body1, body2))
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

    -- Simple flex-flex case, when one meta-variable has no arguments you can simply
    -- use the other meta-variable as its definition.
    (Meta ann i, []) :~: (Meta _ _j, _args) ->
      metaSolved (p <> prov ann) i whnfE2

    -- TODO can this case be absorbed into a general symmetry case as it should already
    -- be covered by the case above
    (Meta _ _i, _args) :~: (Meta ann j, []) ->
      metaSolved (p <> prov ann) j whnfE1

    (Meta _ _i, _args1) :~: (Meta _ _j, _args2) ->
      -- TODO: flex-flex unification:
      --  we could try to solve by trying each direction as a definition, if the metavariables are not equal.
      --  if the heads are equal, then succeed if the argument lists are identical, otherwise postpone
      throwError $ UnificationFailure constraint

    -- ?X e1 e2 e3 =?= ?Y x y z

    -- ==> ?Y := \x. \y. \z. ?X e1 e2 e3

    (Meta _ i, args) :~: _ ->
      -- Check that 'args' is a pattern
      case patternOfArgs args of
        Nothing ->
          -- This constraint is stuck because it is not pattern; shelve
          -- it for now and hope that another constraint allows us to
          -- progress.
          return [constraint]
        Just subst ->
          -- 'subst' is a renaming that renames the variables in 'e2' to
          -- ones bound by the metavariable
          case substAll subst e2 of
            Nothing ->
              return [constraint]
            Just defnBody ->
              -- TODO: fail if 'Meta _ i' occurs in 'e2'
              let{-
                foldFn :: CheckedArg -> DSLExpr -> DSLExpr
                foldFn (Arg argP argV argE) body =
                  -- TODO: 'getType argE' needs to be renamed
                  -- to be in the implicit context being
                  -- generated here.
                  lam argP argV Machine (toDSL $ getType argE) (const body) -}

                -- BUG: using 'DSL' here doesn't work because it cannot handle open terms
                solution = -- fromDSL (foldr foldFn (toDSL defnBody) args)
                    foldr (\(Arg _ v argE) body ->
                             let argType = {- fromJust $ substAll subst -} (getType argE)
                                 bodyType = getType body
                             -- TODO: 'getType argE' needs to be
                             -- renamed to be in the implicit context
                             -- being generated here.
                                 ann = {- trace ("Type: " ++ show (getType argE) ++ " : " ++ show (getType (getType argE)) ++ " --> " ++ show argType ++ " : " ++ show (getType argType)) $ -} RecAnn (piType (getType argType) (getType bodyType)) mempty
                             in
                             Lam (RecAnn (Pi ann (Binder mempty v Machine argType) bodyType) mempty)
                                 (Binder mempty v Machine argType) body)
                      defnBody
                      args
              in
              metaSolved p i solution

    _t :~: (Meta _ _i, _args) ->
      -- this is the mirror image of the previous case, so just swap the
      -- problem over.
      solveConstraint (constraint { unifExprs = (whnfE2, whnfE1) })

    -- Catch-all
    _ -> throwError $ UnificationFailure (constraint { unifExprs = (whnfE1, whnfE2) })

metaSolved :: MonadUnify m
           => Provenance
           -> Meta
           -> CheckedExpr
           -> m [UnificationConstraint]
metaSolved p m e = do
  -- trace (layoutAsString $ "solving " <+> pretty m <+> " as " <+> pretty e) $ return ()

  -- Update the substitution,
  modify (\subst -> let
    -- Insert the new variable throwing an error if the meta-variable is already present
    -- (should have been substituted out)
    subst' = IntMap.insertWith (unexpectedMeta p m) m e subst
    -- Substitute into the existing subsitutions
    subst'' = IntMap.map (substMeta m e) subst'
    in subst'')



  -- Insert the meta-variable into the list of metas solved in this pass
  tell $ IntSet.singleton m

  -- Return an empty list of constraints
  return []

solveEq :: (MonadUnify m, Eq a)
        => UnificationConstraint
        -> a
        -> a
        -> m ()
solveEq c v1 v2
  | v1 /= v2  = throwError $ UnificationFailure c
  | otherwise = return ()

solveArg :: MonadUnify m
         => UnificationConstraint
         -> (CheckedArg, CheckedArg)
         -> m UnificationConstraint
solveArg c@(Unify p ctx history _metas es) (arg1, arg2)
  | visibility arg1 /= visibility arg2 = throwError $ UnificationFailure c
  | otherwise = return $ Unify p ctx (es : history) mempty (argExpr arg1 , argExpr arg2)

decomposeApp :: CheckedExpr -> (CheckedExpr, [CheckedArg])
decomposeApp = go []
  where go args (App _ann fun arg) = go (arg:args) fun
        go args e                  = (e, args)
