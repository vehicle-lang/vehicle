
module Vehicle.Core.Compile.Type.Meta
  ( MetaSet
  , MetaSubstitution
  , MetaSubstitutable(..)
  , MonadMeta
  , freshMetaWith
  , metaSolved
  , makeMetaType
  , addUnificationConstraint
  , addTypeClassConstraint
  , addConstraints
  , setConstraints
  , getConstraints
  , popActivatedConstraints
  , getMetaSubstitution
  , modifyMetaSubstitution
  , MonadConstraintSolving
  , ConstraintProgress(..)
  , nonTriviallySolved
  , triviallySolved
  , partiallySolved
  , isStuck
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.Reader (Reader, runReader, ask, local)
import Control.Monad.State (MonadState(..), modify, gets)
import Data.List (partition)
import Debug.Trace (traceShow)

import Vehicle.Prelude
import Vehicle.Core.AST
import Vehicle.Core.Compile.Type.Core
import Vehicle.Core.Print (prettyVerbose)
import Vehicle.Core.MetaSubstitution ( MetaSubstitution )
import Vehicle.Core.MetaSubstitution qualified as MetaSubst (singleton, map, lookup, insertWith)
import Vehicle.Core.MetaSet (MetaSet)
import Vehicle.Core.MetaSet qualified as MetaSet (singleton, disjoint, null)

class MetaSubstitutable a where
  -- TODO change name away from M
  substM :: a -> Reader MetaSubstitution a

  substMetas :: MetaSubstitution -> a -> a
  substMetas s e = runReader (substM e) s

  substMeta :: Meta -> CheckedExpr -> a -> a
  substMeta m e = substMetas (MetaSubst.singleton m e)

  substMetasLiftLocal :: a -> Reader MetaSubstitution a
  substMetasLiftLocal e = local (MetaSubst.map (liftDBIndices 1)) (substM e)

instance MetaSubstitutable a => MetaSubstitutable (a, a) where
  substM (e1, e2) = do
    e1' <- substM e1
    e2' <- substM e2
    return (e1', e2')

instance MetaSubstitutable a => MetaSubstitutable [a] where
  substM = traverse substM

instance MetaSubstitutable CheckedArg where
  substM (Arg p v e) = Arg p v <$> substM e

instance MetaSubstitutable CheckedBinder where
  substM (Binder p v n t) = Binder p v n <$> substM t

instance MetaSubstitutable CheckedExpr where
  substM = \case
    Type l                   -> return (Type l)
    Hole p name              -> return (Hole p name)
    Builtin ann op           -> return (Builtin ann op)
    Literal ann l            -> return (Literal ann l)
    Var     ann v            -> return (Var     ann v)
    Seq     ann es           -> Seq     ann <$> traverse substM es
    Ann     ann term typ     -> Ann     ann <$> substM term   <*> substM typ
    Pi      ann binder res   -> Pi      ann <$> substM binder <*> substMetasLiftLocal res
    Let     ann e1 binder e2 -> Let     ann <$> substM e1     <*> substM binder <*> substMetasLiftLocal e2
    Lam     ann binder e     -> Lam     ann <$> substM binder <*> substMetasLiftLocal e
    PrimDict tc              -> PrimDict <$> substM tc

    e@(Meta ann _)  -> substMApp ann (e, [])
    e@(App ann _ _) -> substMApp ann (toHead e)

-- | We really don't want un-normalised lambda applications from solved meta-variables
-- clogging up our program so this function detects meta applications and normalises
-- them as it substitutes the meta in.
substMApp :: CheckedAnn -> (CheckedExpr, [CheckedArg]) -> Reader MetaSubstitution CheckedExpr
substMApp ann (fun@(Meta _ m), mArgs) = do
  subst <- ask
  case MetaSubst.lookup m subst of
    Just eRes -> traceShow
                  (layoutAsString $ prettyVerbose eRes <+> prettyVerbose mArgs)
                  (substM $ substMArgs eRes mArgs)
    Nothing   -> normAppList ann fun <$> substM mArgs
  where
    substMArgs :: CheckedExpr -> [CheckedArg] -> CheckedExpr
    substMArgs (Lam _ _ body) (arg : args) = substMArgs (argExpr arg `substInto` body) args
    substMArgs Lam{}          []           = developerError "Meta variable does not appear to be applied to every variable in the context"
    substMArgs e              args         = normAppList ann e args
substMApp ann (fun, args) = normAppList ann <$> substM fun <*> substM args

instance MetaSubstitutable CheckedDecl where
  substM = \case
    DeclNetw p ident t   -> DeclNetw p ident <$> substM t
    DeclData p ident t   -> DeclData p ident <$> substM t
    DefFun   p ident t e -> DefFun   p ident <$> substM t <*> substM e

instance MetaSubstitutable CheckedProg where
  substM (Main ds) = Main <$> traverse substM ds

instance MetaSubstitutable BaseConstraint where
  substM (Unify es)  = Unify <$> substM es
  substM (m `Has` e) = (m `Has`) <$> substM e

instance MetaSubstitutable Constraint where
  substM (Constraint ctx c) = Constraint ctx <$> substM c

--------------------------------------------------------------------------------
-- The meta monad

type MonadMeta m = MonadState MetaCtx m

modifyMetaCtx :: MonadMeta m => (MetaCtx -> MetaCtx) -> m ()
modifyMetaCtx = modify

getConstraints :: MonadMeta m => m [Constraint]
getConstraints = gets constraints

addUnificationConstraint :: (MonadMeta m, MonadLogger m)
                         => Provenance
                         -> VariableCtx
                         -> CheckedExpr
                         -> CheckedExpr
                         -> m ()
addUnificationConstraint p ctx e1 e2 = do
  let context    = ConstraintContext p mempty ctx
  let constraint = Constraint context $ Unify (e1, e2)
  addConstraints [constraint]

addTypeClassConstraint :: (MonadMeta m, MonadLogger m)
                       => VariableCtx
                       -> Meta
                       -> CheckedExpr
                       -> m ()
addTypeClassConstraint ctx meta expr = do
  let context    = ConstraintContext (prov expr) mempty ctx
  let constraint = Constraint context (meta `Has` expr)
  addConstraints [constraint]

addConstraints :: (MonadMeta m, MonadLogger m) => [Constraint] -> m ()
addConstraints []             = return ()
addConstraints newConstraints = do
  logDebug ("add-constraints " <> align (prettyVerbose newConstraints))
  modifyMetaCtx $ \ MetaCtx {..} ->
    MetaCtx { constraints = constraints ++ newConstraints, ..}

setConstraints :: (MonadMeta m) => [Constraint] -> m ()
setConstraints newConstraints = modifyMetaCtx $ \MetaCtx{..} ->
    MetaCtx { constraints = newConstraints, ..}

getMetaSubstitution :: MonadMeta m => m MetaSubstitution
getMetaSubstitution = gets currentSubstitution

modifyMetaSubstitution :: MonadMeta m => (MetaSubstitution -> MetaSubstitution) -> m ()
modifyMetaSubstitution f = modifyMetaCtx $ \ MetaCtx {..} ->
  MetaCtx { currentSubstitution = f currentSubstitution, ..}

freshMetaName :: MonadMeta m => m Meta
freshMetaName = do
  MetaCtx {..} <- get;
  put $ MetaCtx { nextMeta = succ nextMeta , ..}
  return (MetaVar nextMeta)

-- | Creates a fresh meta variable. Meta variables need to remember what was
-- in the current context when they were created. We do this by creating a
-- meta-variable that takes everything in the current context as an argument
-- and then which is immediately applied to everything in the current context.
-- Post unification, any unneeded context arguments will be normalised away.
-- It returns the name of the meta and the expression of it applied to every
-- variable in the context.
freshMetaWith :: (MonadMeta m, MonadLogger m)
              => BoundCtx
              -> Provenance
              -> m (Meta, CheckedExpr)
freshMetaWith boundCtx p = do
  -- Create a fresh name
  metaName <- freshMetaName

  -- Create bound variables for everything in the context
  let boundEnv = reverse [ Var p (Bound varIndex) | varIndex <- [0..length boundCtx - 1] ]

  -- Returns a meta applied to every bound variable in the context
  let meta = normAppList p (Meta p metaName) (map (Arg p Explicit) boundEnv)

  logDebug $ "fresh-meta" <+> pretty metaName
  return (metaName, meta)

-- |Creates a Pi type that abstracts over all bound variables
makeMetaType :: BoundCtx
             -> Provenance
             -> CheckedExpr
             -> CheckedExpr
makeMetaType boundCtx p resultType = foldr entryToPi resultType (reverse boundCtx)
  where
    entryToPi :: (Name, CheckedExpr) -> CheckedExpr -> CheckedExpr
    entryToPi (name, t) resTy = Pi p (Binder p Explicit name t) resTy

-- | Returns any constraints that are activated (i.e. worth retrying) based
-- on the set of metas that were solved last pass.
popActivatedConstraints :: MonadMeta m
                        => MetaSet
                        -> m [Constraint]
popActivatedConstraints metasSolved = do
  allConstraints <- getConstraints
  let (blockedConstraints, unblockedConstraints) = partition (isBlocked metasSolved) allConstraints
  setConstraints blockedConstraints
  return unblockedConstraints
  where
    isBlocked :: MetaSet -> Constraint -> Bool
    isBlocked solvedMetas (Constraint (ConstraintContext _ blockingMetas _) _) =
      -- A constraint is blocked if it is blocking on at least one meta
      -- and none of the metas it is blocking on have been solved in the last pass.
      not (MetaSet.null blockingMetas) && MetaSet.disjoint solvedMetas blockingMetas


metaSolved :: (MonadMeta m, MonadLogger m)
           => Provenance
           -> Meta
           -> CheckedExpr
           -> m ()
metaSolved p m e = do
  logDebug $ "solved" <+> pretty m <+> "as" <+> prettyVerbose e

  -- Insert the new variable throwing an error if the meta-variable is already present
  -- (should have been substituted out)
  modifyMetaSubstitution (MetaSubst.insertWith duplicateMetaError m e)
  where
    duplicateMetaError :: CheckedExpr -> CheckedExpr -> a
    duplicateMetaError new old = developerError $
      "meta-variable" <+> pretty m <+> "already assigned" <+> prettyVerbose old <+>
      "and should have been substituted out but it is still present and" <+>
      "was assigned again to" <+> prettyVerbose new <+>
      pretty p

type MonadConstraintSolving m =
  ( MonadMeta m
  , MonadLogger m
  , MonadError TypingError m
  )

--------------------------------------------------------------------------------
-- Progress in solving meta-variable constraints

-- | Reports progress when trying to solve meta-variable constraints
data ConstraintProgress
  = Stuck
  | Progress
    { newConstraints :: [Constraint]
    , solvedMetas    :: MetaSet
    }

instance Pretty ConstraintProgress where
  pretty Stuck                        = "Stuck"
  pretty (Progress constraints metas) =
    "Progress" <+> prettyVerbose constraints <+> prettyVerbose metas

isStuck :: ConstraintProgress -> Bool
isStuck Stuck = True
isStuck _     = False

instance Semigroup ConstraintProgress where
  Stuck <> x = x
  x <> Stuck = x
  Progress n1 ms1 <> Progress n2 ms2 = Progress (n1 <> n2) (ms1 <> ms2)

instance Monoid ConstraintProgress where
  mempty = Stuck

nonTriviallySolved :: Meta -> ConstraintProgress
nonTriviallySolved m = Progress mempty (MetaSet.singleton m)

triviallySolved :: ConstraintProgress
triviallySolved = Progress mempty mempty

partiallySolved :: [Constraint] -> ConstraintProgress
partiallySolved constraints = Progress constraints mempty