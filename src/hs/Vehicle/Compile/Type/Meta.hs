
module Vehicle.Compile.Type.Meta
  ( MetaSet
  , MetaSubstitution
  , MetaSubstitutable(..)
  , freshMetaWith
  , metaSolved
  , makeMetaType
  , addUnificationConstraint
  , addTypeClassConstraint
  , addConstraints
  , setConstraints
  , getConstraints
  , getTypeClassConstraints
  , popActivatedConstraints
  , getMetaSubstitution
  , modifyMetaSubstitution
  , numberOfMetasCreated
  , getMetaOrigin
  , MonadConstraintSolving
  , ConstraintProgress(..)
  , nonTriviallySolved
  , triviallySolved
  , partiallySolved
  , isStuck
  , MetaCtx(..)
  , emptyMetaCtx
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.Reader (Reader, runReader, ask, local)
import Control.Monad.State (MonadState(..), modify, gets)
import Data.List (partition)
import Data.Maybe (mapMaybe)

import Vehicle.Language.Print (prettyVerbose)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Type.MetaSubstitution ( MetaSubstitution )
import Vehicle.Compile.Type.MetaSubstitution qualified as MetaSubst (map, lookup, insertWith)
import Vehicle.Compile.Type.MetaSet (MetaSet)
import Vehicle.Compile.Type.MetaSet qualified as MetaSet (singleton, disjoint, null)
import Vehicle.Compile.Type.Constraint

--------------------------------------------------------------------------------
-- The meta context

-- | The meta-variables and constraints relating the variables currently in scope.
data MetaCtx = MetaCtx
  { metaOrigins         :: [Provenance]
  -- ^ The origin of each meta variable.
  -- NB: these are stored in *reverse* order from which they were created.
  , currentSubstitution :: MetaSubstitution
  , constraints         :: [Constraint]
  }

emptyMetaCtx :: MetaCtx
emptyMetaCtx = MetaCtx
  { metaOrigins            = mempty
  , currentSubstitution    = mempty
  , constraints            = mempty
  }

--------------------------------------------------------------------------------
-- Meta substitutions

liftSubstitution :: MetaSubstitution -> MetaSubstitution
liftSubstitution = MetaSubst.map (liftFreeDBIndices 1)

class MetaSubstitutable a where
  -- TODO change name away from M
  substM :: a -> Reader MetaSubstitution a

  substMetas :: MetaSubstitution -> a -> a
  substMetas s e = runReader (substM e) s

instance MetaSubstitutable a => MetaSubstitutable (a, a) where
  substM (e1, e2) = do
    e1' <- substM e1
    e2' <- substM e2
    return (e1', e2')

instance MetaSubstitutable a => MetaSubstitutable [a] where
  substM = traverse substM

instance MetaSubstitutable CheckedArg where
  substM = traverseArgExpr substM

instance MetaSubstitutable CheckedBinder where
  substM = traverseBinderType substM

instance MetaSubstitutable CheckedExpr where
  substM = \case
    Type     ann l            -> return $ Type    ann l
    Hole     ann name         -> return $ Hole    ann name
    Builtin  ann op           -> return $ Builtin ann op
    Literal  ann l            -> return $ Literal ann l
    Var      ann v            -> return $ Var     ann v
    LSeq     ann dict es      -> LSeq     ann <$> substM dict   <*> traverse substM es
    Ann      ann term typ     -> Ann      ann <$> substM term   <*> substM typ
    Pi       ann binder res   -> Pi       ann <$> substM binder <*> local liftSubstitution (substM res)
    Let      ann e1 binder e2 -> Let      ann <$> substM e1     <*> substM binder <*> local liftSubstitution (substM e2)
    Lam      ann binder e     -> Lam      ann <$> substM binder <*> local liftSubstitution (substM e)
    PrimDict ann tc           -> PrimDict ann <$> substM tc

    e@(Meta ann _)  -> substMApp ann (e, [])
    e@(App ann _ _) -> substMApp ann (toHead e)


-- | We really don't want un-normalised lambda applications from solved meta-variables
-- clogging up our program so this function detects meta applications and normalises
-- them as it substitutes the meta in.
substMApp :: CheckedAnn -> (CheckedExpr, [CheckedArg]) -> Reader MetaSubstitution CheckedExpr
substMApp ann (fun@(Meta _ m), mArgs) = do
  subst <- ask
  case MetaSubst.lookup m subst of
    Just eRes -> substM $ substMArgs eRes mArgs
    Nothing   -> normAppList ann fun <$> substM mArgs
  where
    substMArgs :: CheckedExpr -> [CheckedArg] -> CheckedExpr
    substMArgs (Lam _ _ body) (arg : args) = substMArgs (argExpr arg `substInto` body) args
    substMArgs Lam{}          []           = developerError "Meta variable does not appear to be applied to every variable in the context"
    substMArgs e              args         = normAppList ann e args
substMApp ann (fun, args) = normAppList ann <$> substM fun <*> substM args

instance MetaSubstitutable CheckedDecl where
  substM = traverseDeclExprs substM

instance MetaSubstitutable CheckedProg where
  substM (Main ds) = Main <$> traverse substM ds

instance MetaSubstitutable TypeClassConstraint where
  substM (m `Has` e) = (m `Has`) <$> substM e

instance MetaSubstitutable UnificationConstraint where
  substM (Unify es) = Unify <$> substM es

instance MetaSubstitutable Constraint where
  substM (UC ctx c) = UC ctx <$> substM c
  substM (TC ctx c) = TC ctx <$> substM c

--------------------------------------------------------------------------------
-- Meta-variables

freshMetaName :: MonadState MetaCtx m => Provenance -> m Meta
freshMetaName origin = do
  MetaCtx {..} <- get
  let nextMeta = length metaOrigins
  put $ MetaCtx { metaOrigins = origin : metaOrigins, .. }
  return (MetaVar nextMeta)

-- | Creates a fresh meta variable. Meta variables need to remember what was
-- in the current context when they were created. We do this by creating a
-- meta-variable that takes everything in the current context as an argument
-- and then which is immediately applied to everything in the current context.
-- Post unification, any unneeded context arguments will be normalised away.
-- It returns the name of the meta and the expression of it applied to every
-- variable in the context.
freshMetaWith :: (MonadState MetaCtx m, MonadLogger m)
              => Provenance
              -> BoundCtx
              -> m (Meta, CheckedExpr)
freshMetaWith p boundCtx = do
  -- Create a fresh name
  metaName <- freshMetaName p

  -- Create bound variables for everything in the context
  let ann = inserted p
  let boundEnv = reverse [ Var ann (Bound i) | i <- [0..length boundCtx - 1] ]

  -- Returns a meta applied to every bound variable in the context
  let meta = normAppList ann (Meta ann metaName) (map (ExplicitArg ann) boundEnv)

  logDebug MaxDetail $ "fresh-meta" <+> pretty metaName
  return (metaName, meta)

-- |Creates a Pi type that abstracts over all bound variables
makeMetaType :: BoundCtx
             -> CheckedAnn
             -> CheckedExpr
             -> CheckedExpr
makeMetaType boundCtx ann resultType = foldr entryToPi resultType (reverse boundCtx)
  where
    entryToPi :: (DBBinding, CheckedExpr, Maybe CheckedExpr) -> CheckedExpr -> CheckedExpr
    entryToPi (name, t, _) = Pi ann (ExplicitBinder ann name t)

getMetaOrigin :: MonadState MetaCtx m => Meta -> m Provenance
getMetaOrigin (MetaVar m) = do
  MetaCtx {..} <- get
  return $ metaOrigins !! (length metaOrigins - m - 1)

getMetaSubstitution :: MonadState MetaCtx m => m MetaSubstitution
getMetaSubstitution = gets currentSubstitution

modifyMetaSubstitution :: MonadState MetaCtx m => (MetaSubstitution -> MetaSubstitution) -> m ()
modifyMetaSubstitution f = modifyMetaCtx $ \ MetaCtx {..} ->
  MetaCtx { currentSubstitution = f currentSubstitution, ..}

modifyMetaCtx :: MonadState MetaCtx m => (MetaCtx -> MetaCtx) -> m ()
modifyMetaCtx = modify

getConstraints :: MonadState MetaCtx m => m [Constraint]
getConstraints = gets constraints

getTypeClassConstraints :: MonadState MetaCtx m => m [(TypeClassConstraint, ConstraintContext)]
getTypeClassConstraints = mapMaybe getTypeClassConstraint <$> getConstraints

numberOfMetasCreated :: MonadState MetaCtx m => m Int
numberOfMetasCreated = gets (length . metaOrigins)

metaSolved :: (MonadState MetaCtx m, MonadLogger m)
           => Provenance
           -> Meta
           -> CheckedExpr
           -> m ()
metaSolved p m e = do
  logDebug MaxDetail $ "solved" <+> pretty m <+> "as" <+> prettyVerbose e

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

--------------------------------------------------------------------------------
-- Constraints

type MonadConstraintSolving m =
  ( MonadState MetaCtx m
  , MonadLogger m
  , MonadError CompileError m
  )

addUnificationConstraint :: (MonadState MetaCtx m, MonadLogger m)
                         => Provenance
                         -> VariableCtx
                         -> CheckedExpr
                         -> CheckedExpr
                         -> m ()
addUnificationConstraint p ctx e1 e2 = do
  let context    = ConstraintContext p mempty ctx
  let constraint = UC context $ Unify (e1, e2)
  addConstraints [constraint]

addTypeClassConstraint :: (MonadState MetaCtx m, MonadLogger m)
                       => VariableCtx
                       -> Meta
                       -> CheckedExpr
                       -> m ()
addTypeClassConstraint ctx meta expr = do
  let context    = ConstraintContext (provenanceOf expr) mempty ctx
  let constraint = TC context (meta `Has` expr)
  addConstraints [constraint]

addConstraints :: (MonadState MetaCtx m, MonadLogger m) => [Constraint] -> m ()
addConstraints []             = return ()
addConstraints newConstraints = do
  logDebug MaxDetail ("add-constraints " <> align (prettyVerbose newConstraints))
  modifyMetaCtx $ \ MetaCtx {..} ->
    MetaCtx { constraints = constraints ++ newConstraints, ..}

setConstraints :: (MonadState MetaCtx m) => [Constraint] -> m ()
setConstraints newConstraints = modifyMetaCtx $ \MetaCtx{..} ->
    MetaCtx { constraints = newConstraints, ..}

-- | Returns any constraints that are activated (i.e. worth retrying) based
-- on the set of metas that were solved last pass.
popActivatedConstraints :: MonadState MetaCtx m
                        => MetaSet
                        -> m [Constraint]
popActivatedConstraints metasSolved = do
  allConstraints <- getConstraints
  let (blockedConstraints, unblockedConstraints) = partition (isBlocked metasSolved) allConstraints
  setConstraints blockedConstraints
  return unblockedConstraints
  where
    isBlocked :: MetaSet -> Constraint -> Bool
    isBlocked solvedMetas constraint =
      let blockingMetas = blockedBy $ constraintContext constraint in
      -- A constraint is blocked if it is blocking on at least one meta
      -- and none of the metas it is blocking on have been solved in the last pass.
      not (MetaSet.null blockingMetas) && MetaSet.disjoint solvedMetas blockingMetas

--------------------------------------------------------------------------------
-- Progress in solving meta-variable constraints

-- | Reports progress when trying to solve meta-variable constraints
-- Progress may be made if
--  a) constraints are solved (may or may not result in metas being solved)
--  b) new constraints are added
data ConstraintProgress
  = Stuck
  | Progress
    { newConstraints  :: [Constraint]
    , solvedMetas     :: MetaSet
    }
  deriving (Show)

instance Pretty ConstraintProgress where
  pretty Stuck                        = "Stuck"
  pretty (Progress constraints metas) =
    "Progress" <+> prettyVerbose constraints <+> pretty metas

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

