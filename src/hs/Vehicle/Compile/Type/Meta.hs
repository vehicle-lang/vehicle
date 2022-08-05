
module Vehicle.Compile.Type.Meta
  ( MetaSet
  , freshUniverseLevelMeta
  , freshLinearityMeta
  , freshPolarityMeta
  , freshExprMeta
  , freshTypeClassPlacementMeta
  , metaSolved
  , makeMetaType
  , addUnificationConstraint
  , addTypeClassConstraint
  , addConstraints
  , setConstraints
  , getUnsolvedConstraints
  , getTypeClassConstraints
  , popActivatedConstraints
  , getMetaSubstitution
  , getSolvedMetas
  , clearMetaSubstitution
  , clearSolvedMetas
  , substMetasThroughCtx
  , getUnsolvedMetas
  , getUnsolvedAuxiliaryMetas
  , getMetaInfo
  , getMetaProvenance
  , getMetaType
  , getMetaContext
  , modifyMetasInfo
  , getMetasLinkedToMetasIn
  , MonadMeta
  , ConstraintProgress(..)
  , isStuck
  , MetaCtx(..)
  , MetaInfo(..)
  , emptyMetaCtx
  , HasMetas(..)
  , substMetas
  , prettyMeta
  , prettyMetas
  , getDeclType
  , clearMetaCtx
  , MetaSubstitution
  , MetaSubstitutable(..)
  ) where

import Control.Monad.Reader (ReaderT (..), MonadReader (..), local)
import Control.Monad.State (MonadState(..), modify, gets)
import Control.Monad (foldM)
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (mapMaybe)
import Data.Map qualified as Map

import Vehicle.Language.Print (prettyVerbose)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Type.MetaMap ( MetaMap(..), keys, insert )
import Vehicle.Compile.Type.MetaMap qualified as MetaMap
import Vehicle.Compile.Type.MetaSet (MetaSet)
import Vehicle.Compile.Type.MetaSet qualified as MetaSet
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.VariableContext


--------------------------------------------------------------------------------
-- Substitution operation

type MetaSubstitution = MetaMap CheckedExpr

type MonadSubst m = (MonadCompile m, MonadReader MetaSubstitution m)

liftSubstitution :: MetaSubstitution -> MetaSubstitution
liftSubstitution = MetaMap.map (liftFreeDBIndices 1)

class MetaSubstitutable a where
  -- TODO change name away from M
  substM :: MonadSubst m => a -> m a

instance MetaSubstitutable a => MetaSubstitutable (a, a) where
  substM (e1, e2) = do
    e1' <- substM e1
    e2' <- substM e2
    return (e1', e2')

instance MetaSubstitutable a => MetaSubstitutable [a] where
  substM = traverse substM

instance MetaSubstitutable a => MetaSubstitutable (NonEmpty a) where
  substM = traverse substM

instance MetaSubstitutable CheckedArg where
  substM = traverseArgExpr substM

instance MetaSubstitutable CheckedBinder where
  substM = traverseBinderType substM

instance MetaSubstitutable CheckedExpr where
  substM ex =
    --logCompilerPass MaxDetail (prettyVerbose ex) $
    case ex of
      Universe ann l            -> return $ Universe ann l
      Hole     ann name         -> return $ Hole    ann name
      Builtin  ann op           -> return $ Builtin ann op
      Literal  ann l            -> return $ Literal ann l
      Var      ann v            -> return $ Var     ann v
      LVec     ann es           -> LVec     ann <$> traverse substM es
      Ann      ann term typ     -> Ann      ann <$> substM term   <*> substM typ
      Pi       ann binder res   -> Pi       ann <$> substM binder <*> local liftSubstitution (substM res)
      Let      ann e1 binder e2 -> Let      ann <$> substM e1     <*> substM binder <*> local liftSubstitution (substM e2)
      Lam      ann binder e     -> Lam      ann <$> substM binder <*> local liftSubstitution (substM e)

      e@(Meta ann _)  -> substMApp ann (e, [])
      e@(App ann _ _) -> substMApp ann (toHead e)

-- | We really don't want un-normalised lambda applications from solved meta-variables
-- clogging up our program so this function detects meta applications and normalises
-- them as it substitutes the meta in.
substMApp :: forall m . MonadSubst m
          => Provenance
          -> (CheckedExpr, [CheckedArg])
          -> m CheckedExpr
substMApp ann (fun@(Meta _ m), mArgs) = do
  subst <- ask
  case MetaMap.lookup m subst of
    Just eRes -> substM =<< substMArgs eRes mArgs
    Nothing   -> normAppList ann fun <$> substM mArgs
  where
    substMArgs :: CheckedExpr -> [CheckedArg] -> m CheckedExpr
    substMArgs (Lam _ _ body) (arg : args) = do
      substMArgs (argExpr arg `substInto` body) args
    substMArgs Lam{}          []           = compilerDeveloperError $
      "Meta variable" <+> pretty m <+> "does not appear to be applied to" <+>
      "every variable in the context"
    substMArgs e              args         = return $ normAppList ann e args
substMApp ann (fun, args) = normAppList ann <$> substM fun <*> substM args

instance MetaSubstitutable CheckedDecl where
  substM = traverseDeclExprs substM

instance MetaSubstitutable CheckedProg where
  substM (Main ds) = Main <$> traverse substM ds

instance MetaSubstitutable UnificationConstraint where
  substM (Unify es) = Unify <$> substM es

instance MetaSubstitutable TypeClassConstraint where
  substM (Has m tc es) = Has m tc <$> substM es

instance MetaSubstitutable Constraint where
  substM (UC ctx c) = UC ctx <$> substM c
  substM (TC ctx c) = TC ctx <$> substM c

instance MetaSubstitutable a => MetaSubstitutable (MetaMap a) where
  substM (MetaMap t) = MetaMap <$> traverse substM t

--------------------------------------------------------------------------------
-- The meta context

data MetaInfo = MetaInfo
  { metaProvenance :: Provenance
  , metaType       :: CheckedType
  , metaCtx        :: TypingBoundCtx
  }

instance MetaSubstitutable MetaInfo where
  substM (MetaInfo p t ctx) = MetaInfo p <$> substM t <*> pure ctx

-- | The meta-variables and constraints relating the variables currently in scope.
data MetaCtx = MetaCtx
  { metaInfo            :: [MetaInfo]
  -- ^ The origin and type of each meta variable.
  -- NB: these are stored in *reverse* order from which they were created.
  , currentSubstitution :: MetaSubstitution
  , constraints         :: [Constraint]
  , solvedMetas         :: MetaSet
  }

emptyMetaCtx :: MetaCtx
emptyMetaCtx = MetaCtx
  { metaInfo               = mempty
  , currentSubstitution    = mempty
  , constraints            = mempty
  , solvedMetas            = mempty
  }

substMetas :: (MonadMeta m, MetaSubstitutable a)
           => a -> m a
substMetas e = do
  subst <- getMetaSubstitution
  runReaderT (substM e) subst

--------------------------------------------------------------------------------
-- Meta-variables

type MonadMeta m =
  ( MonadState MetaCtx m
  , MonadCompile m
  )

-- | Creates a fresh meta variable. Meta variables need to remember what was
-- in the current context when they were created. We do this by creating a
-- meta-variable that takes everything in the current context as an argument
-- and then which is immediately applied to everything in the current context.
-- Post unification, any unneeded context arguments will be normalised away.
-- It returns the name of the meta and the expression of it applied to every
-- variable in the context.
freshMeta :: MonadMeta m
          => Provenance
          -> CheckedType
          -> TypingBoundCtx
          -> m (Meta, CheckedExpr)
freshMeta p metaType boundCtx = do
  -- Create a fresh name
  MetaCtx {..} <- get
  let nextMeta = length metaInfo
  put $ MetaCtx { metaInfo = MetaInfo p metaType boundCtx : metaInfo, .. }
  let meta = MetaVar nextMeta

  -- Create bound variables for everything in the context
  let ann = inserted p
  let boundEnv = reverse [ Var ann (Bound i) | i <- [0..length boundCtx - 1] ]

  -- Returns a meta applied to every bound variable in the context
  let metaExpr = normAppList ann (Meta ann meta) (map (ExplicitArg ann) boundEnv)

  logDebug MaxDetail $ "fresh-meta" <+> pretty meta <+> ":" <+> prettyVerbose metaType
  return (meta, metaExpr)

freshExprMeta :: MonadMeta m
              => Provenance
              -> CheckedType
              -> TypingBoundCtx
              -> m CheckedExpr
freshExprMeta p t ctx = snd <$> freshMeta p t ctx

freshPolarityMeta :: MonadMeta m => Provenance -> m CheckedExpr
freshPolarityMeta p = snd <$> freshMeta p (PolarityUniverse p) mempty

freshLinearityMeta :: MonadMeta m => Provenance -> m CheckedExpr
freshLinearityMeta p = snd <$> freshMeta p (LinearityUniverse p) mempty

freshUniverseLevelMeta :: MonadMeta m => Provenance -> m CheckedExpr
freshUniverseLevelMeta p = snd <$> freshMeta p (TypeUniverse p 0) mempty

freshTypeClassPlacementMeta :: MonadMeta m
                            => Provenance
                            -> CheckedType
                            -> m Meta
freshTypeClassPlacementMeta p t = fst <$> freshMeta p t []

-- |Creates a Pi type that abstracts over all bound variables
makeMetaType :: TypingBoundCtx
             -> Provenance
             -> CheckedType
             -> CheckedType
makeMetaType boundCtx ann resultType = foldr entryToPi resultType (reverse boundCtx)
  where
    entryToPi :: (DBBinding, CheckedType, Maybe CheckedExpr) -> CheckedType -> CheckedType
    entryToPi (name, t, _) = Pi ann (ExplicitBinder ann name t)

getMetaIndex :: [MetaInfo] -> Meta -> Int
getMetaIndex metaInfo (MetaVar m) = length metaInfo - m - 1

getMetaInfo :: MonadMeta m => Meta -> m MetaInfo
getMetaInfo m = do
  MetaCtx {..} <- get
  case metaInfo !!? getMetaIndex metaInfo m of
    Just info -> return info
    Nothing -> compilerDeveloperError $
      "Requesting info for unknown meta" <+> pretty m <+> "not in context"

getMetaProvenance :: MonadMeta m => Meta -> m Provenance
getMetaProvenance m = metaProvenance <$> getMetaInfo m

getMetaType :: MonadMeta m => Meta -> m CheckedType
getMetaType m = metaType <$> getMetaInfo m

getMetaContext :: MonadMeta m => Meta -> m TypingBoundCtx
getMetaContext m = metaCtx <$> getMetaInfo m

modifyMetasInfo :: MonadMeta m => Meta -> (MetaInfo -> MetaInfo) -> m ()
modifyMetasInfo m f = modify (\MetaCtx{..} ->
  let (xs, i : ys) = splitAt (getMetaIndex metaInfo m) metaInfo in
  MetaCtx
    { metaInfo = xs <> [f i] <> ys
    , ..
    })

getMetaSubstitution :: MonadMeta m => m MetaSubstitution
getMetaSubstitution = gets currentSubstitution

getSolvedMetas :: MonadMeta m => m MetaSet
getSolvedMetas = gets solvedMetas

clearMetaSubstitution :: MonadMeta m => m ()
clearMetaSubstitution = modifyMetaCtx $ \ MetaCtx {..} ->
  MetaCtx { currentSubstitution = mempty, ..}

clearSolvedMetas :: MonadMeta m => m ()
clearSolvedMetas = modifyMetaCtx $ \MetaCtx {..} ->
  MetaCtx { solvedMetas = mempty, ..}

modifyMetaCtx :: MonadMeta m => (MetaCtx -> MetaCtx) -> m ()
modifyMetaCtx = modify

substMetasThroughCtx :: MonadMeta m => m ()
substMetasThroughCtx = do
  MetaCtx {..} <- get
  substConstraints <- substMetas constraints
  substMetaInfo    <- substMetas metaInfo
  substMetaSolution <- substMetas currentSubstitution
  put $ MetaCtx
    { constraints = substConstraints
    , metaInfo = substMetaInfo
    , currentSubstitution = substMetaSolution
    , solvedMetas = solvedMetas
    }

getUnsolvedConstraints :: MonadMeta m => m [Constraint]
getUnsolvedConstraints = gets constraints

getTypeClassConstraints :: MonadMeta m => m [(TypeClassConstraint, ConstraintContext)]
getTypeClassConstraints = mapMaybe getTypeClassConstraint <$> getUnsolvedConstraints

getNumberOfMetasCreated :: MonadMeta m => m Int
getNumberOfMetasCreated = gets (length . metaInfo)

getUnsolvedMetas :: MonadMeta m => m MetaSet
getUnsolvedMetas = do
  metasSolved  <- keys <$> getMetaSubstitution
  numberOfMetasCreated <- getNumberOfMetasCreated
  let metasCreated = MetaSet.fromList $ fmap MetaVar [0..numberOfMetasCreated-1]
  return $ MetaSet.difference metasCreated metasSolved

getUnsolvedAuxiliaryMetas :: MonadMeta m => m MetaSet
getUnsolvedAuxiliaryMetas = filterMetasByTypes isAuxiliaryUniverse =<< getUnsolvedMetas

getMetaTypes :: MonadMeta m => MetaSet -> m [(Meta, CheckedType)]
getMetaTypes metas = traverse (\m -> (m,) <$> getMetaType m) (MetaSet.toList metas)

-- | Computes the set of all metas that are related via constraints to the
-- metas in the provided expression as long as the types of those metas
-- satisfy the provided predicate.
getMetasLinkedToMetasIn :: forall m . MonadMeta m
                        => CheckedType
                        -> (CheckedType -> Bool)
                        -> m MetaSet
getMetasLinkedToMetasIn t typeFilter = do
  constraints <- getUnsolvedConstraints
  directMetasInType <- filterMetasByTypes typeFilter (metasIn t)
  loopOverConstraints constraints directMetasInType
  where
    loopOverConstraints :: [Constraint] -> MetaSet -> m MetaSet
    loopOverConstraints constraints metas = do
      (unrelatedConstraints, newMetas) <- foldM processConstraint ([], metas) constraints
      if metas /= newMetas
        then loopOverConstraints unrelatedConstraints newMetas
        else return metas

    processConstraint :: ([Constraint], MetaSet) -> Constraint -> m ([Constraint], MetaSet)
    processConstraint (nonRelatedConstraints, typeMetas) constraint = do
      constraintMetas <- filterMetasByTypes typeFilter (metasIn constraint)
      return $ if MetaSet.disjoint constraintMetas typeMetas
        then (constraint : nonRelatedConstraints, typeMetas)
        else (nonRelatedConstraints, MetaSet.unions [constraintMetas, typeMetas])

filterMetasByTypes :: MonadMeta m => (CheckedType -> Bool) -> MetaSet -> m MetaSet
filterMetasByTypes typeFilter metas = do
  typedMetas <- getMetaTypes metas
  let filteredMetas = filter (typeFilter . snd) typedMetas
  return $ MetaSet.fromList (fmap fst filteredMetas)

abstractOverCtx :: TypingBoundCtx -> CheckedExpr -> CheckedExpr
abstractOverCtx ctx body =
  let ctxTypes   = fmap (\(_, t, _) -> t) ctx in
  foldr typeToLam body ctxTypes
  where
    typeToLam :: CheckedType -> CheckedExpr -> CheckedExpr
    typeToLam t = Lam ann (ExplicitBinder ann Nothing t)
      where ann = provenanceOf t

metaSolved :: (MonadState MetaCtx m, MonadCompile m)
           => Meta
           -> CheckedExpr
           -> m ()
metaSolved m solution = do
  MetaInfo p _ ctx <- getMetaInfo m
  let abstractedSolution = abstractOverCtx ctx solution

  logDebug MaxDetail $ "solved" <+> pretty m <+> "as" <+> prettyVerbose abstractedSolution

  metaSubst <- getMetaSubstitution
  case MetaMap.lookup m metaSubst of
    Just existing -> compilerDeveloperError $
      "meta-variable" <+> pretty m <+> "already solved as" <+>
      line <> indent 2 (squotes (prettyVerbose existing)) <> line <>
      "but is being re-solved as" <+>
      line <> indent 2 (squotes (prettyVerbose solution)) <> line <>
      "at" <+> pretty p
    -- Could use `insertWith` instead of `insert` here for one lookup instead of
    -- two, but not possible to throw a monadic error unfortunately.
    Nothing -> do
      modifyMetaCtx $ \ MetaCtx {..} -> MetaCtx
        { currentSubstitution = insert m abstractedSolution currentSubstitution
        , solvedMetas         = MetaSet.insert m solvedMetas
        , ..
        }

class HasMetas a where
  metasInWithArgs :: a -> MetaMap [CheckedArg]

  metasIn :: a -> MetaSet
  metasIn = MetaMap.keys . metasInWithArgs

instance HasMetas CheckedExpr where
  metasInWithArgs = \case
    Universe{}               -> mempty
    Hole{}                   -> mempty
    Literal{}                -> mempty
    Builtin{}                -> mempty
    Var {}                   -> mempty
    Meta _ m                 -> MetaMap.singleton m mempty
    Ann  _ e t               -> metasInWithArgs e <> metasInWithArgs t
    Pi   _ binder result     -> metasInWithArgs (typeOf binder) <> metasInWithArgs result
    Let  _ bound binder body -> metasInWithArgs bound <> metasInWithArgs (typeOf binder) <> metasInWithArgs body
    Lam  _ binder body       -> metasInWithArgs (typeOf binder) <> metasInWithArgs body
    LVec _ xs                -> MetaMap.unions (fmap metasInWithArgs xs)
    App  _ fun args          ->
      let argExprs = fmap argExpr (NonEmpty.toList args) in
      case fun of
        Meta p m ->
          let (varArgs, remainingArgs) = span isBoundVar argExprs in
          let remainingArgs' = fmap metasInWithArgs remainingArgs in
          MetaMap.unions $ MetaMap.singleton m (map (ExplicitArg p) varArgs) : remainingArgs'
        _ ->
          let args' = fmap metasInWithArgs argExprs in
          MetaMap.unions (metasInWithArgs fun : args')

instance HasMetas CheckedArg where
  metasInWithArgs arg = metasInWithArgs (argExpr arg)

instance HasMetas a => HasMetas (NonEmpty a) where
  metasInWithArgs xs = MetaMap.unions $ map metasInWithArgs (NonEmpty.toList xs)

instance HasMetas Constraint where
  metasInWithArgs = \case
    UC _ (Unify (e1, e2)) -> MetaMap.unions [metasInWithArgs e1, metasInWithArgs e2]
    TC _ (Has _ _ e)     -> metasInWithArgs e

prettyMetas :: MonadMeta m => MetaSet -> m (Doc a)
prettyMetas metas = do
  typedMetaList <- getMetaTypes metas
  let docs = fmap (uncurry prettyMetaInternal) typedMetaList
  return $ prettySetLike docs

prettyMeta :: MonadMeta m => Meta -> m (Doc a)
prettyMeta meta = prettyMetaInternal meta <$> getMetaType meta

prettyMetaInternal :: Meta -> CheckedType -> Doc a
prettyMetaInternal m t = pretty m <+> ":" <+> prettyVerbose t

clearMetaCtx :: MonadMeta m => m ()
clearMetaCtx = do
  logDebug MaxDetail "Clearing meta-variable context"
  modifyMetaCtx (const emptyMetaCtx)

--------------------------------------------------------------------------------
-- Constraints

addUnificationConstraint :: MonadMeta m
                         => ConstraintGroup
                         -> Provenance
                         -> TypingVariableCtx
                         -> CheckedExpr
                         -> CheckedExpr
                         -> m ()
addUnificationConstraint group p ctx e1 e2 = do
  let context    = ConstraintContext p mempty ctx group
  let constraint = UC context $ Unify (e1, e2)
  addConstraints [constraint]

addTypeClassConstraint :: MonadMeta m
                       => TypingVariableCtx
                       -> Meta
                       -> CheckedExpr
                       -> m ()
addTypeClassConstraint ctx meta expr = do
  (tc, args) <- case expr of
    BuiltinTypeClass _ tc args -> return (tc, args)
    _                          -> compilerDeveloperError $
      "Malformed type class constraint" <+> prettyVerbose expr

  let group      = typeClassGroup tc
  let context    = ConstraintContext (provenanceOf expr) mempty ctx group
  let constraint = TC context (Has meta tc args)
  addConstraints [constraint]

addConstraints :: MonadMeta m => [Constraint] -> m ()
addConstraints []             = return ()
addConstraints newConstraints = do
  logDebug MaxDetail ("add-constraints " <> align (prettyVerbose newConstraints))
  modifyMetaCtx $ \ MetaCtx {..} ->
    MetaCtx { constraints = constraints ++ newConstraints, ..}

setConstraints :: MonadMeta m => [Constraint] -> m ()
setConstraints newConstraints = modifyMetaCtx $ \MetaCtx{..} ->
    MetaCtx { constraints = newConstraints, ..}

-- | Returns any constraints that are activated (i.e. worth retrying) based
-- on the set of metas that were solved last pass.
popActivatedConstraints :: MonadMeta m => MetaSet -> m [Constraint]
popActivatedConstraints metasSolved = do
  allConstraints <- getUnsolvedConstraints
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

data ConstraintProgress
  = Stuck MetaSet
  | Progress [Constraint]
  deriving (Show)

instance Pretty ConstraintProgress where
  pretty (Stuck metas)         = "StuckOn[" <+> pretty metas <+> "]"
  pretty (Progress constraints) = "Resolution" <+> prettyVerbose constraints

isStuck :: ConstraintProgress -> Bool
isStuck Stuck{} = True
isStuck _       = False

instance Semigroup ConstraintProgress where
  Stuck m1     <> Stuck m2     = Stuck (m1 <> m2)
  Stuck{}      <> x@Progress{} = x
  x@Progress{} <> Stuck{}      = x
  Progress r1  <> Progress r2 = Progress (r1 <> r2)

getDeclType :: (MonadCompile m, MonadReader TypingVariableCtx m)
            => Provenance -> Identifier -> m CheckedType
getDeclType p ident = do
  ctx <- getDeclCtx
  case Map.lookup ident ctx of
    Just (checkedType, _) -> return checkedType
    -- This should have been caught during scope checking
    Nothing -> compilerDeveloperError $
      "Declaration'" <+> pretty ident <+> "'not found when" <+>
      "looking up variable in context" <+> pretty (Map.keys ctx) <+>
      "at" <+> pretty p