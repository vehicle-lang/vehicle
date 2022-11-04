{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Compile.Type.Monad where

import Control.Monad (foldM)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), mapReaderT)
import Control.Monad.State (MonadState (..), StateT (..), evalStateT, gets,
                            mapStateT, modify)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT (..), mapWriterT)
import Data.List (partition)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)

import Control.Monad.Trans (MonadTrans)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise (NormalisationOptions (..), normalise)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.MetaMap (insert, keys)
import Vehicle.Compile.Type.MetaMap qualified as MetaMap
import Vehicle.Compile.Type.MetaSet qualified as MetaSet
import Vehicle.Compile.Type.VariableContext
import Vehicle.Language.Print (prettyVerbose)

--------------------------------------------------------------------------------
-- The type-checking monad

class Monad m => MonadTypeChecker m where
  getDeclContext  :: m TypingDeclCtx
  addDeclContext :: CheckedDecl -> m a -> m a
  getMetaCtx     :: m MetaCtx
  getsMetaCtx    :: (MetaCtx -> a) -> m a
  putMetaCtx     :: MetaCtx -> m ()
  modifyMetaCtx :: (MetaCtx -> MetaCtx) -> m ()

newtype TypeCheckerT m a = TypeCheckerT
  { unTypeCheckerT :: ReaderT TypingDeclCtx (StateT MetaCtx m) a
  } deriving (Functor, Applicative, Monad)

instance MonadTrans TypeCheckerT where
  lift = TypeCheckerT . lift . lift

instance Monad m => MonadTypeChecker (TypeCheckerT m) where
  getDeclContext = TypeCheckerT ask
  addDeclContext d s = TypeCheckerT $ local (addToDeclCtx d) (unTypeCheckerT s)
  getMetaCtx = TypeCheckerT get
  getsMetaCtx f = TypeCheckerT $ gets f
  putMetaCtx x = TypeCheckerT $ put x
  modifyMetaCtx f = TypeCheckerT $ modify f

instance (Monoid w, MonadTypeChecker m) => MonadTypeChecker (WriterT w m) where
  getDeclContext = lift getDeclContext
  addDeclContext d = mapWriterT (addDeclContext d)
  getMetaCtx = lift getMetaCtx
  getsMetaCtx = lift . getsMetaCtx
  putMetaCtx = lift . putMetaCtx
  modifyMetaCtx = lift . modifyMetaCtx

instance (Monoid w, MonadTypeChecker m) => MonadTypeChecker (ReaderT w m) where
  getDeclContext = lift getDeclContext
  addDeclContext d = mapReaderT (addDeclContext d)
  getMetaCtx = lift getMetaCtx
  getsMetaCtx = lift . getsMetaCtx
  putMetaCtx = lift . putMetaCtx
  modifyMetaCtx = lift . modifyMetaCtx

instance MonadTypeChecker m => MonadTypeChecker (StateT s m) where
  getDeclContext = lift getDeclContext
  addDeclContext d = mapStateT (addDeclContext d)
  getMetaCtx = lift getMetaCtx
  getsMetaCtx = lift . getsMetaCtx
  putMetaCtx = lift . putMetaCtx
  modifyMetaCtx = lift . modifyMetaCtx
{-
instance MonadTypeChecker m => MonadTypeChecker (ExceptT w m) where
  getDeclContext = lift getDeclContext
  addDeclContext d = mapExceptT (addDeclContext d)
  getMetaCtx = lift getMetaCtx
  getsMetaCtx = lift . getsMetaCtx
  putMetaCtx = lift . putMetaCtx
  modifyMetaCtx = lift . modifyMetaCtx

instance MonadTypeChecker m => MonadTypeChecker (LoggerT m) where
  getDeclContext = lift getDeclContext
  addDeclContext d = mapLoggerT (addDeclContext d)
  getMetaCtx = lift getMetaCtx
  getsMetaCtx :: MonadTypeChecker m => (MetaCtx -> a) -> LoggerT m a
  getsMetaCtx = lift . getsMetaCtx
  putMetaCtx = lift . putMetaCtx
  modifyMetaCtx = lift . modifyMetaCtx
  -}

mapTypeCheckerT :: (m (a, MetaCtx) -> n (b, MetaCtx)) -> TypeCheckerT m a -> TypeCheckerT n b
mapTypeCheckerT f m = TypeCheckerT (mapReaderT (mapStateT f) (unTypeCheckerT m))

instance MonadError e m => MonadError e (TypeCheckerT m) where
  throwError = lift . throwError
  catchError m f = TypeCheckerT (catchError (unTypeCheckerT m) (unTypeCheckerT . f))

instance MonadLogger m => MonadLogger (TypeCheckerT m) where
  getCallDepth  = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage    = lift . logMessage

instance MonadReader r m => MonadReader r (TypeCheckerT m) where
  ask = lift ask
  local = mapTypeCheckerT . local

runTypeCheckerT :: Monad m => TypeCheckerT m a -> m a
runTypeCheckerT (TypeCheckerT e) = evalStateT (runReaderT e mempty) emptyMetaCtx

-- | The type-checking monad.
type TCM m =
  ( MonadCompile     m
  , MonadTypeChecker m
  )

-- | Local type checking monad for when we're somewhere inside an expression.
type LocalTCM m =
  ( TCM m
  , MonadReader TypingBoundCtx m
  )

toNormalisationDeclContext :: TypingDeclCtx -> DeclCtx CheckedExpr
toNormalisationDeclContext = Map.mapMaybe snd


substMetas :: (TCM m, MetaSubstitutable a) => a -> m a
substMetas e = do
  metaSubst <- getMetaSubstitution
  runReaderT (substM e) metaSubst

--------------------------------------------------------------------------------
-- Meta-variable creation

-- | Creates a fresh meta variable. Meta variables need to remember what was
-- in the current context when they were created. We do this by creating a
-- meta-variable that takes everything in the current context as an argument
-- and then which is immediately applied to everything in the current context.
-- Post unification, any unneeded context arguments will be normalised away.
-- It returns the name of the meta and the expression of it applied to every
-- variable in the context.
freshMeta :: TCM m
          => Provenance
          -> CheckedType
          -> TypingBoundCtx
          -> m (Meta, CheckedExpr)
freshMeta p metaType boundCtx = do
  -- Create a fresh name
  MetaCtx {..} <- getMetaCtx
  let nextMeta = length metaInfo
  putMetaCtx $ MetaCtx { metaInfo = MetaInfo p metaType boundCtx : metaInfo, .. }
  let meta = MetaVar nextMeta

  -- Create bound variables for everything in the context
  let ann = inserted p
  let boundEnv = reverse [ Var ann (Bound i) | i <- [0..length boundCtx - 1] ]

  -- Returns a meta applied to every bound variable in the context
  let metaExpr = normAppList ann (Meta ann meta) (map (ExplicitArg ann) boundEnv)

  logDebug MaxDetail $ "fresh-meta" <+> pretty meta <+> ":" <+> prettyVerbose metaType
  return (meta, metaExpr)

freshExprMeta :: TCM m
              => Provenance
              -> CheckedType
              -> TypingBoundCtx
              -> m CheckedExpr
freshExprMeta p t ctx = snd <$> freshMeta p t ctx

freshPolarityMeta :: TCM m => Provenance -> m CheckedExpr
freshPolarityMeta p = snd <$> freshMeta p (PolarityUniverse p) mempty

freshLinearityMeta :: TCM m => Provenance -> m CheckedExpr
freshLinearityMeta p = snd <$> freshMeta p (LinearityUniverse p) mempty

freshUniverseLevelMeta :: TCM m => Provenance -> m CheckedExpr
freshUniverseLevelMeta p = snd <$> freshMeta p (TypeUniverse p 0) mempty

freshTypeClassPlacementMeta :: TCM m
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

--------------------------------------------------------------------------------
-- Meta information retrieval

getMetaIndex :: [MetaInfo] -> Meta -> Int
getMetaIndex metaInfo (MetaVar m) = length metaInfo - m - 1

getMetaInfo :: TCM m => Meta -> m MetaInfo
getMetaInfo m = do
  MetaCtx {..} <- getMetaCtx
  case metaInfo !!? getMetaIndex metaInfo m of
    Just info -> return info
    Nothing -> compilerDeveloperError $
      "Requesting info for unknown meta" <+> pretty m <+> "not in context"

getMetaProvenance :: TCM m => Meta -> m Provenance
getMetaProvenance m = metaProvenance <$> getMetaInfo m

getMetaType :: TCM m => Meta -> m CheckedType
getMetaType m = metaType <$> getMetaInfo m

getMetaContext :: TCM m => Meta -> m TypingBoundCtx
getMetaContext m = metaCtx <$> getMetaInfo m

modifyMetasInfo :: TCM m => Meta -> (MetaInfo -> MetaInfo) -> m ()
modifyMetasInfo m f = modifyMetaCtx (\MetaCtx{..} ->
  let (xs, i : ys) = splitAt (getMetaIndex metaInfo m) metaInfo in
  MetaCtx
    { metaInfo = xs <> [f i] <> ys
    , ..
    })

getMetaSubstitution :: TCM m => m MetaSubstitution
getMetaSubstitution = getsMetaCtx currentSubstitution

getSolvedMetas :: TCM m => m MetaSet
getSolvedMetas = getsMetaCtx solvedMetas

clearMetaSubstitution :: TCM m => m ()
clearMetaSubstitution = modifyMetaCtx $ \ MetaCtx {..} ->
  MetaCtx { currentSubstitution = mempty, ..}

clearSolvedMetas :: TCM m => m ()
clearSolvedMetas = modifyMetaCtx $ \MetaCtx {..} ->
  MetaCtx { solvedMetas = mempty, ..}

substMetasThroughCtx :: TCM m => m ()
substMetasThroughCtx = do
  MetaCtx {..} <- getMetaCtx
  substConstraints  <- substMetas constraints
  substMetaInfo     <- substMetas metaInfo
  substMetaSolution <- substMetas currentSubstitution
  putMetaCtx $ MetaCtx
    { constraints = substConstraints
    , metaInfo = substMetaInfo
    , currentSubstitution = substMetaSolution
    , solvedMetas = solvedMetas
    }

getUnsolvedMetas :: TCM m => m MetaSet
getUnsolvedMetas = do
  metasSolved  <- keys <$> getMetaSubstitution
  numberOfMetasCreated <- getNumberOfMetasCreated
  let metasCreated = MetaSet.fromList $ fmap MetaVar [0..numberOfMetasCreated-1]
  return $ MetaSet.difference metasCreated metasSolved

getUnsolvedAuxiliaryMetas :: TCM m => m MetaSet
getUnsolvedAuxiliaryMetas = filterMetasByTypes isAuxiliaryUniverse =<< getUnsolvedMetas

getMetaTypes :: TCM m => MetaSet -> m [(Meta, CheckedType)]
getMetaTypes metas = traverse (\m -> (m,) <$> getMetaType m) (MetaSet.toList metas)

-- | Computes the set of all metas that are related via constraints to the
-- metas in the provided expression as long as the types of those metas
-- satisfy the provided predicate.
getMetasLinkedToMetasIn :: forall m . TCM m
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

filterMetasByTypes :: TCM m => (CheckedType -> Bool) -> MetaSet -> m MetaSet
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

metaSolved :: TCM m => Meta -> CheckedExpr -> m ()
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

prettyMetas :: TCM m => MetaSet -> m (Doc a)
prettyMetas metas = do
  typedMetaList <- getMetaTypes metas
  let docs = fmap (uncurry prettyMetaInternal) typedMetaList
  return $ prettySetLike docs

prettyMeta :: TCM m => Meta -> m (Doc a)
prettyMeta meta = prettyMetaInternal meta <$> getMetaType meta

prettyMetaInternal :: Meta -> CheckedType -> Doc a
prettyMetaInternal m t = pretty m <+> ":" <+> prettyVerbose t

clearMetaCtx :: TCM m => m ()
clearMetaCtx = do
  logDebug MaxDetail "Clearing meta-variable context"
  modifyMetaCtx (const emptyMetaCtx)

getDeclType :: TCM m => Provenance -> Identifier -> m CheckedType
getDeclType p ident = do
  ctx <- getDeclContext
  case Map.lookup ident ctx of
    Just (checkedType, _) -> return checkedType
    -- This should have been caught during scope checking
    Nothing -> compilerDeveloperError $
      "Declaration'" <+> pretty ident <+> "'not found when" <+>
      "looking up variable in context" <+> pretty (Map.keys ctx) <+>
      "at" <+> pretty p

--------------------------------------------------------------------------------
-- Constraints

getUnsolvedConstraints :: TCM m => m [Constraint]
getUnsolvedConstraints = getsMetaCtx constraints

getTypeClassConstraints :: TCM m => m [(TypeClassConstraint, ConstraintContext)]
getTypeClassConstraints = mapMaybe getTypeClassConstraint <$> getUnsolvedConstraints

getNumberOfMetasCreated :: TCM m => m Int
getNumberOfMetasCreated = getsMetaCtx (length . metaInfo)

addUnificationConstraint :: TCM m
                         => ConstraintGroup
                         -> Provenance
                         -> TypingBoundCtx
                         -> CheckedExpr
                         -> CheckedExpr
                         -> m ()
addUnificationConstraint group p ctx e1 e2 = do
  let context    = ConstraintContext p p mempty ctx group
  let constraint = UC context $ Unify (e1, e2)
  addConstraints [constraint]

addTypeClassConstraint :: TCM m
                       => Provenance
                       -> TypingBoundCtx
                       -> Meta
                       -> CheckedExpr
                       -> m ()
addTypeClassConstraint creationProvenance ctx meta expr = do
  (tc, args) <- case expr of
    BuiltinTypeClass _ tc args -> return (tc, args)
    _                          -> compilerDeveloperError $
      "Malformed type class constraint" <+> prettyVerbose expr

  let originProvenance = provenanceOf expr

  let group      = typeClassGroup tc
  let context    = ConstraintContext originProvenance creationProvenance mempty ctx group
  let constraint = TC context (Has meta tc args)
  addConstraints [constraint]

addConstraints :: TCM m => [Constraint] -> m ()
addConstraints []             = return ()
addConstraints newConstraints = do
  logDebug MaxDetail ("add-constraints " <> align (prettyVerbose newConstraints))
  modifyMetaCtx $ \ MetaCtx {..} ->
    MetaCtx { constraints = constraints ++ newConstraints, ..}

setConstraints :: TCM m => [Constraint] -> m ()
setConstraints newConstraints = modifyMetaCtx $ \MetaCtx{..} ->
    MetaCtx { constraints = newConstraints, ..}

-- | Returns any constraints that are activated (i.e. worth retrying) based
-- on the set of metas that were solved last pass.
popActivatedConstraints :: TCM m => MetaSet -> m [Constraint]
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

createMetaAndAddTypeClassConstraint :: LocalTCM m => Provenance -> CheckedType -> m CheckedExpr
createMetaAndAddTypeClassConstraint p tc = do
  m <- freshTypeClassPlacementMeta p tc
  ctx <- getBoundCtx
  addTypeClassConstraint p ctx m tc
  return $ Meta p m

whnf :: TCM m => CheckedExpr -> m CheckedExpr
whnf e = do
  declCtx <- getDeclContext
  discardLoggerT $ normalise e Options
    { declContext                 = toNormalisationDeclContext declCtx
    , boundContext                = mempty -- see issue #129
    , normaliseDeclApplications   = True
    , normaliseLambdaApplications = True
    , normaliseStdLibApplications = True
    , normaliseBuiltin            = const True
    , normaliseWeakly             = False
    }
