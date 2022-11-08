module Vehicle.Compile.Type.Monad.Class
  ( MonadTypeChecker(..)
  , substMetas
  , freshExprMeta
  , freshPolarityMeta
  , freshLinearityMeta
  , freshUniverseLevelMeta
  , freshTypeClassPlacementMeta
  , getMetaIndex
  , metaSolved
  , filterMetasByTypes
  , getUnsolvedAuxiliaryMetas
  , getMetasLinkedToMetasIn
  , clearSolvedMetas
  , clearMetaSubstitution
  , substMetasThroughCtx
  , modifyMetasInfo
  , getMetaContext
  , getMetaProvenance
  , prettyMetas
  , prettyMeta
  , clearMetaCtx
  , getMetaType
  , getDeclType
  , getMetaInfo
  , getTypeClassConstraints
  , addUnificationConstraint
  , addTypeClassConstraint
  , addConstraints
  , setConstraints
  , getMetaSubstitution
  , getSolvedMetas
  , getUnsolvedMetas
  , getUnsolvedConstraints
  , popActivatedConstraints
  , whnf
  ) where

import Control.Monad.Reader (ReaderT (..), mapReaderT)
import Control.Monad.State (StateT (..), mapStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT (..), mapWriterT)
import Data.List (partition)
import Data.Map qualified as Map

import Control.Monad (foldM)
import Data.Maybe (mapMaybe)
import Vehicle.Compile.Error (MonadCompile, compilerDeveloperError)
import Vehicle.Compile.Normalise (NormalisationOptions (..), normalise)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta (HasMetas (..), MetaInfo (..),
                                  MetaSubstitutable (..), MetaSubstitution,
                                  TypingMetaCtx (..), emptyMetaCtx)
import Vehicle.Compile.Type.MetaMap qualified as MetaMap
import Vehicle.Compile.Type.MetaSet (MetaSet)
import Vehicle.Compile.Type.MetaSet qualified as MetaSet
import Vehicle.Compile.Type.VariableContext (TypingBoundCtx, TypingDeclCtx,
                                             toNormalisationDeclContext)
import Vehicle.Language.Print (prettyVerbose)

--------------------------------------------------------------------------------
-- The type-checking monad class

-- | The type-checking monad.
class MonadCompile m => MonadTypeChecker m where
  getDeclContext :: m TypingDeclCtx
  addDeclContext :: CheckedDecl -> m a -> m a
  getMetaCtx     :: m TypingMetaCtx
  getsMetaCtx    :: (TypingMetaCtx -> a) -> m a
  putMetaCtx     :: TypingMetaCtx -> m ()
  modifyMetaCtx  :: (TypingMetaCtx -> TypingMetaCtx) -> m ()

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

instance MonadTypeChecker m => MonadTypeChecker (LoggerT m) where
  getDeclContext = lift getDeclContext
  addDeclContext d = mapLoggerT (addDeclContext d)
  getMetaCtx = lift getMetaCtx
  getsMetaCtx = lift . getsMetaCtx
  putMetaCtx = lift . putMetaCtx
  modifyMetaCtx = lift . modifyMetaCtx

-}

getUnsolvedConstraints :: MonadTypeChecker m => m [WithContext Constraint]
getUnsolvedConstraints = getsMetaCtx constraints

getNumberOfMetasCreated :: MonadTypeChecker m => m Int
getNumberOfMetasCreated = getsMetaCtx (length . metaInfo)

getMetaSubstitution :: MonadTypeChecker m => m MetaSubstitution
getMetaSubstitution = getsMetaCtx currentSubstitution

getSolvedMetas :: MonadTypeChecker m => m MetaSet
getSolvedMetas = getsMetaCtx solvedMetas

getUnsolvedMetas :: MonadTypeChecker m => m MetaSet
getUnsolvedMetas = do
  metasSolved  <- MetaMap.keys <$> getMetaSubstitution
  numberOfMetasCreated <- getNumberOfMetasCreated
  let metasCreated = MetaSet.fromList $ fmap MetaID [0..numberOfMetasCreated-1]
  return $ MetaSet.difference metasCreated metasSolved

setConstraints :: MonadTypeChecker m => [WithContext Constraint] -> m ()
setConstraints newConstraints = modifyMetaCtx $ \TypingMetaCtx{..} ->
    TypingMetaCtx { constraints = newConstraints, ..}


-- | Returns any constraints that are activated (i.e. worth retrying) based
-- on the set of metas that were solved last pass.
popActivatedConstraints :: MonadTypeChecker m => MetaSet -> m [WithContext Constraint]
popActivatedConstraints metasSolved = do
  allConstraints <- getUnsolvedConstraints
  let (blockedConstraints, unblockedConstraints) = partition (isBlocked metasSolved) allConstraints
  setConstraints blockedConstraints
  return unblockedConstraints
  where
    isBlocked :: MetaSet -> WithContext Constraint -> Bool
    isBlocked solvedMetas constraint =
      let blockingMetas = blockedBy $ contextOf constraint in
      -- A constraint is blocked if it is blocking on at least one meta
      -- and none of the metas it is blocking on have been solved in the last pass.
      not (MetaSet.null blockingMetas) && MetaSet.disjoint solvedMetas blockingMetas


substMetas :: (MonadTypeChecker m, MetaSubstitutable a) => a -> m a
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
freshMeta :: MonadTypeChecker m
          => Provenance
          -> CheckedType
          -> TypingBoundCtx
          -> m (MetaID, CheckedExpr)
freshMeta p metaType boundCtx = do
  -- Create a fresh name
  TypingMetaCtx {..} <- getMetaCtx
  let nextMeta = length metaInfo
  putMetaCtx $ TypingMetaCtx { metaInfo = MetaInfo p metaType boundCtx : metaInfo, .. }
  let meta = MetaID nextMeta

  -- Create bound variables for everything in the context
  let ann = inserted p
  let boundEnv = reverse [ Var ann (Bound i) | i <- [0..length boundCtx - 1] ]

  -- Returns a meta applied to every bound variable in the context
  let metaExpr = normAppList ann (Meta ann meta) (map (ExplicitArg ann) boundEnv)

  logDebug MaxDetail $ "fresh-meta" <+> pretty meta <+> ":" <+> prettyVerbose metaType
  return (meta, metaExpr)

freshExprMeta :: MonadTypeChecker m
              => Provenance
              -> CheckedType
              -> TypingBoundCtx
              -> m CheckedExpr
freshExprMeta p t ctx = snd <$> freshMeta p t ctx

freshPolarityMeta :: MonadTypeChecker m => Provenance -> m CheckedExpr
freshPolarityMeta p = snd <$> freshMeta p (PolarityUniverse p) mempty

freshLinearityMeta :: MonadTypeChecker m => Provenance -> m CheckedExpr
freshLinearityMeta p = snd <$> freshMeta p (LinearityUniverse p) mempty

freshUniverseLevelMeta :: MonadTypeChecker m => Provenance -> m CheckedExpr
freshUniverseLevelMeta p = snd <$> freshMeta p (TypeUniverse p 0) mempty

freshTypeClassPlacementMeta :: MonadTypeChecker m
                            => Provenance
                            -> CheckedType
                            -> m MetaID
freshTypeClassPlacementMeta p t = fst <$> freshMeta p t []

--------------------------------------------------------------------------------
-- Meta information retrieval

getMetaIndex :: [MetaInfo] -> MetaID -> Int
getMetaIndex metaInfo (MetaID m) = length metaInfo - m - 1

getMetaInfo :: MonadTypeChecker m => MetaID -> m MetaInfo
getMetaInfo m = do
  TypingMetaCtx {..} <- getMetaCtx
  case metaInfo !!? getMetaIndex metaInfo m of
    Just info -> return info
    Nothing -> compilerDeveloperError $
      "Requesting info for unknown meta" <+> pretty m <+> "not in context"

getMetaProvenance :: MonadTypeChecker m => MetaID -> m Provenance
getMetaProvenance m = metaProvenance <$> getMetaInfo m

getMetaType :: MonadTypeChecker m => MetaID -> m CheckedType
getMetaType m = metaType <$> getMetaInfo m

getMetaContext :: MonadTypeChecker m => MetaID -> m TypingBoundCtx
getMetaContext m = metaCtx <$> getMetaInfo m

modifyMetasInfo :: MonadTypeChecker m => MetaID -> (MetaInfo -> MetaInfo) -> m ()
modifyMetasInfo m f = modifyMetaCtx (\TypingMetaCtx{..} ->
  let (xs, i : ys) = splitAt (getMetaIndex metaInfo m) metaInfo in
  TypingMetaCtx
    { metaInfo = xs <> [f i] <> ys
    , ..
    })

clearMetaSubstitution :: MonadTypeChecker m => m ()
clearMetaSubstitution = modifyMetaCtx $ \ TypingMetaCtx {..} ->
  TypingMetaCtx { currentSubstitution = mempty, ..}

clearSolvedMetas :: MonadTypeChecker m => m ()
clearSolvedMetas = modifyMetaCtx $ \TypingMetaCtx {..} ->
  TypingMetaCtx { solvedMetas = mempty, ..}

substMetasThroughCtx :: MonadTypeChecker m => m ()
substMetasThroughCtx = do
  TypingMetaCtx {..} <- getMetaCtx
  substConstraints  <- substMetas constraints
  substMetaInfo     <- substMetas metaInfo
  substMetaSolution <- substMetas currentSubstitution
  putMetaCtx $ TypingMetaCtx
    { constraints = substConstraints
    , metaInfo = substMetaInfo
    , currentSubstitution = substMetaSolution
    , solvedMetas = solvedMetas
    }

getUnsolvedAuxiliaryMetas :: MonadTypeChecker m => m MetaSet
getUnsolvedAuxiliaryMetas = filterMetasByTypes isAuxiliaryUniverse =<< getUnsolvedMetas

getMetaTypes :: MonadTypeChecker m => MetaSet -> m [(MetaID, CheckedType)]
getMetaTypes metas = traverse (\m -> (m,) <$> getMetaType m) (MetaSet.toList metas)

-- | Computes the set of all metas that are related via constraints to the
-- metas in the provided expression as long as the types of those metas
-- satisfy the provided predicate.
getMetasLinkedToMetasIn :: forall m . MonadTypeChecker m
                        => CheckedType
                        -> (CheckedType -> Bool)
                        -> m MetaSet
getMetasLinkedToMetasIn t typeFilter = do
  constraints <- fmap objectIn <$> getUnsolvedConstraints
  directMetasInType <- filterMetasByTypes typeFilter (metasIn t)
  loopOverConstraints constraints directMetasInType
  where
    loopOverConstraints :: [Constraint] -> MetaSet -> m MetaSet
    loopOverConstraints constraints metas = do
      (unrelatedConstraints, newMetas) <- foldM processConstraint ([], metas) constraints
      if metas /= newMetas
        then loopOverConstraints unrelatedConstraints newMetas
        else return metas

    processConstraint :: ([Constraint], MetaSet)
                      -> Constraint
                      -> m ([Constraint], MetaSet)
    processConstraint (nonRelatedConstraints, typeMetas) constraint = do
      constraintMetas <- filterMetasByTypes typeFilter (metasIn constraint)
      return $ if MetaSet.disjoint constraintMetas typeMetas
        then (constraint : nonRelatedConstraints, typeMetas)
        else (nonRelatedConstraints, MetaSet.unions [constraintMetas, typeMetas])

filterMetasByTypes :: MonadTypeChecker m => (CheckedType -> Bool) -> MetaSet -> m MetaSet
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

metaSolved :: MonadTypeChecker m => MetaID -> CheckedExpr -> m ()
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
      modifyMetaCtx $ \ TypingMetaCtx {..} -> TypingMetaCtx
        { currentSubstitution = MetaMap.insert m abstractedSolution currentSubstitution
        , solvedMetas         = MetaSet.insert m solvedMetas
        , ..
        }

prettyMetas :: MonadTypeChecker m => MetaSet -> m (Doc a)
prettyMetas metas = do
  typedMetaList <- getMetaTypes metas
  let docs = fmap (uncurry prettyMetaInternal) typedMetaList
  return $ prettySetLike docs

prettyMeta :: MonadTypeChecker m => MetaID -> m (Doc a)
prettyMeta meta = prettyMetaInternal meta <$> getMetaType meta

prettyMetaInternal :: MetaID -> CheckedType -> Doc a
prettyMetaInternal m t = pretty m <+> ":" <+> prettyVerbose t

clearMetaCtx :: MonadTypeChecker m => m ()
clearMetaCtx = do
  logDebug MaxDetail "Clearing meta-variable context"
  modifyMetaCtx (const emptyMetaCtx)

getDeclType :: MonadTypeChecker m => Provenance -> Identifier -> m CheckedType
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

getTypeClassConstraints :: MonadTypeChecker m => m [WithContext TypeClassConstraint]
getTypeClassConstraints = mapMaybe getTypeClassConstraint <$> getUnsolvedConstraints

addUnificationConstraint :: MonadTypeChecker m
                         => ConstraintGroup
                         -> Provenance
                         -> TypingBoundCtx
                         -> CheckedExpr
                         -> CheckedExpr
                         -> m ()
addUnificationConstraint group p ctx e1 e2 = do
  let constraint = UnificationConstraint $ Unify e1 e2
  let context    = ConstraintContext p p mempty ctx group
  addConstraints [WithContext constraint context]

addTypeClassConstraint :: MonadTypeChecker m
                       => Provenance
                       -> TypingBoundCtx
                       -> MetaID
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
  let constraint = TypeClassConstraint (Has meta tc args)
  addConstraints [WithContext constraint context]

addConstraints :: MonadTypeChecker m => [WithContext Constraint] -> m ()
addConstraints []             = return ()
addConstraints newConstraints = do
  logDebug MaxDetail ("add-constraints " <> align (prettyVerbose newConstraints))
  modifyMetaCtx $ \ TypingMetaCtx {..} ->
    TypingMetaCtx { constraints = constraints ++ newConstraints, ..}

whnf :: MonadTypeChecker m => CheckedExpr -> m CheckedExpr
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
