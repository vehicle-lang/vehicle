module Vehicle.Compile.Type.Meta
  ( MetaSet
  , ConstraintProgress(..)
  , isStuck
  , TypingMetaCtx(..)
  , MetaInfo(..)
  , emptyMetaCtx
  , HasMetas(..)
  , MetaSubstitution
  , MetaSubstitutable(..)
  , makeMetaType
  , getMetaDependencies
  , getNormMetaDependencies
  ) where

import Control.Monad.Reader (MonadReader (..))
import Control.Monad.Writer (MonadWriter (..), execWriterT)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty

import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.MetaMap (MetaMap (..))
import Vehicle.Compile.Type.MetaMap qualified as MetaMap
import Vehicle.Compile.Type.MetaSet (MetaSet)
import Vehicle.Compile.Type.VariableContext
import Vehicle.Language.Print (prettyVerbose)
import Vehicle.Compile.Normalise.NormExpr (NormArg, NormExpr(..), GluedExpr (..))
import Vehicle.Compile.Normalise.NBE (evalApp, evalBuiltin)
import Control.Monad.Trans.Reader (ReaderT(..))


--------------------------------------------------------------------------------
-- Substitution operation

type MetaSubstitution = MetaMap GluedExpr

type MonadSubst m = (MonadCompile m, MonadReader (MetaSubstitution, DeclCtx NormExpr) m)

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

instance MetaSubstitutable a => MetaSubstitutable (GenericArg a) where
  substM = traverse substM

instance MetaSubstitutable a => MetaSubstitutable (GenericBinder value a) where
  substM = traverse substM

instance MetaSubstitutable CheckedExpr where
  substM expr =
    --logCompilerPass MaxDetail (prettyVerbose ex) $
    case expr of
      e@(Meta ann _)  -> substMApp ann (e, [])
      e@(App ann _ _) -> substMApp ann (toHead e)

      Universe{} -> return expr
      Hole{}     -> return expr
      Builtin{}  -> return expr
      Literal{}  -> return expr
      Var{}      -> return expr

      LVec ann es       -> LVec ann <$> traverse substM es
      Ann  ann term typ -> Ann  ann <$> substM term   <*> substM typ
      -- NOTE: no need to lift the substitutions here as we're passing under the binders
      -- because by construction every meta-variable solution is a closed term.
      Pi  ann binder res   -> Pi       ann <$> substM binder <*> substM res
      Let ann e1 binder e2 -> Let      ann <$> substM e1     <*> substM binder <*> substM e2
      Lam ann binder e     -> Lam      ann <$> substM binder <*> substM e

-- | We really don't want un-normalised lambda applications from solved meta-variables
-- clogging up our program so this function detects meta applications and normalises
-- them as it substitutes the meta in.
substMApp :: forall m . MonadSubst m
          => Provenance
          -> (CheckedExpr, [CheckedArg])
          -> m CheckedExpr
substMApp ann (fun@(Meta _ m), mArgs) = do
  (metaSubst, _declCtx) <- ask
  case MetaMap.lookup m metaSubst of
    Just value -> substM =<< substMArgs (unnormalised value) mArgs
    Nothing    -> normAppList ann fun <$> substM mArgs
  where
    substMArgs :: CheckedExpr -> [CheckedArg] -> m CheckedExpr
    substMArgs (Lam _ _ body) (arg : args) = do
      substMArgs (argExpr arg `substInto` body) args
    substMArgs Lam{}          []           = compilerDeveloperError $
      "Meta variable" <+> pretty m <+> "does not appear to be applied to" <+>
      "every variable in the context"
    substMArgs e              args         = return $ normAppList ann e args
substMApp ann (fun, args) = normAppList ann <$> substM fun <*> substM args

instance MetaSubstitutable NormExpr where
  substM expr = case expr of
    VMeta p m args -> do
      (metaSubst, declCtx) <- ask
      case MetaMap.lookup m metaSubst of
        -- TODO do we need to subst through the args here?
        Nothing    -> VMeta p m <$> substM args
        Just value -> do
          -- logDebug MaxDetail $ pretty m <+> pretty (show (normalised value))
          substValue <- substM $ normalised value
          case args of
            []       -> return substValue
            (a : as) -> do
              -- logDebug MaxDetail $ prettyVerbose substValue -- <+> prettyVerbose (fmap argExpr (a : as))
              runReaderT (evalApp substValue (a :| as)) declCtx

    VUniverse{} -> return expr
    VLiteral{}  -> return expr

    VVar     p v  spine -> VVar     p v <$> traverse substM spine
    VLVec    p xs spine -> VLVec    p   <$> traverse substM xs <*> traverse substM spine

    VBuiltin p b  spine -> do
      (_metaSubst, declCtx) <- ask
      spine' <- traverse substM spine
      runReaderT (evalBuiltin p b spine') declCtx

    -- NOTE: no need to lift the substitutions here as we're passing under the binders
    -- because by construction every meta-variable solution is a closed term.
    VLam  p binder env body -> VLam p <$> substM binder <*> substM env <*> substM body
    VPi   p binder body     -> VPi  p <$> substM binder <*> substM body

instance MetaSubstitutable GluedExpr where
  substM (Glued a b) = Glued <$> substM a <*> substM b

instance MetaSubstitutable expr => MetaSubstitutable (GenericDecl expr) where
  substM = traverse substM

instance MetaSubstitutable expr => MetaSubstitutable (GenericProg expr) where
  substM (Main ds) = Main <$> traverse substM ds

instance MetaSubstitutable UnificationConstraint where
  substM (Unify e1 e2) = Unify <$> substM e1 <*> substM e2

instance MetaSubstitutable TypeClassConstraint where
  substM (Has m tc es) = Has m tc <$> substM es

instance MetaSubstitutable Constraint where
  substM = \case
    UnificationConstraint c -> UnificationConstraint <$> substM c
    TypeClassConstraint   c -> TypeClassConstraint   <$> substM c

instance MetaSubstitutable a => MetaSubstitutable (MetaMap a) where
  substM (MetaMap t) = MetaMap <$> traverse substM t


--------------------------------------------------------------------------------
-- The meta context

data MetaInfo = MetaInfo
  { metaProvenance   :: Provenance
  , metaType         :: CheckedType
  , metaCtxSize      :: Int
  }

instance MetaSubstitutable MetaInfo where
  substM (MetaInfo p t ctx) = MetaInfo p <$> substM t <*> pure ctx

-- | The meta-variables and constraints relating the variables currently in scope.
data TypingMetaCtx = TypingMetaCtx
  { metaInfo            :: [MetaInfo]
  -- ^ The origin and type of each meta variable.
  -- NB: these are stored in *reverse* order from which they were created.
  , currentSubstitution :: MetaSubstitution
  , constraints         :: [WithContext Constraint]
  , solvedMetas         :: MetaSet
  }

emptyMetaCtx :: TypingMetaCtx
emptyMetaCtx = TypingMetaCtx
  { metaInfo               = mempty
  , currentSubstitution    = mempty
  , constraints            = mempty
  , solvedMetas            = mempty
  }

-- |Creates a Pi type that abstracts over all bound variables
makeMetaType :: TypingBoundCtx
             -> Provenance
             -> CheckedType
             -> CheckedType
makeMetaType boundCtx ann resultType = foldr entryToPi resultType (reverse boundCtx)
  where
    entryToPi :: (DBBinding, CheckedType, Maybe CheckedExpr) -> CheckedType -> CheckedType
    entryToPi (name, t, _) = Pi ann (ExplicitBinder ann name t)

getMetaDependencies :: [CheckedArg] -> [DBIndex]
getMetaDependencies = fmap (getDep . argExpr)
  where
  getDep :: CheckedExpr -> DBIndex
  getDep (Var _ (Bound i)) = i
  getDep e                 = developerError $
    "Non-variable expression" <+> prettyVerbose e <+> "found in meta dependencies"

getNormMetaDependencies :: forall m . MonadCompile m => [NormArg] -> m [DBIndex]
getNormMetaDependencies args = do
  let explicitArgs = takeWhile isExplicit args
  traverse (getDep . argExpr) explicitArgs
  where
  getDep :: NormExpr -> m DBIndex
  getDep (VVar _ (Bound i) []) = return i
  getDep e                     = compilerDeveloperError $
    "Non-variable expression" <+> prettyVerbose e <+> "found in norm meta dependencies"

--------------------------------------------------------------------------------
-- Objects which have meta variables in.

class HasMetas a where
  findMetas :: (MonadCompile m, MonadWriter (MetaMap [DBIndex]) m) => a -> m ()

  metasInWithDependencies :: MonadCompile m => a -> m (MetaMap [DBIndex])
  metasInWithDependencies e = execWriterT (findMetas e)

  metasIn :: MonadCompile m => a -> m MetaSet
  metasIn e = MetaMap.keys <$> metasInWithDependencies e

instance HasMetas CheckedExpr where
  findMetas expr = case expr of
    Meta _ m -> do
      tell (MetaMap.singleton m [])

    App _ (Meta _ m) args    -> do
      let deps = getMetaDependencies (NonEmpty.toList args)
      tell (MetaMap.singleton m deps)
      findMetas args

    Universe{}               -> return ()
    Hole{}                   -> return ()
    Literal{}                -> return ()
    Builtin{}                -> return ()
    Var {}                   -> return ()

    LVec _ xs                -> findMetas xs
    Ann  _ e t               -> do findMetas e; findMetas t
    Pi   _ binder result     -> do findMetas binder; findMetas result
    Let  _ bound binder body -> do findMetas bound; findMetas binder; findMetas body
    Lam  _ binder body       -> do findMetas binder; findMetas body
    App  _ fun args          -> do findMetas fun; findMetas args

instance HasMetas NormExpr where
  findMetas expr = case expr of
    VMeta _ m args -> do
      deps <- getNormMetaDependencies args
      tell (MetaMap.singleton m deps)

    VUniverse{}               -> return ()
    VLiteral{}                -> return ()

    VBuiltin _ _ spine        -> findMetas spine
    VVar _ _ spine            -> findMetas spine
    VLVec _ xs spine          -> do findMetas xs; findMetas spine
    VPi   _ binder result     -> do findMetas binder; findMetas result
    VLam{}                    -> developerError "Finding metas in normalised lambda not yet supported"

instance HasMetas expr => HasMetas (GenericArg expr) where
  findMetas = mapM_ findMetas

instance HasMetas expr => HasMetas (GenericBinder binder expr) where
  findMetas = mapM_ findMetas

instance HasMetas a => HasMetas [a] where
  findMetas = mapM_ findMetas

instance HasMetas a => HasMetas (NonEmpty a) where
  findMetas = mapM_ findMetas

instance HasMetas TypeClassConstraint where
  findMetas (Has _ _ e) = findMetas e

instance HasMetas UnificationConstraint where
  findMetas (Unify e1 e2) = do findMetas e1; findMetas e2

instance HasMetas Constraint where
  findMetas = \case
    UnificationConstraint c -> findMetas c
    TypeClassConstraint   c -> findMetas c

--------------------------------------------------------------------------------
-- Progress in solving meta-variable constraints

data ConstraintProgress
  = Stuck MetaSet
  | Progress [WithContext Constraint]
  deriving (Show)

instance Pretty ConstraintProgress where
  pretty (Stuck metas)          = "StuckOn[" <+> pretty metas <+> "]"
  pretty (Progress constraints) = "Resolution" <+> prettyVerbose constraints

isStuck :: ConstraintProgress -> Bool
isStuck Stuck{} = True
isStuck _       = False

instance Semigroup ConstraintProgress where
  Stuck m1     <> Stuck m2     = Stuck (m1 <> m2)
  Stuck{}      <> x@Progress{} = x
  x@Progress{} <> Stuck{}      = x
  Progress r1  <> Progress r2  = Progress (r1 <> r2)
