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
  ) where

import Control.Monad.Reader (MonadReader (..))
import Control.Monad.Writer (MonadWriter (..), execWriter)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty

import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.MetaMap (MetaMap (..))
import Vehicle.Compile.Type.MetaMap qualified as MetaMap
import Vehicle.Compile.Type.MetaSet (MetaSet)
import Vehicle.Compile.Type.VariableContext
import Vehicle.Language.Print (prettyVerbose)


--------------------------------------------------------------------------------
-- Substitution operation

type MetaSubstitution = MetaMap CheckedExpr

type MonadSubst m = (MonadCompile m, MonadReader MetaSubstitution m)

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
  substM = traverse substM

instance MetaSubstitutable CheckedBinder where
  substM = traverse substM

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
      -- NOTE: no need to lift the substitutions here as we're passing under the binders
      -- because by construction every meta-variable solution is a closed term.
      Pi       ann binder res   -> Pi       ann <$> substM binder <*> substM res
      Let      ann e1 binder e2 -> Let      ann <$> substM e1     <*> substM binder <*> substM e2
      Lam      ann binder e     -> Lam      ann <$> substM binder <*> substM e

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
  metaSubst <- ask
  case MetaMap.lookup m metaSubst of
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
  substM = traverse substM

instance MetaSubstitutable CheckedProg where
  substM (Main ds) = Main <$> traverse substM ds

instance MetaSubstitutable UnificationConstraint where
  substM (Unify e1 e2) = Unify <$> substM e1 <*> substM e2

instance MetaSubstitutable TypeClassConstraint where
  substM (Has m tc es) = Has m tc <$> substM es

instance MetaSubstitutable Constraint where
  substM = \case
    UnificationConstraint c -> UnificationConstraint <$> substM c
    TypeClassConstraint   c -> TypeClassConstraint   <$> substM c

instance MetaSubstitutable object => MetaSubstitutable (Contextualised object context) where
  substM (WithContext object context) = WithContext <$> substM object <*> pure context

instance MetaSubstitutable a => MetaSubstitutable (MetaMap a) where
  substM (MetaMap t) = MetaMap <$> traverse substM t

--------------------------------------------------------------------------------
-- The meta context

data MetaInfo = MetaInfo
  { metaProvenance :: Provenance
  , metaType       :: CheckedType
  , metaCtxSize    :: Int
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

--------------------------------------------------------------------------------
-- Objects which have meta variables in.

class HasMetas a where
  findMetas :: MonadWriter (MetaMap [CheckedArg]) m => a -> m ()

  metasInWithArgs :: a -> MetaMap [CheckedArg]
  metasInWithArgs e = execWriter (findMetas e)

  metasIn :: a -> MetaSet
  metasIn = MetaMap.keys . metasInWithArgs

instance HasMetas CheckedExpr where
  findMetas expr = case expr of
    Meta _ m -> do
      tell (MetaMap.singleton m [])

    App _ (Meta _ m) args    -> do
      let (metaCtxArgs, otherArgs) = span (isBoundVar . argExpr) (NonEmpty.toList args)
      tell (MetaMap.singleton m metaCtxArgs)
      findMetas otherArgs

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

instance HasMetas CheckedArg where
  findMetas = mapM_ findMetas

instance HasMetas CheckedBinder where
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
