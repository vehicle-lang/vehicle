module Vehicle.Compile.Type.Meta
  ( MetaSet
  , ConstraintProgress(..)
  , isStuck
  , MetaCtx(..)
  , MetaInfo(..)
  , emptyMetaCtx
  , HasMetas(..)
  , MetaSubstitution
  , MetaSubstitutable(..)
  ) where

import Control.Monad.Reader (MonadReader (..), local)
import Control.Monad.Writer (MonadWriter (..), execWriter)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty

import Vehicle.Language.Print (prettyVerbose)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Type.MetaMap ( MetaMap(..) )
import Vehicle.Compile.Type.MetaMap qualified as MetaMap
import Vehicle.Compile.Type.MetaSet (MetaSet)
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

--------------------------------------------------------------------------------
-- Objects which have meta variables in.

class HasMetas a where
  traverseMetas :: Monad m
                => (Provenance -> Meta -> [CheckedArg] -> m CheckedExpr)
                -> a
                -> m a

  metasInWithArgs :: a -> MetaMap [CheckedArg]
  metasInWithArgs e = execWriter (traverseMetas f e)
    where
      f p m args = do
        tell (MetaMap.singleton m args)
        return $ normAppList p (Meta p m) args

  metasIn :: a -> MetaSet
  metasIn = MetaMap.keys . metasInWithArgs

instance HasMetas CheckedExpr where
  traverseMetas f expr = case expr of
    Meta p m                 -> f p m []
    App p (Meta _ m) args    -> do
      let (metaCtxArgs, otherArgs) = span (isBoundVar . argExpr) (NonEmpty.toList args)
      otherArgs' <- traverseMetas f otherArgs
      result <- f p m metaCtxArgs
      return $ normAppList p result otherArgs'

    Universe{}               -> return expr
    Hole{}                   -> return expr
    Literal{}                -> return expr
    Builtin{}                -> return expr
    Var {}                   -> return expr
    Ann  p e t               -> Ann p <$> traverseMetas f e <*> traverseMetas f t
    Pi   p binder result     -> Pi p <$> traverseMetas f binder <*> traverseMetas f result
    Let  p bound binder body -> Let p <$> traverseMetas f bound <*> traverseMetas f binder <*> traverseMetas f body
    Lam  p binder body       -> Lam p <$> traverseMetas f binder <*> traverseMetas f body
    LVec p xs                -> LVec p <$> traverseMetas f xs
    App  p fun args          -> App p <$> traverseMetas f fun <*> traverseMetas f args

instance HasMetas CheckedArg where
  traverseMetas f = traverseArgExpr (traverseMetas f)

instance HasMetas CheckedBinder where
  traverseMetas f = traverseBinderType (traverseMetas f)

instance HasMetas a => HasMetas [a] where
  traverseMetas f = traverse (traverseMetas f)

instance HasMetas a => HasMetas (NonEmpty a) where
  traverseMetas f = traverse (traverseMetas f)

instance HasMetas Constraint where
  traverseMetas f = \case
    UC ctx (Unify (e1, e2)) -> do
      e1' <- traverseMetas f e1
      e2' <- traverseMetas f e2
      return $ UC ctx (Unify (e1', e2'))

    TC ctx (Has m tc e) -> do
      e' <- traverseMetas f e
      return $ TC ctx $ Has m tc e'

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