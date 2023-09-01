module Vehicle.Compile.Type.Meta.Variable
  ( MetaInfo (..),
    MetaCtxSize,
    extendMetaCtx,
    makeMetaType,
    makeMetaExpr,
    getMetaDependencies,
    getNormMetaDependencies,
    HasMetas (..),
  )
where

import Control.Monad.Writer (MonadWriter (..), execWriterT)
import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta.Set (MetaSet)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Data.NormalisedExpr

-- Eventually when metas make into the builtins, this should module
-- should also contain the definition of meta-variables themselves.

--------------------------------------------------------------------------------
-- Meta information

-- | The size of a meta-variable's context (i.e. how many bound variables it
-- can depend on.) In theory this should be identical to the current `Lv`
-- but in practice, linearity/polarity/type-class insertion meta-variables
-- have a context of zero size as they cannot depend on the context.
type MetaCtxSize = Int

-- | The information stored about each meta-variable.
data MetaInfo builtin = MetaInfo
  { -- | Location in the source file the meta-variable was generated
    metaProvenance :: Provenance,
    -- | The type of the meta-variable
    metaType :: Expr Ix builtin,
    -- | The number of bound variables in scope when the meta-variable was created.
    metaCtx :: BoundCtx builtin
  }

extendMetaCtx :: Binder Ix builtin -> MetaInfo builtin -> MetaInfo builtin
extendMetaCtx binder MetaInfo {..} =
  MetaInfo
    { metaCtx = binder : metaCtx,
      ..
    }

-- | Creates an expression that abstracts over all bound variables
makeMetaExpr ::
  Provenance ->
  MetaID ->
  BoundCtx builtin ->
  GluedExpr builtin
makeMetaExpr p metaID boundCtx = do
  -- Create bound variables for everything in the context
  let dependencyLevels = [0 .. (length boundCtx - 1)]
  let unnormBoundEnv = [Arg p Explicit Relevant (BoundVar p $ Ix i) | i <- reverse dependencyLevels]
  let normBoundEnv = [Arg p Explicit Relevant (VBoundVar (Lv i) []) | i <- dependencyLevels]

  -- Returns a meta applied to every bound variable in the context
  Glued
    { unnormalised = normAppList p (Meta p metaID) unnormBoundEnv,
      normalised = VMeta metaID normBoundEnv
    }

-- | Creates a Pi type that abstracts over all bound variables
makeMetaType ::
  BoundCtx builtin ->
  Provenance ->
  Type Ix builtin ->
  Type Ix builtin
makeMetaType boundCtx p resultType = foldr entryToPi resultType (reverse boundCtx)
  where
    entryToPi ::
      Binder Ix builtin ->
      Type Ix builtin ->
      Type Ix builtin
    entryToPi binder = do
      let n = fromMaybe "_" (nameOf binder)
      Pi p (Binder p (BinderDisplayForm (OnlyName n) True) Explicit (relevanceOf binder) (typeOf binder))

getMetaDependencies :: [Arg Ix builtin] -> [Ix]
getMetaDependencies = \case
  (ExplicitArg _ _ (BoundVar _ i)) : args -> i : getMetaDependencies args
  _ -> []

getNormMetaDependencies :: [VArg builtin] -> ([Lv], Spine builtin)
getNormMetaDependencies = \case
  (ExplicitArg _ _ (VBoundVar i [])) : args -> first (i :) $ getNormMetaDependencies args
  spine -> ([], spine)

--------------------------------------------------------------------------------
-- Objects which have meta variables in.

class HasMetas a where
  findMetas :: (MonadCompile m, MonadWriter MetaSet m) => a -> m ()

  metasIn :: (MonadCompile m) => a -> m MetaSet
  metasIn e = execWriterT (findMetas e)

instance HasMetas (Expr Ix builtin) where
  findMetas expr = case expr of
    Meta _ m -> tell (MetaSet.singleton m)
    Universe {} -> return ()
    Hole {} -> return ()
    Builtin {} -> return ()
    BoundVar {} -> return ()
    FreeVar {} -> return ()
    Pi _ binder result -> do findMetas binder; findMetas result
    Let _ bound binder body -> do findMetas bound; findMetas binder; findMetas body
    Lam _ binder body -> do findMetas binder; findMetas body
    App _ fun args -> do findMetas fun; findMetas args

instance HasMetas (Value builtin) where
  findMetas expr = case expr of
    VMeta m spine -> do
      tell (MetaSet.singleton m)
      findMetas spine
    VUniverse {} -> return ()
    VBuiltin _ spine -> findMetas spine
    VFreeVar _ spine -> findMetas spine
    VBoundVar _ spine -> findMetas spine
    VPi binder result -> do findMetas binder; findMetas result
    VLam {} -> developerError "Finding metas in normalised lambda not yet supported"

instance (HasMetas expr) => HasMetas (GenericArg expr) where
  findMetas = mapM_ findMetas

instance (HasMetas expr) => HasMetas (GenericBinder expr) where
  findMetas = mapM_ findMetas

instance (HasMetas a) => HasMetas [a] where
  findMetas = mapM_ findMetas

instance (HasMetas a) => HasMetas (NonEmpty a) where
  findMetas = mapM_ findMetas

instance HasMetas (InstanceConstraint builtin) where
  findMetas (Resolve _ _ _ e) = findMetas e

instance HasMetas (UnificationConstraint builtin) where
  findMetas (Unify _ e1 e2) = do findMetas e1; findMetas e2

instance HasMetas (Constraint builtin) where
  findMetas = \case
    UnificationConstraint c -> findMetas c
    InstanceConstraint c -> findMetas c
