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
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalisable
import Vehicle.Expr.Normalised

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
data MetaInfo types = MetaInfo
  { -- | Location in the source file the meta-variable was generated
    metaProvenance :: Provenance,
    -- | The type of the meta-variable
    metaType :: NormalisableExpr types,
    -- | The number of bound variables in scope when the meta-variable was created.
    metaCtx :: TypingBoundCtx types
  }

extendMetaCtx :: NormalisableBinder types -> MetaInfo types -> MetaInfo types
extendMetaCtx binder MetaInfo {..} =
  MetaInfo
    { metaCtx = mkTypingBoundCtxEntry binder : metaCtx,
      ..
    }

-- | Creates an expression that abstracts over all bound variables
makeMetaExpr ::
  Provenance ->
  MetaID ->
  TypingBoundCtx types ->
  GluedExpr types
makeMetaExpr p metaID boundCtx = do
  -- Create bound variables for everything in the context
  let dependencyLevels = [0 .. (length boundCtx - 1)]
  let unnormBoundEnv = [ExplicitArg p (BoundVar p $ Ix i) | i <- reverse dependencyLevels]
  let normBoundEnv = [ExplicitArg p (VBoundVar (Lv i) []) | i <- dependencyLevels]

  -- Returns a meta applied to every bound variable in the context
  Glued
    { unnormalised = normAppList p (Meta p metaID) unnormBoundEnv,
      normalised = VMeta metaID normBoundEnv
    }

-- | Creates a Pi type that abstracts over all bound variables
makeMetaType ::
  TypingBoundCtx types ->
  Provenance ->
  NormalisableType types ->
  NormalisableType types
makeMetaType boundCtx p resultType = foldr entryToPi resultType (reverse boundCtx)
  where
    entryToPi ::
      (Maybe Name, NormalisableType types) ->
      NormalisableType types ->
      NormalisableType types
    entryToPi (name, t) = do
      let n = fromMaybe "_" name
      Pi p (Binder p (BinderDisplayForm (OnlyName n) True) Explicit Relevant t)

getMetaDependencies :: [NormalisableArg types] -> [Ix]
getMetaDependencies = \case
  (ExplicitArg _ (BoundVar _ i)) : args -> i : getMetaDependencies args
  _ -> []

getNormMetaDependencies :: [VArg types] -> ([Lv], Spine types)
getNormMetaDependencies = \case
  (ExplicitArg _ (VBoundVar i [])) : args -> first (i :) $ getNormMetaDependencies args
  spine -> ([], spine)

--------------------------------------------------------------------------------
-- Objects which have meta variables in.

class HasMetas a where
  findMetas :: (MonadCompile m, MonadWriter MetaSet m) => a -> m ()

  metasIn :: (MonadCompile m) => a -> m MetaSet
  metasIn e = execWriterT (findMetas e)

instance HasMetas (NormalisableExpr types) where
  findMetas expr = case expr of
    Meta _ m -> tell (MetaSet.singleton m)
    Universe {} -> return ()
    Hole {} -> return ()
    Builtin {} -> return ()
    BoundVar {} -> return ()
    FreeVar {} -> return ()
    Ann _ e t -> do findMetas e; findMetas t
    Pi _ binder result -> do findMetas binder; findMetas result
    Let _ bound binder body -> do findMetas bound; findMetas binder; findMetas body
    Lam _ binder body -> do findMetas binder; findMetas body
    App _ fun args -> do findMetas fun; findMetas args

instance HasMetas (Value types) where
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

instance HasMetas (TypeClassConstraint types) where
  findMetas (Has _ _ e) = findMetas e

instance HasMetas (UnificationConstraint types) where
  findMetas (Unify e1 e2) = do findMetas e1; findMetas e2

instance HasMetas (Constraint types) where
  findMetas = \case
    UnificationConstraint c -> findMetas c
    TypeClassConstraint c -> findMetas c
