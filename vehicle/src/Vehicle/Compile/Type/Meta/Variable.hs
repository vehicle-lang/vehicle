module Vehicle.Compile.Type.Meta.Variable
  ( MetaInfo (..),
    MetaCtxSize,
    increaseMetaCtxSize,
    makeMetaType,
    makeMetaExpr,
    getMetaDependencies,
    getNormMetaDependencies,
    metasInWithDependencies,
    HasMetas (..),
  )
where

import Control.Monad.Writer (MonadWriter (..), execWriterT)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint
  ( Constraint (..),
    TypeClassConstraint (..),
    UnificationConstraint (..),
  )
import Vehicle.Compile.Type.Meta.Map (MetaMap)
import Vehicle.Compile.Type.Meta.Map qualified as MetaMap
import Vehicle.Compile.Type.Meta.Set (MetaSet)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.VariableContext (TypingBoundCtx)
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised

-- Eventually when metas make into the builtins, this should module
-- should also contain the definition of meta-variables themselves.

--------------------------------------------------------------------------------
-- Meta information

-- | The size of a meta-variable's context (i.e. how many bound variables it
-- can depend on.) In theory this should be identical to the current `DBLevel`
-- but in practice, linearity/polarity/type-class insertion meta-variables
-- have a context of zero size as they cannot depend on the context.
type MetaCtxSize = Int

-- | The information stored about each meta-variable.
data MetaInfo = MetaInfo
  { -- | Location in the source file the meta-variable was generated
    metaProvenance :: Provenance,
    -- | The type of the meta-variable
    metaType :: CheckedType,
    -- | The number of bound variables in scope when the meta-variable was created.
    metaCtxSize :: MetaCtxSize
  }

increaseMetaCtxSize :: MetaInfo -> MetaInfo
increaseMetaCtxSize (MetaInfo p t size) = MetaInfo p t (size + 1)

-- | Creates an expression that abstracts over all bound variables
makeMetaExpr ::
  Provenance ->
  MetaID ->
  MetaCtxSize ->
  GluedExpr
makeMetaExpr p metaID ctxSize = do
  -- Create bound variables for everything in the context
  let ann = inserted p
  let dependencyLevels = [0 .. (ctxSize - 1)]
  let unnormBoundEnv = [ExplicitArg ann (Var ann (Bound $ DBIndex i)) | i <- reverse dependencyLevels]
  let normBoundEnv = [ExplicitArg ann (VBoundVar ann (DBLevel i) []) | i <- dependencyLevels]

  -- Returns a meta applied to every bound variable in the context
  Glued
    { unnormalised = normAppList ann (Meta ann metaID) unnormBoundEnv,
      normalised = VMeta ann metaID normBoundEnv
    }

-- | Creates a Pi type that abstracts over all bound variables
makeMetaType ::
  TypingBoundCtx ->
  Provenance ->
  CheckedType ->
  CheckedType
makeMetaType boundCtx ann resultType = foldr entryToPi resultType (reverse boundCtx)
  where
    entryToPi :: (Maybe Name, CheckedType, Maybe CheckedExpr) -> CheckedType -> CheckedType
    entryToPi (name, t, _) = do
      let n = fromMaybe "_" name
      Pi ann (Binder ann (BinderForm (OnlyName n) True) Explicit Relevant () t)

getMetaDependencies :: [CheckedArg] -> [DBIndex]
getMetaDependencies = \case
  (ExplicitArg _ (Var _ (Bound i))) : args -> i : getMetaDependencies args
  _ -> []

getNormMetaDependencies :: [NormArg] -> [DBLevel]
getNormMetaDependencies = \case
  (ExplicitArg _ (VBoundVar _ i [])) : args -> i : getNormMetaDependencies args
  _ -> []

--------------------------------------------------------------------------------
-- Objects which have meta variables in.

class HasMetas a where
  findMetas :: (MonadCompile m, MonadWriter MetaSet m) => a -> m ()

  metasIn :: MonadCompile m => a -> m MetaSet
  metasIn e = execWriterT (findMetas e)

instance HasMetas CheckedExpr where
  findMetas expr = case expr of
    Meta _ m -> tell (MetaSet.singleton m)
    Universe {} -> return ()
    Hole {} -> return ()
    Literal {} -> return ()
    Builtin {} -> return ()
    Var {} -> return ()
    LVec _ xs -> findMetas xs
    Ann _ e t -> do findMetas e; findMetas t
    Pi _ binder result -> do findMetas binder; findMetas result
    Let _ bound binder body -> do findMetas bound; findMetas binder; findMetas body
    Lam _ binder body -> do findMetas binder; findMetas body
    App _ fun args -> do findMetas fun; findMetas args

instance HasMetas NormExpr where
  findMetas expr = case expr of
    VMeta _ m spine -> do
      tell (MetaSet.singleton m)
      findMetas spine
    VUniverse {} -> return ()
    VLiteral {} -> return ()
    VBuiltin _ _ spine -> findMetas spine
    VFreeVar _ _ spine -> findMetas spine
    VBoundVar _ _ spine -> findMetas spine
    VLVec _ xs spine -> do findMetas xs; findMetas spine
    VPi _ binder result -> do findMetas binder; findMetas result
    VLam {} -> developerError "Finding metas in normalised lambda not yet supported"

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
    TypeClassConstraint c -> findMetas c

metasInWithDependencies :: MonadCompile m => NormExpr -> m (MetaMap [DBLevel])
metasInWithDependencies e = execWriterT (go e)
  where
    go :: (MonadCompile m, MonadWriter (MetaMap [DBLevel]) m) => NormExpr -> m ()
    go expr = case expr of
      VMeta _ m args -> do
        let deps = getNormMetaDependencies args
        tell (MetaMap.singleton m deps)
      VUniverse {} -> return ()
      VLiteral {} -> return ()
      VBuiltin _ _ spine -> goSpine spine
      VBoundVar _ _ spine -> goSpine spine
      VFreeVar _ _ spine -> goSpine spine
      VLVec _ xs spine -> do traverse_ go xs; goSpine spine
      VPi _ binder result -> do traverse_ go binder; go result
      VLam {} -> developerError "Finding metas in normalised lambda not yet supported"

    goSpine :: (MonadCompile m, MonadWriter (MetaMap [DBLevel]) m) => Spine -> m ()
    goSpine = traverse_ (traverse_ go)
