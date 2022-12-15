module Vehicle.Compile.Type.Meta.Variable
  ( MetaInfo (..),
    increaseMetaCtxSize,
    makeMetaType,
    makeMetaExpr,
    getMetaDependencies,
    getNormMetaDependencies,
    HasMetas (..),
  )
where

import Control.Monad.Writer (MonadWriter (..), execWriterT)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
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
import Vehicle.Compile.Type.VariableContext (TypingBoundCtx)
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised

-- Eventually when metas make into the builtins, this should module
-- should also contain the definition of meta-variables themselves.

--------------------------------------------------------------------------------
-- Meta information

-- | The information stored about each meta-variable.
data MetaInfo = MetaInfo
  { -- | Location in the source file the meta-variable was generated
    metaProvenance :: Provenance,
    -- | The type of the meta-variable
    metaType :: CheckedType,
    -- | The number of bound variables in scope when the meta-variable was created.
    metaCtxSize :: Int
  }

increaseMetaCtxSize :: MetaInfo -> MetaInfo
increaseMetaCtxSize (MetaInfo p t size) = MetaInfo p t (size + 1)

-- | Creates an expression that abstracts over all bound variables
makeMetaExpr ::
  Provenance ->
  MetaID ->
  Int ->
  GluedExpr
makeMetaExpr p metaID boundCtxSize = do
  -- Create bound variables for everything in the context
  let ann = inserted p
  let deps = fmap (Bound . DBIndex) (reverse [0 .. boundCtxSize - 1])
  let unnormBoundEnv = [ExplicitArg ann (Var ann i) | i <- deps]
  let normBoundEnv = [ExplicitArg ann (VVar ann i []) | i <- deps]

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
    entryToPi :: (DBBinding, CheckedType, Maybe CheckedExpr) -> CheckedType -> CheckedType
    entryToPi (name, t, _) = Pi ann (Binder ann (BinderForm OnlyName True) Explicit Relevant name t)

getMetaDependencies :: [CheckedArg] -> [DBIndex]
getMetaDependencies = \case
  (ExplicitArg _ (Var _ (Bound i))) : args -> i : getMetaDependencies args
  _ -> []

getNormMetaDependencies :: [NormArg] -> [DBIndex]
getNormMetaDependencies = \case
  (ExplicitArg _ (VVar _ (Bound i) [])) : args -> i : getNormMetaDependencies args
  _ -> []

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
    App _ (Meta _ m) args -> do
      let deps = getMetaDependencies (NonEmpty.toList args)
      tell (MetaMap.singleton m deps)
      findMetas args
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
    VMeta _ m args -> do
      let deps = getNormMetaDependencies args
      tell (MetaMap.singleton m deps)
    VUniverse {} -> return ()
    VLiteral {} -> return ()
    VBuiltin _ _ spine -> findMetas spine
    VVar _ _ spine -> findMetas spine
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
