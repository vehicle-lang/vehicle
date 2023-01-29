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
import Vehicle.Compile.Type.Constraint
  ( Constraint (..),
    TypeClassConstraint (..),
    UnificationConstraint (..),
  )
import Vehicle.Compile.Type.Meta.Set (MetaSet)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.VariableContext (TypingBoundCtx, mkTypingBoundCtxEntry)
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
    metaCtx :: TypingBoundCtx
  }

extendMetaCtx :: CheckedBinder -> MetaInfo -> MetaInfo
extendMetaCtx binder MetaInfo {..} =
  MetaInfo
    { metaCtx = mkTypingBoundCtxEntry binder : metaCtx,
      ..
    }

-- | Creates an expression that abstracts over all bound variables
makeMetaExpr ::
  Provenance ->
  MetaID ->
  TypingBoundCtx ->
  GluedExpr
makeMetaExpr p metaID boundCtx = do
  -- Create bound variables for everything in the context
  let dependencyLevels = [0 .. (length boundCtx - 1)]
  let unnormBoundEnv = [ExplicitArg p (Var p (Bound $ DBIndex i)) | i <- reverse dependencyLevels]
  let normBoundEnv = [ExplicitArg p (VBoundVar p (DBLevel i) []) | i <- dependencyLevels]

  -- Returns a meta applied to every bound variable in the context
  Glued
    { unnormalised = normAppList p (Meta p metaID) unnormBoundEnv,
      normalised = VMeta p metaID normBoundEnv
    }

-- | Creates a Pi type that abstracts over all bound variables
makeMetaType ::
  TypingBoundCtx ->
  Provenance ->
  CheckedType ->
  CheckedType
makeMetaType boundCtx p resultType = foldr entryToPi resultType (reverse boundCtx)
  where
    entryToPi :: (Maybe Name, CheckedType, Maybe CheckedExpr) -> CheckedType -> CheckedType
    entryToPi (name, t, _) = do
      let n = fromMaybe "_" name
      Pi p (Binder p (BinderDisplayForm (OnlyName n) True) Explicit Relevant () t)

getMetaDependencies :: [CheckedArg] -> [DBIndex]
getMetaDependencies = \case
  (ExplicitArg _ (Var _ (Bound i))) : args -> i : getMetaDependencies args
  _ -> []

getNormMetaDependencies :: [NormArg] -> ([DBLevel], Spine)
getNormMetaDependencies = \case
  (ExplicitArg _ (VBoundVar _ i [])) : args -> first (i :) $ getNormMetaDependencies args
  spine -> ([], spine)

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
