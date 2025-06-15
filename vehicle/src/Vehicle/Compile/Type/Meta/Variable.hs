module Vehicle.Compile.Type.Meta.Variable
  ( MetaInfo (..),
    extendMetaCtx,
    makeMetaType,
    getMetaDependencies,
    HasMetas (..),
    MetaVariableContext,
    findMetaInfo,
    addMetaSolution,
  )
where

import Control.Monad.Writer (MonadWriter (..), execWriter)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta.Map (MetaMap)
import Vehicle.Compile.Type.Meta.Map qualified as MetaMap
import Vehicle.Compile.Type.Meta.Set (MetaSet)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Data.Code.Value

-- Eventually when metas make into the builtins, this should module
-- should also contain the definition of meta-variables themselves.

--------------------------------------------------------------------------------
-- Meta information

-- | The information stored about each meta-variable.
data MetaInfo builtin = MetaInfo
  { -- | Location in the source file the meta-variable was generated
    metaProvenance :: Provenance,
    -- | The type of the meta-variable
    metaType :: Type builtin,
    -- | The number of bound variables in scope when the meta-variable was created.
    metaCtx :: BoundCtx (Expr builtin),
    -- | The solution to the meta variable
    metaSolution :: Maybe (GluedExpr builtin)
  }

extendMetaCtx :: Binder builtin -> MetaInfo builtin -> MetaInfo builtin
extendMetaCtx binder MetaInfo {..} =
  MetaInfo
    { metaCtx = binder : metaCtx,
      ..
    }

addSolutionToInfo :: GluedExpr builtin -> MetaInfo builtin -> MetaInfo builtin
addSolutionToInfo solution info = info {metaSolution = Just solution}

-- | Creates a Pi type that abstracts over all bound variables
makeMetaType ::
  BoundCtx (Type builtin) ->
  Provenance ->
  Type builtin ->
  Type builtin
makeMetaType boundCtx p resultType = foldr entryToPi resultType (reverse boundCtx)
  where
    entryToPi ::
      Binder builtin ->
      Type builtin ->
      Type builtin
    entryToPi binder = do
      let n = fromMaybe "_" (nameOf binder)
      Pi p (Binder p (BinderDisplayForm (OnlyName n) True) Explicit (relevanceOf binder) (typeOf binder))

getMetaDependencies :: [Arg builtin] -> [Ix]
getMetaDependencies = \case
  (ExplicitArg _ _ (BoundVar _ i)) : args -> i : getMetaDependencies args
  _ -> []

--------------------------------------------------------------------------------
-- Objects which have meta variables in.

class HasMetas a where
  findMetas :: (MonadWriter MetaSet m) => a -> m ()

  metasIn :: a -> MetaSet
  metasIn e = execWriter (findMetas e)

instance HasMetas (Expr builtin) where
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
    App fun args -> do findMetas fun; findMetas args

instance HasMetas (Value builtin) where
  findMetas expr = case expr of
    VMeta m spine -> do
      tell (MetaSet.singleton m)
      findMetas spine
    VUniverse {} -> return ()
    VBuiltin _ spine -> findMetas spine
    VFreeVar _ spine -> findMetas spine
    VBoundVar _ spine -> findMetas spine
    VPi binder closure -> do findMetas binder; findMetas closure
    VLam binder closure -> do findMetas binder; findMetas closure

instance HasMetas (Closure builtin) where
  findMetas (Closure env expr) = do findMetas (fmap snd env); findMetas expr

instance (HasMetas expr) => HasMetas (GenericArg expr) where
  findMetas = mapM_ findMetas

instance (HasMetas expr) => HasMetas (GenericBinder expr) where
  findMetas = mapM_ findMetas

instance (HasMetas a) => HasMetas [a] where
  findMetas = mapM_ findMetas

instance (HasMetas a) => HasMetas (NonEmpty a) where
  findMetas = mapM_ findMetas

instance HasMetas (InstanceConstraint builtin) where
  findMetas (Resolve _ m _ goal) = do
    tell (MetaSet.singleton m)
    findMetas goal

instance HasMetas (InstanceGoal builtin) where
  findMetas (InstanceGoal _ _ spine) = findMetas spine

instance HasMetas (UnificationConstraint builtin) where
  findMetas (Unify _ e1 e2) = do findMetas e1; findMetas e2

instance HasMetas (ArgInsertionProblem builtin) where
  findMetas ArgInsertionProblem {..} = do
    findMetas originalFun
    findMetas checkedArgs
    findMetas uncheckedArgs

instance HasMetas (ApplicationConstraint builtin) where
  findMetas (InferArgs _ _ insertionProblem) = findMetas insertionProblem

instance HasMetas (Constraint builtin) where
  findMetas = \case
    UnificationConstraint c -> findMetas c
    InstanceConstraint c -> findMetas c
    ApplicationConstraint c -> findMetas c

--------------------------------------------------------------------------------
-- Meta context

type MetaVariableContext builtin = MetaMap (MetaInfo builtin)

findMetaInfo :: MetaVariableContext builtin -> MetaID -> MetaInfo builtin
findMetaInfo ctx meta =
  case MetaMap.lookup meta ctx of
    Just info -> info
    Nothing ->
      developerError $
        "Requesting info for unknown meta" <+> pretty meta <+> "not in context"

addMetaSolution :: GluedExpr builtin -> MetaID -> MetaVariableContext builtin -> MetaVariableContext builtin
addMetaSolution solution = MetaMap.adjust (addSolutionToInfo solution)
