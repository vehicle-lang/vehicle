module Vehicle.Compile.Type.Meta.Variable
  ( MetaInfo (..),
    extendMetaCtx,
    makeMetaType,
    getMetaDependencies,
    MetaVariableContext,
    emptyMetaVariableContext,
    addFreshMeta,
    numberOfMetas,
    addMetaSolution,
    lookupMetaInfo,
    adjustMetaInfo,
    traverseMetaInfos,
    unsolvedMetasIn,
    solvedMetasIn,
    metasSatisfyingIn,
  )
where

import Data.Maybe (fromMaybe, isJust, isNothing)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Meta.Map (MetaMap)
import Vehicle.Compile.Type.Meta.Map qualified as MetaMap
import Vehicle.Data.Code.ForcedValue (GluedExprWithMetas)
import Vehicle.Data.Variable.Bound.Context.Generic.Core

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
    -- | The relevance of the meta-variable
    metaRelevance :: Relevance,
    -- | The number of bound variables in scope when the meta-variable was created.
    metaCtx :: BoundCtx (Expr builtin),
    -- | The solution to the meta variable
    metaSolution :: Maybe (GluedExprWithMetas builtin)
  }

extendMetaCtx :: Binder builtin -> MetaInfo builtin -> MetaInfo builtin
extendMetaCtx binder MetaInfo {..} =
  MetaInfo
    { metaCtx = binder : metaCtx,
      ..
    }

addSolutionToInfo :: GluedExprWithMetas builtin -> MetaInfo builtin -> MetaInfo builtin
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
      Pi p (Binder (BinderDisplayForm (OnlyName n mempty) True) Explicit (relevanceOf binder) (typeOf binder))

getMetaDependencies :: [Arg builtin] -> [Ix]
getMetaDependencies = \case
  (ExplicitArg _ (BoundVar _ i)) : args -> i : getMetaDependencies args
  _ -> []

--------------------------------------------------------------------------------
-- Meta context

-- | The context of meta-variables. This type is deliberately kept abstract
-- so that the only way to create a new meta-variable is via `addFreshMeta`,
-- guaranteeing that `nextMetaID` stays in sync with the set of metas.
data MetaVariableContext builtin = MetaVariableContext
  { -- | The information about each meta-variable created so far.
    metaInfoMap :: MetaMap (MetaInfo builtin),
    -- | The ID to assign to the next meta-variable created. Stored explicitly
    -- because computing the size of a `MetaMap` is O(n).
    nextMetaID :: MetaID
  }

emptyMetaVariableContext :: MetaVariableContext builtin
emptyMetaVariableContext =
  MetaVariableContext
    { metaInfoMap = mempty,
      nextMetaID = MetaID 0
    }

-- | The number of meta-variables created so far.
numberOfMetas :: MetaVariableContext builtin -> Int
numberOfMetas ctx = unMetaID $ nextMetaID ctx

-- | Adds a new meta-variable to the context, returning its ID.
addFreshMeta ::
  MetaInfo builtin ->
  MetaVariableContext builtin ->
  (MetaID, MetaVariableContext builtin)
addFreshMeta info (MetaVariableContext infos metaID@(MetaID i)) =
  (metaID, MetaVariableContext (MetaMap.insert metaID info infos) (MetaID (i + 1)))

lookupMetaInfo ::
  MetaVariableContext builtin ->
  MetaID ->
  MetaInfo builtin
lookupMetaInfo ctx meta =
  case MetaMap.lookup meta (metaInfoMap ctx) of
    Just info -> info
    Nothing ->
      developerError $
        "Requesting info for unknown meta"
          <+> pretty meta
          <+> "not in context"

-- | Updates the information about an existing meta-variable.
adjustMetaInfo ::
  (MetaInfo builtin -> MetaInfo builtin) ->
  MetaID ->
  MetaVariableContext builtin ->
  MetaVariableContext builtin
adjustMetaInfo f meta ctx =
  ctx {metaInfoMap = MetaMap.adjust f meta (metaInfoMap ctx)}

-- | Updates the information about every meta-variable.
traverseMetaInfos ::
  (Monad m) =>
  (MetaInfo builtin -> m (MetaInfo builtin)) ->
  MetaVariableContext builtin ->
  m (MetaVariableContext builtin)
traverseMetaInfos f ctx = do
  metaInfoMap' <- traverse f (metaInfoMap ctx)
  return $ ctx {metaInfoMap = metaInfoMap'}

addMetaSolution ::
  GluedExprWithMetas builtin ->
  MetaID ->
  MetaVariableContext builtin ->
  MetaVariableContext builtin
addMetaSolution solution =
  adjustMetaInfo (addSolutionToInfo solution)

metasSatisfyingIn ::
  (MetaInfo builtin -> Bool) ->
  MetaVariableContext builtin ->
  MetaMap (MetaInfo builtin)
metasSatisfyingIn f ctx = MetaMap.filter f (metaInfoMap ctx)

unsolvedMetasIn ::
  MetaVariableContext builtin ->
  MetaMap (MetaInfo builtin)
unsolvedMetasIn = metasSatisfyingIn (isNothing . metaSolution)

solvedMetasIn ::
  MetaVariableContext builtin ->
  MetaMap (MetaInfo builtin)
solvedMetasIn = metasSatisfyingIn (isJust . metaSolution)
