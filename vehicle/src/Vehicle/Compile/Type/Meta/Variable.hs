module Vehicle.Compile.Type.Meta.Variable
  ( MetaInfo (..),
    SolvedMetaInfo (..),
    extendMetaCtx,
    makeMetaType,
    getMetaDependencies,
    MetaVariableContext,
    emptyMetaVariableContext,
    addFreshMeta,
    numberOfMetas,
    addMetaSolution,
    lookupMetaInfo,
    lookupMetaSolution,
    isMetaSolved,
    setMetaType,
    traverseMetaTypesAndSolutions,
    unsolvedMetaInfos,
    solvedMetaInfos,
    solvedMetaIDs,
  )
where

import Data.Maybe (fromMaybe)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Meta.Map (MetaMap)
import Vehicle.Compile.Type.Meta.Map qualified as MetaMap
import Vehicle.Compile.Type.Meta.Set (MetaSet)
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
    metaCtx :: BoundCtx (Expr builtin)
  }

-- | The information stored about each solved meta-variable.
data SolvedMetaInfo builtin = SolvedMetaInfo
  { -- | The information about the meta-variable itself.
    solvedMetaInfo :: MetaInfo builtin,
    -- | The solution to the meta variable
    metaSolution :: GluedExprWithMetas builtin
  }

extendMetaCtx :: Binder builtin -> MetaInfo builtin -> MetaInfo builtin
extendMetaCtx binder MetaInfo {..} =
  MetaInfo
    { metaCtx = binder : metaCtx,
      ..
    }

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

-- | The context of meta-variables. This type is deliberately kept abstract.
data MetaVariableContext builtin = MetaVariableContext
  { -- | The ID to assign to the next meta-variable created. Stored explicitly
    -- because computing the size of a `MetaMap` is O(n).
    nextMetaID :: MetaID,
    -- | The information about each meta-variable that has not yet been solved.
    unsolvedMetaMap :: MetaMap (MetaInfo builtin),
    -- | The information about, and solution to, each solved meta-variable.
    solvedMetaMap :: MetaMap (SolvedMetaInfo builtin)
  }

emptyMetaVariableContext :: MetaVariableContext builtin
emptyMetaVariableContext =
  MetaVariableContext
    { nextMetaID = MetaID 0,
      unsolvedMetaMap = mempty,
      solvedMetaMap = mempty
    }

-- | The number of meta-variables created so far.
numberOfMetas :: MetaVariableContext builtin -> Int
numberOfMetas ctx = unMetaID $ nextMetaID ctx

-- | Adds a new unsolved meta-variable to the context, returning its ID.
addFreshMeta ::
  MetaInfo builtin ->
  MetaVariableContext builtin ->
  (MetaID, MetaVariableContext builtin)
addFreshMeta info ctx@(MetaVariableContext {nextMetaID = metaID@(MetaID i)}) =
  ( metaID,
    ctx
      { nextMetaID = MetaID (i + 1),
        unsolvedMetaMap = MetaMap.insert metaID info (unsolvedMetaMap ctx)
      }
  )

-- | Marks an unsolved meta-variable as solved.
addMetaSolution ::
  GluedExprWithMetas builtin ->
  MetaID ->
  MetaVariableContext builtin ->
  MetaVariableContext builtin
addMetaSolution solution meta ctx =
  case MetaMap.lookup meta (unsolvedMetaMap ctx) of
    Just info ->
      ctx
        { unsolvedMetaMap = MetaMap.delete meta (unsolvedMetaMap ctx),
          solvedMetaMap = MetaMap.insert meta (SolvedMetaInfo info solution) (solvedMetaMap ctx)
        }
    Nothing
      | MetaMap.member meta (solvedMetaMap ctx) ->
          developerError $ "Attempting to solve already solved meta" <+> pretty meta
      | otherwise -> unknownMetaError meta

-- | Looks up the information about a meta-variable, whether solved or not.
lookupMetaInfo ::
  MetaVariableContext builtin ->
  MetaID ->
  MetaInfo builtin
lookupMetaInfo ctx meta =
  case MetaMap.lookup meta (unsolvedMetaMap ctx) of
    Just info -> info
    Nothing -> case MetaMap.lookup meta (solvedMetaMap ctx) of
      Just solved -> solvedMetaInfo solved
      Nothing -> unknownMetaError meta

-- | Looks up the solution to a meta-variable, returning `Nothing` if it is unsolved.
lookupMetaSolution ::
  MetaVariableContext builtin ->
  MetaID ->
  Maybe (GluedExprWithMetas builtin)
lookupMetaSolution ctx meta =
  case MetaMap.lookup meta (solvedMetaMap ctx) of
    Just solved -> Just (metaSolution solved)
    Nothing
      | MetaMap.member meta (unsolvedMetaMap ctx) -> Nothing
      | otherwise -> unknownMetaError meta

-- | Whether or not the meta-variable has been solved. O(min(n, W)).
isMetaSolved :: MetaVariableContext builtin -> MetaID -> Bool
isMetaSolved ctx meta = MetaMap.member meta (solvedMetaMap ctx)

-- | Replaces the type of an existing meta-variable.
setMetaType ::
  MetaID ->
  Type builtin ->
  MetaVariableContext builtin ->
  MetaVariableContext builtin
setMetaType meta typ ctx = do
  let setType info = info {metaType = typ}
  let setSolvedType solved = solved {solvedMetaInfo = setType (solvedMetaInfo solved)}
  ctx
    { unsolvedMetaMap = MetaMap.adjust setType meta (unsolvedMetaMap ctx),
      solvedMetaMap = MetaMap.adjust setSolvedType meta (solvedMetaMap ctx)
    }

-- | Updates the type and, if it exists, the solution of every meta-variable.
-- Each function is also passed the original information about the meta.
traverseMetaTypesAndSolutions ::
  (Monad m) =>
  (MetaInfo builtin -> Type builtin -> m (Type builtin)) ->
  (MetaInfo builtin -> GluedExprWithMetas builtin -> m (GluedExprWithMetas builtin)) ->
  MetaVariableContext builtin ->
  m (MetaVariableContext builtin)
traverseMetaTypesAndSolutions updateType updateSolution ctx = do
  let updateInfo info = do
        newType <- updateType info (metaType info)
        return $ info {metaType = newType}
  let updateSolved (SolvedMetaInfo info solution) =
        SolvedMetaInfo <$> updateInfo info <*> updateSolution info solution
  unsolvedMetaMap' <- traverse updateInfo (unsolvedMetaMap ctx)
  solvedMetaMap' <- traverse updateSolved (solvedMetaMap ctx)
  return $ ctx {unsolvedMetaMap = unsolvedMetaMap', solvedMetaMap = solvedMetaMap'}

-- | The information about each unsolved meta-variable.
unsolvedMetaInfos :: MetaVariableContext builtin -> MetaMap (MetaInfo builtin)
unsolvedMetaInfos = unsolvedMetaMap

-- | The information about, and solution to, each solved meta-variable.
solvedMetaInfos :: MetaVariableContext builtin -> MetaMap (SolvedMetaInfo builtin)
solvedMetaInfos = solvedMetaMap

-- | The set of solved meta-variables. Note this is O(n) so to check whether
-- an individual meta-variable is solved use `isMetaSolved` instead.
solvedMetaIDs :: MetaVariableContext builtin -> MetaSet
solvedMetaIDs = MetaMap.keys . solvedMetaMap

unknownMetaError :: MetaID -> a
unknownMetaError meta =
  developerError $
    "Requesting info for unknown meta"
      <+> pretty meta
      <+> "not in context"
