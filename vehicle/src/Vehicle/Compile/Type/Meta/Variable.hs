module Vehicle.Compile.Type.Meta.Variable
  ( MetaInfo (..),
    extendMetaCtx,
    makeMetaType,
    getMetaDependencies,
    MetaVariableContext,
    findMetaInfo,
    addMetaSolution,
  )
where

import Data.Maybe (fromMaybe)
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

type MetaVariableContext builtin = MetaMap (MetaInfo builtin)

findMetaInfo :: MetaVariableContext builtin -> MetaID -> MetaInfo builtin
findMetaInfo ctx meta =
  case MetaMap.lookup meta ctx of
    Just info -> info
    Nothing ->
      developerError $
        "Requesting info for unknown meta" <+> pretty meta <+> "not in context"

addMetaSolution :: GluedExprWithMetas builtin -> MetaID -> MetaVariableContext builtin -> MetaVariableContext builtin
addMetaSolution solution = MetaMap.adjust (addSolutionToInfo solution)
