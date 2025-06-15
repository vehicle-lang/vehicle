module Vehicle.Compile.Type.Meta
  ( MetaSet,
    MetaVariableContext,
    MetaInfo (..),
    extendMetaCtx,
    HasMetas (..),
    makeMetaType,
    getMetaDependencies,
    findMetaInfo,
    findUltimateUnsolvedMeta,
  )
where

import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Meta.Set (MetaSet)
import Vehicle.Compile.Type.Meta.Variable
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Code.Value (GluedExpr (..))

findUltimateUnsolvedMeta ::
  (MonadLogger m, PrintableBuiltin builtin) =>
  MetaVariableContext builtin ->
  MetaID ->
  m MetaID
findUltimateUnsolvedMeta ctx meta = do
  let metaInfo = findMetaInfo ctx meta
  maybeNextMeta <- case metaSolution metaInfo of
    Just solution -> findMetaInSolution ctx $ unnormalised solution
    Nothing -> return Nothing

  case maybeNextMeta of
    Just nextMeta -> do
      logDebug MaxDetail $ "Found" <+> pretty nextMeta
      findUltimateUnsolvedMeta ctx nextMeta
    _ -> return meta

findMetaInSolution ::
  (MonadLogger m, PrintableBuiltin builtin) =>
  MetaVariableContext builtin ->
  Expr builtin ->
  m (Maybe MetaID)
findMetaInSolution ctx = \case
  Lam _ _ body -> findMetaInSolution ctx body
  Meta _ m -> return $ Just m
  App (Meta _ m) args -> do
    let metaInfo = findMetaInfo ctx m
    return $
      if length (metaCtx metaInfo) == length args
        then return m
        else Nothing
  _e -> return Nothing
