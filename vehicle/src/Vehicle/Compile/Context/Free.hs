module Vehicle.Compile.Context.Free
  ( module X,
    appHiddenStdlibDef,
    mkDeclCtxEntry,
    addDeclToContext,
  )
where

import Data.Proxy
import Vehicle.Compile.Context.Free.Class as X
import Vehicle.Compile.Context.Free.Core as X
import Vehicle.Compile.Context.Free.Instance as X
import Vehicle.Compile.Normalise.Builtin (NormalisableBuiltin)
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Data.Code.Value
import Vehicle.Libraries.StandardLibrary.Definitions

appHiddenStdlibDef ::
  forall builtin m.
  (MonadFreeContext builtin m, NormalisableBuiltin builtin) =>
  StdLibFunction ->
  Spine builtin ->
  m (Value builtin)
appHiddenStdlibDef fn spine = do
  (_fnDef, normBody) <- getHiddenStdLibDecl (Proxy @builtin) fn
  case bodyOf normBody of
    Just fnBody -> normaliseApp fnBody spine
    Nothing -> developerError $ "Unexpected found" <+> quotePretty fn <+> "to have no body"

mkDeclCtxEntry ::
  (MonadLogger m, MonadFreeContext builtin m, NormalisableBuiltin builtin) =>
  Proxy normBuiltin ->
  Decl builtin ->
  m (FreeCtxEntry builtin)
mkDeclCtxEntry _ decl = do
  normDecl <- traverse normaliseInEmptyEnv decl
  return (decl, normDecl)

addDeclToContext ::
  (MonadLogger m, MonadFreeContext builtin m, NormalisableBuiltin builtin) =>
  Proxy normBuiltin ->
  Decl builtin ->
  m a ->
  m a
addDeclToContext proxy decl cont = do
  declEntry <- mkDeclCtxEntry proxy decl
  addDeclEntryToContext declEntry cont
