module Vehicle.Compile.Context.Free
  ( module X,
    mkDeclCtxEntry,
    addDeclToContext,
  )
where

import Vehicle.Compile.Context.Free.Class as X
import Vehicle.Compile.Context.Free.Core as X
import Vehicle.Compile.Context.Free.Instance as X
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude

mkDeclCtxEntry ::
  (MonadLogger m, MonadFreeContext builtin m, NormalisableBuiltin builtin) =>
  Decl builtin ->
  m (FreeCtxEntry builtin)
mkDeclCtxEntry decl = do
  normDecl <- traverse normaliseInEmptyEnv decl
  return (decl, normDecl)

addDeclToContext ::
  (MonadLogger m, MonadFreeContext builtin m, NormalisableBuiltin builtin) =>
  Decl builtin ->
  m a ->
  m a
addDeclToContext decl cont = do
  declEntry <- mkDeclCtxEntry decl
  addDeclEntryToContext declEntry cont
