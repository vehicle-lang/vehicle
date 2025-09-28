module Vehicle.Data.Variable.Free.Context
  ( module X,
    mkDeclCtxEntry,
    addDeclToContext,
  )
where

import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Data.Variable.Free.Context.Class as X
import Vehicle.Data.Variable.Free.Context.Core as X
import Vehicle.Data.Variable.Free.Context.Instance as X

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
