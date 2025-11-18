module Vehicle.Data.Variable.Free.Context
  ( module X,
    mkDeclCtxEntry,
    addDeclToContext,
    traverseNormalisedDecls_,
  )
where

import Data.Proxy (Proxy (..))
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Interface.Normalise
import Vehicle.Data.Code.Value
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

traverseNormalisedDecls_ ::
  forall m builtin.
  (MonadLogger m, NormalisableBuiltin builtin) =>
  (VDecl builtin -> FreeContextT builtin m ()) ->
  Prog builtin ->
  m ()
traverseNormalisedDecls_ f (Main ds) =
  runFreshFreeContextT (Proxy @builtin) $ do
    go ds
  where
    go :: [Decl builtin] -> FreeContextT builtin m ()
    go = \case
      [] -> return ()
      decl : decls -> do
        normDecl <- traverse normaliseInEmptyEnv decl
        _ <- f normDecl
        decls' <- addDeclEntryToContext (decl, normDecl) $ go decls
        return decls'
