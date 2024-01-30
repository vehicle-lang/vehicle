module Vehicle.Compile.Context.Free
  ( module X,
    addDeclToContext,
    mkDeclCtxEntry,
  )
where

import Vehicle.Compile.Context.Free.Class as X
import Vehicle.Compile.Context.Free.Core as X
import Vehicle.Compile.Context.Free.Instance as X
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Data.NormalisedExpr

mkDeclCtxEntry :: (MonadFreeContext builtin m) => Decl Ix builtin -> m (WHNFDecl builtin, Type Ix builtin)
mkDeclCtxEntry decl = do
  normDecl <- traverse normaliseInEmptyEnv decl
  return (normDecl, typeOf decl)

addDeclToContext :: (MonadFreeContext builtin m) => Decl Ix builtin -> m a -> m a
addDeclToContext decl k = do
  normDecl <- traverse normaliseInEmptyEnv decl
  addDeclEntryToContext (normDecl, typeOf decl) k
