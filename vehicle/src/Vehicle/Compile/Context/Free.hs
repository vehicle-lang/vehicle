module Vehicle.Compile.Context.Free
  ( module X,
    addDeclToContext,
  )
where

import Vehicle.Compile.Context.Free.Class as X
import Vehicle.Compile.Context.Free.Core as X
import Vehicle.Compile.Context.Free.Instance as X
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude

addDeclToContext :: (MonadFreeContext builtin m) => Decl Ix builtin -> m a -> m a
addDeclToContext decl k = do
  normDecl <- traverse normaliseInEmptyEnv decl
  addDeclEntryToContext (normDecl, typeOf decl) k
