module Vehicle.Compile.Context.Free
  ( module X,
    addDeclToContext,
    mkDeclCtxEntry,
    appHiddenStdlibDef,
  )
where

import Data.Proxy
import Vehicle.Compile.Context.Free.Class as X
import Vehicle.Compile.Context.Free.Core as X
import Vehicle.Compile.Context.Free.Instance as X
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Data.NormalisedExpr
import Vehicle.Libraries.StandardLibrary.Definitions

mkDeclCtxEntry :: (MonadFreeContext builtin m) => Decl Ix builtin -> m (WHNFDecl builtin, Type Ix builtin)
mkDeclCtxEntry decl = do
  normDecl <- traverse normaliseInEmptyEnv decl
  return (normDecl, typeOf decl)

addDeclToContext :: (MonadFreeContext builtin m) => Decl Ix builtin -> m a -> m a
addDeclToContext decl k = do
  normDecl <- traverse normaliseInEmptyEnv decl
  addDeclEntryToContext (normDecl, typeOf decl) k

appHiddenStdlibDef :: forall builtin m. (MonadFreeContext builtin m) => StdLibFunction -> WHNFSpine builtin -> m (WHNFValue builtin)
appHiddenStdlibDef fn spine = do
  (fnDef, _) <- getHiddenStdLibDecl (Proxy @builtin) fn
  case bodyOf fnDef of
    Just fnBody -> normaliseApp fnBody spine
    Nothing -> developerError $ "Unexpected found" <+> quotePretty fn <+> "to have no body"
