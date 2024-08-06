module Vehicle.Compile.EtaConversion
  ( -- etaExpandProg,
  -- etaExpand,
  )
where

{-
import Data.Data (Proxy (..))
import Vehicle.Compile.Context.Bound.Class
import Vehicle.Compile.Context.Free
import Vehicle.Compile.Context.Var
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (normaliseInEmptyEnv)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.DeBruijn (liftDBIndices)
import Vehicle.Data.Expr.Normalised

etaExpandProg ::
  forall m builtin.
  (MonadCompile m, PrintableBuiltin builtin, HasStandardData builtin) =>
  Prog Ix builtin ->
  m (Prog Ix builtin)
etaExpandProg (Main ds) =
  runFreshVarContextT (Proxy @builtin) (Main <$> etaExpandDecls ds)

etaExpandDecls ::
  (MonadVarContext builtin m, PrintableBuiltin builtin) =>
  [Decl Ix builtin] ->
  m [Decl Ix builtin]
etaExpandDecls = \case
  [] -> return []
  decl : decls -> do
    newDecl <- case decl of
      DefAbstract {} -> return decl
      DefFunction p ident anns t e -> do
        e' <- etaExpand ident t e
        return $ DefFunction p ident anns t e'

    newDecls <- addDeclToContext newDecl $ etaExpandDecls decls
    return $ newDecl : newDecls

etaExpand ::
  forall m builtin.
  (MonadFreeContext builtin m, MonadCompile m) =>
  Identifier ->
  Type Ix builtin ->
  Expr Ix builtin ->
  m (Expr Ix builtin)
etaExpand declIdent originalType originalBody = do
  normType <- normaliseInEmptyEnv originalType
  runFreshBoundContextT (Proxy @builtin) $ go normType originalBody
  where
    go ::
      forall n.
      (MonadBoundContext builtin n, MonadCompile n) =>
      WHNFType builtin ->
      Expr Ix builtin ->
      n (Expr Ix builtin)
    go typ body = case (typ, body) of
      (VPi _ piBody, Lam p lamBinder lamBody) -> do
        body' <- addBinderToContext lamBinder (go piBody lamBody)
        return $ Lam p lamBinder body'
      (VPi piBinder piBody, _) -> do
        unnormPiBinder <- traverse unnormalise piBinder
        lamBinder <- piBinderToLamBinder unnormPiBinder
        let liftedBody = liftDBIndices 1 body
        let p = provenanceOf body
        let appliedBody = normAppList liftedBody [argFromBinder lamBinder (BoundVar p 0)]
        recBody <- addBinderToContext lamBinder $ go piBody appliedBody
        return $ Lam p lamBinder recBody
      (_, Lam {}) ->
        compilerDeveloperError $
          "More lambdas found than pis during" <+> currentPass
            <> "of" <+> quotePretty declIdent
            <> ":"
            <> line
            <> indent
              2
              ( "type:" <+> prettyVerbose originalType
                  <> line
                  <> "body:" <+> prettyVerbose originalBody
                  <> line
                  <> "problematic-type:" <+> prettyVerbose typ
                  <> line
                  <> "problematic-body" <+> prettyVerbose body
              )
      (_, _) -> return body

currentPass :: CompilerPass
currentPass = "eta-expansion"
-}
