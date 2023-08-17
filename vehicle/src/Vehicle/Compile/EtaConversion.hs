module Vehicle.Compile.EtaConversion
  ( etaExpandProg,
  )
where

import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Prelude.MonadContext
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Type.Subsystem.Standard.Interface
import Vehicle.Expr.DeBruijn (Ix, liftDBIndices)
import Vehicle.Expr.Normalised (VType, Value (..))

etaExpandProg ::
  forall m builtin.
  (MonadCompile m, PrintableBuiltin builtin, HasStandardData builtin) =>
  Prog Ix builtin ->
  m (Prog Ix builtin)
etaExpandProg (Main ds) =
  runContextT @m @builtin (Main <$> etaExpandDecls ds)

etaExpandDecls ::
  (MonadContext builtin m, PrintableBuiltin builtin) =>
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
  (MonadContext builtin m, PrintableBuiltin builtin) =>
  Identifier ->
  Type Ix builtin ->
  Expr Ix builtin ->
  m (Expr Ix builtin)
etaExpand declIdent originalType originalBody = do
  normType <- normalise originalType
  go normType originalBody
  where
    go :: VType builtin -> Expr Ix builtin -> m (Expr Ix builtin)
    go typ body = case (typ, body) of
      (VPi _ piBody, Lam p lamBinder lamBody) -> Lam p lamBinder <$> go piBody lamBody
      (VPi piBinder piBody, _) -> do
        unnormPiBinder <- traverse unnormalise piBinder
        lamBinder <- piBinderToLamBinder unnormPiBinder
        let liftedBody = liftDBIndices 1 body
        let p = provenanceOf body
        let appliedBody = normAppList p liftedBody [argFromBinder lamBinder (BoundVar p 0)]
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
