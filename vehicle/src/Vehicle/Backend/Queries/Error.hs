module Vehicle.Backend.Queries.Error
  ( diagnoseNonLinearity,
    diagnoseAlternatingQuantifiers,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Vehicle.Compile.Error
import Vehicle.Compile.Monomorphisation (MonomorphisationSettings (MonoSettings, isMonomorphisableBinder, keepUnusedDeclaration), monomorphise)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Irrelevance (removeIrrelevantCodeFromProg)
import Vehicle.Compile.Type.Subsystem (linearityTypeCheck, polarityTypeCheck, resolveInstanceArgumentsAndCasts)
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Builtin.Linearity
import Vehicle.Data.Builtin.Linearity.Type ()
import Vehicle.Data.Builtin.Polarity
import Vehicle.Data.Builtin.Polarity.Type ()
import Vehicle.Data.Builtin.Standard
import Vehicle.Verify.QueryFormat.Core (QueryFormatID)

diagnoseNonLinearity ::
  forall m.
  (MonadCompile m) =>
  QueryFormatID ->
  Prog Builtin ->
  DeclProvenance ->
  m CompileError
diagnoseNonLinearity queryFormat prog propertyProv@(propertyIdentifier, _) = do
  errorOrOrigin <- diagnoseSpecIncompatiblility prog propertyIdentifier linearityTypeCheck
  let origin = case errorOrOrigin of
        Left err -> Left err
        Right originType -> case originType of
          Builtin _ (Linearity (NonLinear source)) -> Right source
          _ -> Left $ unexpectedOriginType propertyIdentifier
  return $ UnsupportedNonLinearConstraint queryFormat propertyProv origin

diagnoseAlternatingQuantifiers ::
  forall m.
  (MonadCompile m) =>
  QueryFormatID ->
  Prog Builtin ->
  DeclProvenance ->
  m CompileError
diagnoseAlternatingQuantifiers queryFormat prog propertyProv@(propertyIdentifier, _) = do
  errorOrOrigin <- diagnoseSpecIncompatiblility prog propertyIdentifier polarityTypeCheck
  let origin = case errorOrOrigin of
        Left err -> Left err
        Right originType -> case originType of
          Builtin _ (Polarity (MixedSequential q p pp2)) -> Right (q, p, pp2)
          _ -> Left $ unexpectedOriginType propertyIdentifier
  return $ UnsupportedAlternatingQuantifiers queryFormat propertyProv origin

diagnoseSpecIncompatiblility ::
  (MonadCompile m) =>
  Prog Builtin ->
  Identifier ->
  (Prog Builtin -> m (Either CompileError (Prog builtin))) ->
  m (Either CompileError (Type builtin))
diagnoseSpecIncompatiblility prog propertyIdentifier typeCheck = do
  setCallDepth 0
  logDebug MinDetail $
    "ERROR: found uncompilable property."
      <+> "Switching to linearity type-checking mode for"
      <+> quotePretty propertyIdentifier
      <> line

  monomorphisedProg <-
    monomorphise prog $
      MonoSettings
        { isMonomorphisableBinder = not . isExplicit,
          keepUnusedDeclaration = (== propertyIdentifier)
        }
  irrelevantFreeProg <- removeIrrelevantCodeFromProg monomorphisedProg
  implicitFreeProg <- removeImplicitArgs irrelevantFreeProg
  instanceFreeProg <- resolveInstanceArgumentsAndCasts implicitFreeProg
  errorOrLinearityProg <- typeCheck instanceFreeProg

  case errorOrLinearityProg of
    Left err -> return $ Left err
    Right linearityProg -> Right <$> findDeclType propertyIdentifier linearityProg

removeImplicitArgs ::
  forall m builtin.
  (MonadCompile m, PrintableBuiltin builtin) =>
  Prog builtin ->
  m (Prog builtin)
removeImplicitArgs prog =
  logCompilerSection2 MaxDetail "removal of implicit arguments" $ do
    result <- traverse go prog
    logCompilerPassOutput $ prettyExternal result
    return result
  where
    go :: Expr builtin -> m (Expr builtin)
    go expr = case expr of
      App fun args -> do
        fun' <- go fun
        let nonImplicitArgs = NonEmpty.filter (not . isImplicit) args
        nonImplicitArgs' <- traverse (traverse go) nonImplicitArgs
        return $ normAppList fun' nonImplicitArgs'
      BoundVar {} -> return expr
      FreeVar {} -> return expr
      Universe {} -> return expr
      Meta {} -> return expr
      Hole {} -> return expr
      Builtin {} -> return expr
      Pi p binder res -> Pi p <$> traverse go binder <*> go res
      Lam p binder body -> Lam p <$> traverse go binder <*> go body
      Let p bound binder body -> Let p <$> go bound <*> traverse go binder <*> go body
      Record p ident fields -> Record p ident <$> traverseRecordFields go fields
      RecordAcc p record field -> RecordAcc p <$> go record <*> pure field

findDeclType :: (MonadCompile m) => Identifier -> Prog builtin -> m (Expr builtin)
findDeclType ident (Main decls) = do
  let candidates = filter (\decl -> identifierOf decl == ident) decls
  case candidates of
    [property] -> return $ typeOf property
    _ -> compilerDeveloperError $ "Could not find property" <+> quotePretty ident <+> "in program after subtyping."

unexpectedOriginType :: Identifier -> CompileError
unexpectedOriginType ident =
  DevError $
    "Unexpected secondary type for property" <+> quotePretty ident
