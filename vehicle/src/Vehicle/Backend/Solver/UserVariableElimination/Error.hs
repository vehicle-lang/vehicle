module Vehicle.Backend.Solver.UserVariableElimination.Error
  ( diagnoseNonLinearity,
    diagnoseAlternatingQuantifiers,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Set (Set)
import Data.Set qualified as Set (singleton)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Subsystem (linearityTypeCheck, polarityTypeCheck)
import Vehicle.Data.Builtin.Linearity
import Vehicle.Data.Builtin.Linearity.Type ()
import Vehicle.Data.Builtin.Polarity
import Vehicle.Data.Builtin.Polarity.Type ()
import Vehicle.Data.Builtin.Standard
import Vehicle.Verify.QueryFormat.Core (QueryFormatID)

diagnoseNonLinearity ::
  forall m.
  (MonadCompile m, MonadIO m) =>
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
  (MonadCompile m, MonadIO m) =>
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
  (Prog Builtin -> Set Identifier -> m (Either CompileError (Prog builtin))) ->
  m (Either CompileError (Type builtin))
diagnoseSpecIncompatiblility prog propertyIdentifier typeCheckFn = do
  setCallDepth 0
  logDebug MinDetail $
    "ERROR: found uncompilable property."
      <+> "Switching to diagnostic type-checking mode for"
      <+> quotePretty propertyIdentifier
      <> line

  logCompilerPass QueryError $ do
    errorOrLinearityProg <- typeCheckFn prog (Set.singleton propertyIdentifier)
    case errorOrLinearityProg of
      Left err -> return $ Left err
      Right linearityProg -> Right <$> findDeclType propertyIdentifier linearityProg

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
