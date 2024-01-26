module Vehicle.Backend.Queries.Error
  ( diagnoseNonLinearity,
    diagnoseAlternatingQuantifiers,
  )
where

import Control.Monad.Except (MonadError (..))
import Vehicle.Backend.Queries.Error.Linearity
import Vehicle.Backend.Queries.Error.Polarity
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Type.Subsystem (typeCheckWithSubsystem)
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Verify.QueryFormat.Core (QueryFormatID)

diagnoseNonLinearity ::
  forall m.
  (MonadCompile m) =>
  QueryFormatID ->
  Prog Ix Builtin ->
  DeclProvenance ->
  m CompileError
diagnoseNonLinearity queryFormat prog propertyProv@(propertyIdentifier, _) = do
  setCallDepth 0
  logDebug MinDetail $
    "ERROR: found non-linear property. Switching to linearity type-checking mode for"
      <+> quotePretty propertyIdentifier
      <> line

  Main typedDecls <- typeCheckWithSubsystem mempty handleUnexpectedError prog

  -- Extract and diagnose the type.
  let property = head $ filter (\decl -> identifierOf decl == propertyIdentifier) typedDecls
  let propertyType = typeOf property
  case propertyType of
    LinearityExpr _ (NonLinear source) -> do
      throwError $ UnsupportedNonLinearConstraint queryFormat propertyProv (Right source)
    _ -> handleUnexpectedError (DevError $ "Unexpected linearity type for property" <+> quotePretty propertyIdentifier)
  where
    handleUnexpectedError :: (MonadCompile m) => CompileError -> m a
    handleUnexpectedError err =
      throwError $ UnsupportedNonLinearConstraint queryFormat propertyProv (Left err)

diagnoseAlternatingQuantifiers ::
  forall m.
  (MonadCompile m) =>
  QueryFormatID ->
  Prog Ix Builtin ->
  DeclProvenance ->
  m CompileError
diagnoseAlternatingQuantifiers queryFormat prog propertyProv@(propertyIdentifier, _) = do
  setCallDepth 0
  logDebug MinDetail $
    "ERROR: found property with alterating quantifiers. Switching to polarity type-checking mode for"
      <+> quotePretty propertyIdentifier
      <> line

  Main typedDecls <- typeCheckWithSubsystem mempty handleUnexpectedError prog

  -- Extract and diagnose the type.
  let property = head $ filter (\decl -> identifierOf decl == propertyIdentifier) typedDecls
  let propertyType = typeOf property
  case propertyType of
    PolarityExpr _ (MixedSequential q p pp2) -> do
      throwError $ UnsupportedAlternatingQuantifiers queryFormat propertyProv (Right (q, p, pp2))
    _ -> compilerDeveloperError $ "Unexpected polarity type for property" <+> quotePretty propertyIdentifier <> ":" <+> prettyVerbose propertyType
  where
    handleUnexpectedError :: (MonadCompile m) => CompileError -> m a
    handleUnexpectedError err =
      throwError $ UnsupportedAlternatingQuantifiers queryFormat propertyProv (Left err)
