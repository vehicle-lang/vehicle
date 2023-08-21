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
import Vehicle.Compile.Type.Subsystem (typeCheckWithSubsystem)
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Verify.QueryFormat.Core (QueryFormatID)

diagnoseNonLinearity ::
  forall m.
  (MonadCompile m) =>
  QueryFormatID ->
  Prog Ix Builtin ->
  Identifier ->
  m CompileError
diagnoseNonLinearity queryFormat prog propertyIdentifier = do
  setCallDepth 0
  logDebug MinDetail $
    "ERROR: found non-linear property. Switching to linearity type-checking mode for"
      <+> quotePretty propertyIdentifier
      <> line

  Main typedDecls <- typeCheckWithSubsystem mempty prog

  -- Extract and diagnose the type.
  let property = head $ filter (\decl -> identifierOf decl == propertyIdentifier) typedDecls
  let propertyType = typeOf property
  case propertyType of
    LinearityExpr _ (NonLinear p pp1 pp2) -> do
      let propertyProv = (propertyIdentifier, provenanceOf property)
      throwError $ UnsupportedNonLinearConstraint queryFormat propertyProv p pp1 pp2
    _ -> compilerDeveloperError $ "Unexpected linearity type for property" <+> quotePretty propertyIdentifier

diagnoseAlternatingQuantifiers ::
  forall m.
  (MonadCompile m) =>
  QueryFormatID ->
  Prog Ix Builtin ->
  Identifier ->
  m CompileError
diagnoseAlternatingQuantifiers queryFormat prog propertyIdentifier = do
  setCallDepth 0
  logDebug MinDetail $
    "ERROR: found property with alterating quantifiers. Switching to polarity type-checking mode for"
      <+> quotePretty propertyIdentifier
      <> line

  Main typedDecls <- typeCheckWithSubsystem mempty prog

  -- Extract and diagnose the type.
  let property = head $ filter (\decl -> identifierOf decl == propertyIdentifier) typedDecls
  let propertyType = typeOf property
  case propertyType of
    PolarityExpr _ (MixedSequential p pp1 pp2) -> do
      let propertyProv = (propertyIdentifier, provenanceOf property)
      throwError $ UnsupportedAlternatingQuantifiers queryFormat propertyProv p pp1 pp2
    _ -> compilerDeveloperError $ "Unexpected polarity type for property" <+> quotePretty propertyIdentifier
