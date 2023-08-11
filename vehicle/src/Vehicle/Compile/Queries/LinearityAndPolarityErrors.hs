module Vehicle.Compile.Queries.LinearityAndPolarityErrors
  ( diagnoseNonLinearity,
    diagnoseAlternatingQuantifiers,
  )
where

import Control.Monad.Except (MonadError (..))
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Subsystem (typeCheckWithSubsystem)
import Vehicle.Compile.Type.Subsystem.Linearity
import Vehicle.Compile.Type.Subsystem.Polarity
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Expr.Normalised (GluedExpr (..))
import Vehicle.Verify.Core (QueryFormatID)

diagnoseNonLinearity ::
  forall m.
  (MonadCompile m) =>
  QueryFormatID ->
  StandardGluedProg ->
  Identifier ->
  m CompileError
diagnoseNonLinearity queryFormat prog propertyIdentifier = do
  setCallDepth 0
  logDebug MinDetail $
    "ERROR: found non-linear property. Switching to linearity type-checking mode for"
      <+> quotePretty propertyIdentifier
      <> line

  let unnormalisedProg = fmap unnormalised prog
  Main typedDecls <- typeCheckWithSubsystem mempty unnormalisedProg

  -- Extract and diagnose the type.
  let property = head $ filter (\decl -> identifierOf decl == propertyIdentifier) typedDecls
  let propertyType = unnormalised $ typeOf property
  case propertyType of
    LinearityExpr _ (NonLinear p pp1 pp2) -> do
      let propertyProv = (propertyIdentifier, provenanceOf property)
      throwError $ UnsupportedNonLinearConstraint queryFormat propertyProv p pp1 pp2
    _ -> compilerDeveloperError $ "Unexpected linearity type for property" <+> quotePretty propertyIdentifier

diagnoseAlternatingQuantifiers ::
  forall m.
  (MonadCompile m) =>
  QueryFormatID ->
  StandardGluedProg ->
  Identifier ->
  m CompileError
diagnoseAlternatingQuantifiers queryFormat prog propertyIdentifier = do
  setCallDepth 0
  logDebug MinDetail $
    "ERROR: found property with alterating quantifiers. Switching to polarity type-checking mode for"
      <+> quotePretty propertyIdentifier
      <> line

  let unnormalisedProg = fmap unnormalised prog
  Main typedDecls <- typeCheckWithSubsystem mempty unnormalisedProg

  -- Extract and diagnose the type.
  let property = head $ filter (\decl -> identifierOf decl == propertyIdentifier) typedDecls
  let propertyType = unnormalised $ typeOf property
  case propertyType of
    PolarityExpr _ (MixedSequential p pp1 pp2) -> do
      let propertyProv = (propertyIdentifier, provenanceOf property)
      throwError $ UnsupportedAlternatingQuantifiers queryFormat propertyProv p pp1 pp2
    _ -> compilerDeveloperError $ "Unexpected polarity type for property" <+> quotePretty propertyIdentifier
