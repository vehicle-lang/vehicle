module Vehicle.Compile.Warning where

import Data.Set (Set)
import Data.Set qualified as Set
import Vehicle.Backend.Queries.Variable
import Vehicle.Prelude
import Vehicle.Resource (ExternalResource)
import Vehicle.Syntax.AST
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core

-- TODO we should really integrate these directly into `MonadLogger` and
-- then implement caching to avoid printing multiple equal warnings out twice
-- and then add the ability to surpress warnings as well.

data CompileWarning
  = UnusedResource ExternalResource (Set Name)
  | ResortingtoFMElimination Name (Set MixedVariable)
  | TrivialProperty PropertyAddress Bool
  | UnsoundStrictOrderConversion Name QueryFormatID

instance Pretty CompileWarning where
  pretty = \case
    UnusedResource resourceType unusedResources ->
      "the following"
        <+> pretty resourceType
        <> "s were provided"
          <+> "but not used by the specification:"
          <+> prettySet unusedResources
    ResortingtoFMElimination propertyName unsolvedVars -> do
      let varsDoc = concatWith (\u v -> u <> "," <+> v) (fmap quotePretty (Set.toList unsolvedVars))
      let pluralisedVarsDoc
            | length unsolvedVars == 1 = "variable" <+> varsDoc <+> "is"
            | otherwise = "variables" <+> varsDoc <+> "are"

      "In property"
        <+> quotePretty (nameOf propertyName)
        <+> "the quantified"
        <+> pluralisedVarsDoc
        <+> "not always directly related to the input or output of a network."
        <+> "This is frequently indicative of a bug in the specification."
    TrivialProperty propertyName status ->
      "The property"
        <+> quotePretty propertyName
        <+> "was found to be"
        <+> pretty status
        <+> "without needing to call the verifier. This usually indicates a fault with either the"
        <+> "specification or any external datasets used."
    UnsoundStrictOrderConversion propertyName queryFormatID ->
      "While compiling property"
        <+> quotePretty propertyName
        <+> "at least one of the generated SAT problems generated was found"
        <+> "to contain a strict inequality (i.e. constraints of the form 'x < y')."
        <+> "Unfortunately the"
        <+> pretty queryFormatID
        <+> "only supports non-strict"
        <+> "inequalities (i.e. constraints of the form 'x <= y')."
        <> line
        <> line
        <> "In order to provide support, Vehicle has automatically converted the"
          <+> "strict inequalities to non-strict inequalites."
          <+> "This is not sound, but errors will be at most the floating point epsilon"
          <+> "used by the verifier, which is usually very small (e.g. 1e-9)."
          <+> "However, this may lead to unexpected behaviour (e.g. loss of the law of excluded middle)."
        <> line
        <> line
        <> "See https://github.com/vehicle-lang/vehicle/issues/74 for further details."
