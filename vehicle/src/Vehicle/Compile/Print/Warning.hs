{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Print.Warning where

import Data.Set qualified as Set
import Vehicle.Backend.Prelude (Target (..))
import Vehicle.Backend.Queries.Variable
import Vehicle.Prelude
import Vehicle.Prelude.Warning
import Vehicle.Syntax.AST
import Vehicle.Verify.Core

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
        <+> "was found to evalute to"
        <+> quotePretty status
        <+> "without needing to call the verifier. This usually indicates a fault with either the"
        <+> "specification or any external datasets used."
    UnsoundStrictOrderConversion propertyName queryFormatID ->
      "While compiling property"
        <+> quotePretty propertyName
        <> ", at least one of the generated SAT problems was found"
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
    AllConstantInputsMarabouBug propertyName ->
      "While compiling property"
        <+> quotePretty propertyName
        <> ", in at least one of the generated SAT problems"
          <+> "all network inputs were fixed to be constants."
          <+> "Unfortunately there is a known bug in Marabou that it sometimes erroneously returns"
          <+> "'unsat' for these type of degenerate problems."
        <> line
        <> line
        <> "See https://github.com/NeuralNetworkVerification/Marabou/issues/670 for details."
    UnderSpecifiedNetworkInputs propertyName queryFormat underSpecified ->
      "While compiling property"
        <+> quotePretty propertyName
        <> ", in at least one of the generated SAT problems not all network input variables have both"
          <+> "a lower and a upper bound."
          <+> "Such queries are not currently supported by the"
          <+> pretty queryFormat
        <> "."
        <> line
        <> line
        <> "In particular, the following input variables are under-specified:"
        <> line
        <> indent 2 (vsep $ fmap prettyEntry underSpecified)
      where
        prettyEntry :: (NetworkVariable, UnderConstrainedVariableStatus) -> Doc a
        prettyEntry (var, constraint) =
          pretty var <+> "-" <+> case constraint of
            Unconstrained -> "no constraints"
            BoundedAbove -> "bounded above but not below"
            BoundedBelow -> "bounded below but not above"
    ResourcesUnnecessariyProvidedForBackend target resources ->
      "The following provided resources:"
        <> line
        <> line
        <> indent 2 resourceDocs
        <> line
        <> line
        <> "will be ignored as when compiling to" <+> pretty target <+> reasonUnnecessary
      where
        resourceDocs = vsep (fmap (\(r, n) -> pretty r <+> pretty n) resources)
        reasonUnnecessary = case target of
          ITP {} -> "their values will be taken directly from the verification cache."
          ExplicitVehicle -> "their values should be provided upon export."
          LossFunction {} -> "their values should be provided upon export."
          VerifierQueries {} -> developerError "resources are necessary when compiling to verifier queries"
