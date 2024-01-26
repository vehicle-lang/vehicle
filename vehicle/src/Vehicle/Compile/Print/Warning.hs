{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Print.Warning where

import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty, sort)
import Data.Set qualified as Set
import Vehicle.Backend.Prelude (Target (..))
import Vehicle.Prelude
import Vehicle.Prelude.Warning
import Vehicle.Verify.Core
import Vehicle.Verify.Variable

instance Pretty SummarisedCompileWarning where
  pretty = \case
    UnusedResourcesSummary resourceType unusedResources ->
      "the following"
        <+> pretty resourceType
        <> "s were provided"
          <+> "but not used by the specification:"
          <+> prettySet unusedResources
    TrivialPropertySummary propertyName status ->
      "The property"
        <+> quotePretty propertyName
        <+> "was found to evaluate to"
        <+> quotePretty status
        <+> "without needing to call the verifier. This usually indicates a fault with either the"
        <+> "specification or any external datasets used."
    UnnecessaryResourcesProvidedSummary target resources ->
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
    UnderSpecifiedProblemSpaceVariablesSummary propertyAddress unsolvedVars -> do
      let varsDoc = concatWith (\u v -> u <> "," <+> v) (fmap quotePretty (Set.toList unsolvedVars))
      let pluralisedVarsDoc
            | length unsolvedVars == 1 = "variable" <+> varsDoc <+> "is"
            | otherwise = "variables" <+> varsDoc <+> "are"

      "In property"
        <+> quotePretty propertyAddress
        <+> "the quantified"
        <+> pluralisedVarsDoc
        <+> "not always directly related to the input or output of a network."
        <+> "This is frequently indicative of a bug in the specification."
    UnsoundStrictOrderConversionsSummary queryFormatID propertyAddress _ ->
      "In property"
        <+> quotePretty propertyAddress
        <> ", at least one of the generated queries were found"
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
    AllConstantNetworkInputVariablesSummary _ propertyAddress queryIDs ->
      "In property"
        <+> quotePretty propertyAddress
        <> ", in"
          <+> prettyQueryIDs queryIDs
          <+> "all network inputs were fixed to be constants."
          <+> "Unfortunately there is a known bug in Marabou that it sometimes erroneously returns"
          <+> "'unsat' for these type of queries."
        <> line
        <> line
        <> "See https://github.com/NeuralNetworkVerification/Marabou/issues/670 for details."
    UnboundedNetworkInputVariablesSummary queryFormat propertyAddress varsByQueryID ->
      "In property"
        <+> quotePretty propertyAddress
        <> ", the following network input variables do not always have both"
          <+> "a lower and a upper bound."
          <+> "This is not currently supported by the"
          <+> pretty queryFormat
        <> "."
        <> line
        <> indent 2 (vsep $ fmap prettyQueries (sortOn (\(qs, _) -> sort qs) varsByQueryID))
      where
        prettyQueries :: (NonEmpty QueryID, [(NetworkRationalVariable, UnderConstrainedVariableStatus)]) -> Doc a
        prettyQueries (queryIDs, vars) =
          "In" <+> prettyQueryIDs queryIDs
            <> ":"
            <> line
            <> indent 2 (vsep $ fmap prettyVar vars)

        prettyVar :: (NetworkRationalVariable, UnderConstrainedVariableStatus) -> Doc a
        prettyVar (var, constraint) =
          pretty var <+> "-" <+> case constraint of
            Unconstrained -> "no lower or upper bound"
            BoundedAbove -> "no lower bound"
            BoundedBelow -> "no upper bound"

prettyQueryIDs :: NonEmpty QueryID -> Doc a
prettyQueryIDs ids =
  (if length ids == 1 then "query" else "queries")
    <+> prettyNonEmptyList (fmap pretty (sort ids))
