module Vehicle.Compile.Warning where

import Data.Set (Set)
import Data.Set qualified as Set
import Vehicle.Compile.Queries.Variable
import Vehicle.Prelude
import Vehicle.Resource (ExternalResource)
import Vehicle.Syntax.AST

-- TODO we should really integrate these directly into `MonadLogger` and
-- then implement caching to avoid printing multiple equal warnings out twice
-- and then add the ability to surpress warnings as well.

data CompileWarning
  = UnusedResource ExternalResource (Set Name)
  | ResortingtoFMElimination Name (Set MixedVariable)

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
