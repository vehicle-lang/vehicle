module Vehicle.Prelude.Warning where

import Data.Hashable (Hashable (..))
import Data.Set (Set)
import Vehicle.Backend.Prelude (Target)
import Vehicle.Backend.Queries.Variable
import Vehicle.Resource (ExternalResource)
import Vehicle.Syntax.AST
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core

-- | Exhaustive list of warnings thrown by the Vehicle compiler.
data CompileWarning
  = UnusedResource ExternalResource (Set Name)
  | ResourcesUnnecessariyProvidedForBackend Target [(ExternalResource, Name)]
  | ResortingtoFMElimination Name (Set MixedVariable)
  | TrivialProperty PropertyAddress Bool
  | UnsoundStrictOrderConversion Name QueryFormatID
  | AllConstantInputsMarabouBug Name
  | UnderSpecifiedNetworkInputs Name QueryFormatID [(NetworkVariable, UnderConstrainedVariableStatus)]

-- | Used to prevent duplicate warnings being thrown. Return `Nothing` if duplicates are fine,
-- otherwise return the hash of the relevant parts of the warning for which different values
-- should return multiple warnings.
warningDuplicateDetectionHash :: CompileWarning -> Maybe Int
warningDuplicateDetectionHash w = case w of
  UnusedResource {} -> Nothing
  ResourcesUnnecessariyProvidedForBackend {} -> Nothing
  TrivialProperty {} -> Nothing
  ResortingtoFMElimination name _vars -> Just $ hash (1 :: Int, name)
  UnsoundStrictOrderConversion name _format -> Just $ hash (2 :: Int, name)
  AllConstantInputsMarabouBug name -> Just $ hash (3 :: Int, name)
  UnderSpecifiedNetworkInputs name _format _vars -> Just $ hash (4 :: Int, name)
