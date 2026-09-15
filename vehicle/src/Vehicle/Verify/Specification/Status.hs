module Vehicle.Verify.Specification.Status where

import Data.Set (Set)
import Vehicle.Compile.Prelude
import Vehicle.Data.MaybeTrivial (MaybeTrivial (..))
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core (QueryVariable)
import Vehicle.Verify.Specification (QueryMetaData (..))

class IsVerified a where
  isVerified :: a -> Bool

instance (IsVerified a) => IsVerified (MaybeTrivial a) where
  isVerified = \case
    Trivial b -> b
    NonTrivial r -> isVerified r

instance IsVerified (QueryResult witness) where
  isVerified = \case
    SAT {} -> True
    UnSAT -> False

evaluateQuery :: QuerySetNegationStatus -> QueryResult witness -> Bool
evaluateQuery negated q = negated `xor` isVerified q

--------------------------------------------------------------------------------
-- Verification status of a single property

-- | Errors thrown by a solver
data SolverError
  = SolverTerminatedByOS Int
  | SolverError String
  | SolverOutputMalformed (Doc ())
  | SolverIncompleteWitness (Set QueryVariable)
  | SolverTimedOut
  deriving (Show)

isTimeoutError :: SolverError -> Bool
isTimeoutError = \case
  SolverTimedOut -> True
  _ -> False

data PropertyStatus
  = PropertyCompleted (MaybeTrivial (QuerySetNegationStatus, QueryResult UserVariableAssignment))
  | PropertyErrored (QueryMetaData, SolverError)

instance IsVerified PropertyStatus where
  isVerified = \case
    PropertyCompleted maybeResult -> case maybeResult of
      Trivial b -> b
      NonTrivial (negated, result) -> evaluateQuery negated result
    PropertyErrored {} -> False
