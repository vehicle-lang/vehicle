{-# LANGUAGE CPP #-}

module Vehicle.Verify.Solver where

# ifdef mingw32_HOST_OS
# else
import System.Posix.Signals
# endif

import Control.Monad.Error.Class (MonadError (..))
import Data.Map (Map)
import Data.Version (Version)
import Vehicle.Compile.Prelude hiding (Solver)
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core
import Vehicle.Verify.Specification.Status

--------------------------------------------------------------------------------
-- Solver interface

type SolverName = String

-- | Location of the solver executable file
type SolverExecutable = FilePath

-- | The type of methods that prepare the command line arguments for the solver
type PrepareSolverArgs =
  MetaNetwork -> QueryFile -> [String]

-- | A (satisfying) assignment to a set of reduced network-level variables.
newtype QueryVariableAssignment
  = QueryVariableAssignment (Map QueryVariable Rational)

-- | The type of methods that parse the output of the solver.
type ParseSolverOutput =
  forall m.
  (MonadError SolverError m, MonadLogger m) =>
  String ->
  m (QueryResult QueryVariableAssignment)

-- | A complete solver implementation
data Solver = Solver
  { -- | The name that the solver reports
    solverName :: String,
    -- | Solver version
    solverVersion :: Version,
    -- | The query format that the solver accepts
    solverQueryFormatID :: QueryFormatID,
    -- | The executable file for the solver
    solverExecutable :: SolverExecutable,
    -- | Prepare the command line arguments for the solver.
    prepareArgs :: PrepareSolverArgs,
    -- | Parse the output of the solver.
    parseOutput :: ParseSolverOutput,
    -- | Does the solver support multiple network applications?
    supportsMultipleNetworkApplications :: Bool
  }

--------------------------------------------------------------------------------
-- Error messages

data VerificationErrorAction = VerificationErrorAction
  { reproducerIsUseful :: Bool,
    verificationErrorMessage :: Doc ()
  }

convertVerificationError :: Solver -> QueryAddress -> SolverError -> VerificationErrorAction
convertVerificationError Solver {..} (QueryAddress propertyAddress queryID) = \case
  SolverError errorMessage ->
    VerificationErrorAction
      { reproducerIsUseful = True,
        verificationErrorMessage = do
          "while verifying query"
            <+> pretty queryID
            <+> "of property"
            <+> quotePretty propertyAddress
            <> ","
              <+> solverDoc
              <+> "threw the error:"
            <> line
            <> line
            <> indent 2 (pretty errorMessage)
      }
  SolverTerminatedByOS signal ->
    exitFailureReason signal
  SolverOutputMalformed message ->
    VerificationErrorAction
      { reproducerIsUseful = True,
        verificationErrorMessage = "Unexpected output from" <+> solverDoc <> "." <+> message
      }
  SolverTimedOut ->
    VerificationErrorAction
      { reproducerIsUseful = False,
        verificationErrorMessage = "Verification timed out"
      }
  SolverIncompleteWitness missingVariables ->
    VerificationErrorAction
      { reproducerIsUseful = True,
        verificationErrorMessage =
          "The witness provided from"
            <+> solverDoc
            <+> "was incomplete."
            <+> "In particular, values for the following variables were not provided:"
            <> line
            <> indent 2 (prettySet pretty missingVariables)
      }
  where
    solverDoc = pretty solverName

    exitFailureReason :: Int -> VerificationErrorAction
# ifdef mingw32_HOST_OS
    exitFailureReason exitValue = VerificationErrorAction
        { reproducerIsUseful = True
        , verificationErrorMessage = basicExitFailureMessage solverID exitValue <+>
            "Vehicle is unable to interpret this error code on Windows but the most common reasons" <+>
            "are the" <+> solverDoc <+> "either ran out of memory or performed an illegal instruction."
        }
# else
    exitFailureReason exitValue
      | toEnum exitValue == illegalInstruction = VerificationErrorAction
        { reproducerIsUseful = True
        , verificationErrorMessage = basicExitFailureMessage solverName exitValue <+>
            "This is an `Illegal Instruction` error and indicates a bug in" <+> solverDoc <+> "itself"
        }
      | toEnum exitValue == killProcess = VerificationErrorAction
        { reproducerIsUseful = True
        , verificationErrorMessage = basicExitFailureMessage solverName exitValue <+>
            "This is often (but not always) a result of" <+> solverDoc <+> "running out of memory."
        }
      | otherwise = VerificationErrorAction
        { reproducerIsUseful = True
        , verificationErrorMessage = basicExitFailureMessage solverName exitValue <+>
            "Please consult the manual on Unix signals to work out what this means."
        }
# endif

basicExitFailureMessage :: SolverExecutable -> Int -> Doc a
basicExitFailureMessage solver exitValue =
  pretty solver
    <+> "was killed with the signal"
    <+> quotePretty exitValue
    <> "."
