module Vehicle.Verify.Solver.VNNLIB
  ( vnnlibSolver,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.List (findIndex)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Version (Version)
import Text.Read (readMaybe)
import Vehicle.Compile.Prelude hiding (Solver)
import Vehicle.Data.Tensor (TensorShape, allMultiIndices)
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core
import Vehicle.Verify.QueryFormat.VNNLIB (compileNetworkName)
import Vehicle.Verify.Solver
import Vehicle.Verify.Specification.Status

--------------------------------------------------------------------------------
-- The main interface

vnnlibSolver :: SolverExecutable -> String -> Version -> Solver
vnnlibSolver solverExecutable solverName solverVersion =
  Solver
    { solverName = solverName,
      solverVersion = solverVersion,
      solverQueryFormatID = VNNLibQueries,
      solverExecutable = solverExecutable,
      prepareArgs = prepareSolverArgs,
      parseOutput = parseSolverOutput,
      supportsMultipleNetworkApplications = False
    }

prepareSolverArgs :: PrepareSolverArgs
prepareSolverArgs metaNetwork queryFile = [queryFile] <> networkArgs
  where
    networkArgs = flip concatMap metaNetwork $ \(name, info, n) ->
      concatMap (\i -> networkArg name (networkFilepath info) i) ([1 .. n] :: [Int])

    networkArg name path appIndex = ["--network", layoutAsString (compileNetworkName name appIndex <> "=" <> pretty path)]

parseSolverOutput :: ParseSolverOutput
parseSolverOutput output = do
  let outputLines = fmap Text.pack (lines output)
  let resultIndex = findIndex (\v -> v == "sat" || v == "unsat" || v == "timeout") outputLines
  case resultIndex of
    Nothing -> do
      logDebug MinDetail $ pretty output
      throwError $ SolverOutputMalformed "Cannot find 'sat', 'unsat' or 'timeout'"
    Just i
      | outputLines !! i == "timeout" -> throwError SolverTimedOut
      | outputLines !! i == "unsat" -> return UnSAT
      | otherwise -> do
          let assignmentOutput = drop (i + 1) outputLines
          ioVarAssignment <- parseSATAssignment (filter (/= "") assignmentOutput)
          return $ SAT $ Just ioVarAssignment

parseSATAssignment ::
  (MonadError SolverError m, MonadLogger m) =>
  [Text] ->
  m QueryVariableAssignment
parseSATAssignment output = do
  assignments <- go mempty output
  return $ QueryVariableAssignment $ Map.fromList assignments
  where
    go acc [] = return (reverse acc)
    go acc (assignmentLine : rest) =
      case parseTensorDecl assignmentLine of
        Nothing ->
          throwError $
            SolverOutputMalformed $
              "Could not parse tensor declaration line" <+> quotePretty assignmentLine
        Just decl -> do
          let numberOfElements = product (tensorShape decl)
          let (valueLines, remaining) = splitAt numberOfElements rest
          if length valueLines /= numberOfElements
            then
              throwError $
                SolverOutputMalformed $
                  "Missing tensor assignment values for" <+> quotePretty (tensorName decl)
            else do
              let indices = allMultiIndices (tensorShape decl)
              let names = fmap (tensorVariableName (tensorName decl)) indices
              let values = fmap readFloatAsRational valueLines
              go (reverse (zip names values) <> acc) remaining

data TensorDecl = TensorDecl
  { tensorName :: String,
    tensorElementType :: String,
    tensorShape :: TensorShape
  }

parseTensorDecl :: Text -> Maybe TensorDecl
parseTensorDecl text
  | Text.null text = Nothing
  | otherwise = do
      let parts = Text.words text
      case parts of
        [name, typ, shapeText] -> do
          shape <- parseTensorShape shapeText
          return
            ( TensorDecl
                { tensorName = Text.unpack name,
                  tensorElementType = Text.unpack typ,
                  tensorShape = shape
                }
            )
        _ -> Nothing

parseTensorShape :: Text -> Maybe TensorShape
parseTensorShape shapeText = do
  let strippedShape = Text.strip shapeText
  innerText <- Text.stripPrefix "[" strippedShape
  dimsText <- Text.stripSuffix "]" innerText
  case Text.strip dimsText of
    "" -> Just []
    contents -> traverse (readMaybe . Text.unpack . Text.strip) (Text.splitOn "," contents)

tensorVariableName :: String -> [Int] -> QueryVariable
tensorVariableName name indices = do
  let indicesDoc = if null indices then "" else pretty indices
  layoutAsText $ pretty name <> indicesDoc
