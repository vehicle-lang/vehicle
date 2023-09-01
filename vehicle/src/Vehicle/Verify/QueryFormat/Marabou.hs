module Vehicle.Verify.QueryFormat.Marabou
  ( marabouQueryFormat,
  )
where

import Control.Monad (forM)
import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Vehicle.Backend.Queries.LinearExpr
import Vehicle.Backend.Queries.Variable
import Vehicle.Compile.Prelude
import Vehicle.Prelude.Warning
import Vehicle.Syntax.Builtin
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core
import Vehicle.Verify.QueryFormat.Interface

--------------------------------------------------------------------------------
-- Marabou query format

-- | The query format accepted by the Marabou verifier.
marabouQueryFormat :: QueryFormat
marabouQueryFormat =
  QueryFormat
    { queryFormatID = MarabouQueries,
      compileQuery = compileMarabouQuery,
      queryOutputFormat =
        ExternalOutputFormat
          { formatName = pretty MarabouQueries,
            formatVersion = Nothing,
            commentToken = "//",
            emptyLines = False
          }
    }

-- | Compiles an expression representing a single Marabou query. The expression
-- passed should only have conjunctions and existential quantifiers at the boolean
-- level.
compileMarabouQuery ::
  (MonadLogger m) =>
  Name ->
  CLSTProblem ->
  m QueryText
compileMarabouQuery propertyName (CLSTProblem variables assertions) = do
  let variableNames = sequentialNetworkVariableNaming "x" "y" variables
  let variableNamesMap = Map.fromList (zip variables variableNames)
  assertionDocs <- forM assertions (compileAssertion propertyName variableNamesMap)
  let assertionsDoc = vsep assertionDocs
  return $ layoutAsText assertionsDoc

compileAssertion ::
  (MonadLogger m) =>
  Name ->
  Map NetworkVariable Name ->
  Assertion NetworkVariable ->
  m (Doc a)
compileAssertion propertyName varNames assertion = do
  let (coeffVars, rel, constant) = convertToSparseFormat varNames assertion
  let (coeffVars', rel', constant', multipleVariables) =
        case coeffVars of
          (coeff, var) :| [] -> do
            -- Workaround for bug
            -- https://github.com/NeuralNetworkVerification/Marabou/issues/625
            let newCoeffVars = (1, var) :| []
            let newRel = if coeff < 0 then second flipOrder rel else rel
            let newConstant = constant / coeff
            (newCoeffVars, newRel, newConstant, False)
          _ -> (coeffVars, rel, constant, True)

  let compiledLHS = hsep (fmap (compileVar multipleVariables) coeffVars')
  compiledRel <- compileRel propertyName rel'
  let compiledRHS = prettyRationalAsFloat constant'
  return $ compiledLHS <+> compiledRel <+> compiledRHS

compileRel :: (MonadLogger m) => Name -> Either () OrderOp -> m (Doc a)
compileRel propertyName = \case
  Left () -> return "="
  Right Le -> return "<="
  Right Ge -> return ">="
  -- Suboptimal. Marabou doesn't currently support strict inequalities.
  -- See https://github.com/vehicle-lang/vehicle/issues/74 for details.
  Right Lt -> do
    logWarning (UnsoundStrictOrderConversion propertyName MarabouQueries)
    return "<="
  Right Gt -> do
    logWarning (UnsoundStrictOrderConversion propertyName MarabouQueries)
    return ">="

compileVar :: Bool -> (Rational, Name) -> Doc a
compileVar False (1, var) = pretty var
compileVar True (1, var) = "+" <> pretty var
compileVar _ (-1, var) = "-" <> pretty var
compileVar _ (coefficient, var) = prettyRationalAsFloat coefficient <> pretty var
