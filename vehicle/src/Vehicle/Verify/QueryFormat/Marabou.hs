module Vehicle.Verify.QueryFormat.Marabou
  ( marabouQueryFormat,
    compileMarabouVar,
  )
where

import Control.Monad (forM)
import Control.Monad.Except (MonadError (..))
import Data.List.NonEmpty (NonEmpty (..))
import Vehicle.Compile.Error (CompileError (..))
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Core
import Vehicle.Data.QuantifiedVariable (prettyRationalAsFloat)
import Vehicle.Prelude.Warning
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
      supportsStrictInequalities = False,
      queryOutputFormat = outputFormat,
      compileQuery = compileMarabouQuery,
      compileVariable = compileMarabouVar
    }

outputFormat :: ExternalOutputFormat
outputFormat =
  ExternalOutputFormat
    { formatName = pretty MarabouQueries,
      formatVersion = Nothing,
      commentToken = "//",
      emptyLines = False
    }

-- | Compiles an individual variable
compileMarabouVar :: CompileQueryVariable
compileMarabouVar inputOrOutput ioIndex = do
  let name = if inputOrOutput == Input then "x" else "y"
  layoutAsText $ name <> pretty ioIndex

-- | Compiles an expression representing a single Marabou query.
compileMarabouQuery :: CompileQuery
compileMarabouQuery address (QueryContents _variables assertions) = do
  assertionDocs <- forM assertions (compileAssertion address)
  let assertionsDoc = vsep assertionDocs
  return $ layoutAsText assertionsDoc

compileAssertion ::
  (MonadLogger m, MonadError CompileError m) =>
  QueryAddress ->
  QueryAssertion QueryVariable ->
  m (Doc a)
compileAssertion address QueryAssertion {..} = do
  let (coeffVars', rel', constant', multipleVariables) = case lhs of
        (coeff, var) :| [] -> do
          -- Workaround for bug https://github.com/NeuralNetworkVerification/Marabou/issues/625
          let newCoeffVars = (1, var) :| []
          let newRel = if coeff < 0 then flipQueryRel rel else rel
          let newConstant = rhs / coeff
          (newCoeffVars, newRel, newConstant, False)
        _ -> (lhs, rel, rhs, True)

  compiledRel <- compileRel address rel'
  let compiledLHS = hsep (fmap (compileCoefVar multipleVariables) coeffVars')
  let compiledRHS = prettyRationalAsFloat constant'
  return $ compiledLHS <+> compiledRel <+> compiledRHS

compileRel :: (MonadLogger m, MonadError CompileError m) => QueryAddress -> QueryRelation -> m (Doc a)
compileRel address = \case
  EqualRel -> return "="
  OrderRel Le -> return "<="
  OrderRel Ge -> return ">="
  -- Suboptimal. Marabou doesn't currently support strict inequalities.
  -- See https://github.com/vehicle-lang/vehicle/issues/74 for details.
  OrderRel Lt -> do
    logWarning (UnsoundStrictOrderConversion MarabouQueries address)
    return "<="
  OrderRel Gt -> do
    logWarning (UnsoundStrictOrderConversion MarabouQueries address)
    return ">="

compileCoefVar :: Bool -> (Coefficient, QueryVariable) -> Doc a
compileCoefVar False (1, var) = pretty var
compileCoefVar True (1, var) = "+" <> pretty var
compileCoefVar _ (-1, var) = "-" <> pretty var
compileCoefVar _ (coefficient, var) = prettyRationalAsFloat coefficient <> pretty var
