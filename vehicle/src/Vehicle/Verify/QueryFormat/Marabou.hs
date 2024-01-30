module Vehicle.Verify.QueryFormat.Marabou
  ( marabouQueryFormat,
    compileVar,
  )
where

import Control.Monad (forM)
import Control.Monad.Except (MonadError (..))
import Data.List.NonEmpty (NonEmpty (..))
import Vehicle.Compile.Error (CompileError (..))
import Vehicle.Compile.Prelude
import Vehicle.Data.LinearExpr
import Vehicle.Prelude.Warning
import Vehicle.Syntax.Builtin
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core
import Vehicle.Verify.QueryFormat.Interface
import Vehicle.Verify.Variable

--------------------------------------------------------------------------------
-- Marabou query format

-- | The query format accepted by the Marabou verifier.
marabouQueryFormat :: QueryFormat
marabouQueryFormat =
  QueryFormat
    { queryFormatID = MarabouQueries,
      formatQuery = compileMarabouQuery,
      supportsStrictInequalities = False,
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
  (MonadLogger m, MonadError CompileError m) =>
  QueryAddress ->
  QueryContents ->
  m QueryText
compileMarabouQuery address (QueryContents _variables assertions) = do
  assertionDocs <- forM assertions (compileAssertion address)
  let assertionsDoc = vsep assertionDocs
  return $ layoutAsText assertionsDoc

compileAssertion ::
  (MonadLogger m, MonadError CompileError m) =>
  QueryAddress ->
  QueryAssertion ->
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

compileCoefVar :: Bool -> (Coefficient, NetworkRationalVariable) -> Doc a
compileCoefVar False (1, var) = compileVar var
compileCoefVar True (1, var) = "+" <> compileVar var
compileCoefVar _ (-1, var) = "-" <> compileVar var
compileCoefVar _ (coefficient, var) = prettyRationalAsFloat coefficient <> compileVar var

compileVar :: NetworkRationalVariable -> Doc a
compileVar var = do
  let name = if inputOrOutput (originalVar var) == Input then "x" else "y"
  let index = computeAbsoluteIndex var
  name <> pretty index
