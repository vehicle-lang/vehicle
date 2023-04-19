module Vehicle.Verify.QueryFormat.Marabou
  ( marabouQueryFormat,
  )
where

import Control.Monad (forM)
import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Vehicle.Compile.Prelude
import Vehicle.Compile.Queries.LinearExpr
import Vehicle.Compile.Queries.Variable
import Vehicle.Verify.Core

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
compileMarabouQuery :: (MonadLogger m) => CLSTProblem -> m QueryText
compileMarabouQuery (CLSTProblem variables assertions) = do
  let variableNames = sequentialNetworkVariableNaming "x" "y" variables
  let variableNamesMap = Map.fromList (zip variables variableNames)
  assertionDocs <- forM assertions (compileAssertion variableNamesMap)
  let assertionsDoc = vsep assertionDocs
  return $ layoutAsText assertionsDoc

compileAssertion ::
  (MonadLogger m) =>
  Map NetworkVariable Name ->
  Assertion NetworkVariable ->
  m (Doc a)
compileAssertion varNames assertion = do
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
  let compiledRel = compileRel rel'
  let compiledRHS = prettyCoefficient constant'

  return $ compiledLHS <+> compiledRel <+> compiledRHS

compileRel :: Either () OrderOp -> Doc a
compileRel = \case
  Left () -> "="
  Right Le -> "<="
  Right Ge -> ">="
  -- Suboptimal. Marabou doesn't currently support strict inequalities.
  -- See https://github.com/vehicle-lang/vehicle/issues/74 for details.
  Right Lt -> "<="
  Right Gt -> ">="

compileVar :: Bool -> (Double, Name) -> Doc a
compileVar False (1, var) = pretty var
compileVar True (1, var) = "+" <> pretty var
compileVar _ (-1, var) = "-" <> pretty var
compileVar _ (coefficient, var) = prettyCoefficient coefficient <> pretty var
