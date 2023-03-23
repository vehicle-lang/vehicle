module Vehicle.Verify.QueryFormat.Marabou
  ( marabouQueryFormat,
  )
where

import Control.Monad (forM)
import Data.HashMap.Strict qualified as HashMap
import Data.List (sortOn)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Vehicle.Compile.Prelude
import Vehicle.Compile.Queries.LinearExpr
import Vehicle.Compile.Queries.Variable
import Vehicle.Verify.Core

--------------------------------------------------------------------------------
-- Query format

-- | The query format accepted by the Marabou verifier.
marabouQueryFormat :: QueryFormat
marabouQueryFormat =
  QueryFormat
    { queryFormatID = MarabouQueryFormat,
      compileQuery = compileMarabouQuery,
      queryOutputFormat =
        ExternalOutputFormat
          { formatName = pretty MarabouQueryFormat,
            formatVersion = Nothing,
            commentToken = "//"
          }
    }

{-
commentTokenOf :: Task -> Maybe (Doc a)
commentTokenOf = \case
  TypeCheck {} -> Nothing

  CompileToQueryFormat format -> case format of
    MarabouFormat -> Just "//"
    VNNLib -> ";"

  CompileToITP itp -> case itp of
    Agda -> Just "--"

  CompileToLossFunction {} -> Nothing
-}

-- | Compiles an expression representing a single Marabou query. The expression
-- passed should only have conjunctions and existential quantifiers at the boolean
-- level.
compileMarabouQuery :: MonadLogger m => CLSTProblem NetworkVariable -> m QueryText
compileMarabouQuery (CLSTProblem variables assertions) = do
  let variableNames = sequentialIONetworkVariableNaming "x" "y" variables
  assertionDocs <- forM assertions (compileAssertion variableNames)
  let assertionsDoc = vsep assertionDocs
  return $ layoutAsText assertionsDoc

compileAssertion ::
  MonadLogger m =>
  Seq Name ->
  Assertion SolvingLinearExpr ->
  m (Doc a)
compileAssertion variableNames (Assertion rel linearExpr) = do
  let Sparse _ coeffs constant = toSparse linearExpr
  let varCoeff = sortOn fst $ HashMap.toList coeffs
  let lookupName (v, c) = (c, variableNames `Seq.index` v)
  let coeffVars = lookupName <$> varCoeff

  -- Make the properties a tiny bit nicer by checking if all the vars are
  -- negative and if so negating everything.
  let allCoefficientsNegative = all (\(c, _) -> c < 0) coeffVars
  let (finalCoefVars, constant', flipRel) =
        if allCoefficientsNegative
          then (fmap (\(c, v) -> (-c, v)) coeffVars, -constant, True)
          else (coeffVars, constant, False)

  -- Marabou always has the constants on the RHS so we need to negate the constant.
  let negatedConstant = -constant'
  -- Also check for and remove `-0.0`s for cleanliness.
  let finalConstant = if isNegativeZero negatedConstant then 0.0 else negatedConstant

  let compiledRel = compileRel flipRel rel
  let compiledLHS = hsep (fmap (compileVar (length finalCoefVars > 1)) finalCoefVars)
  let compiledRHS = prettyCoefficient finalConstant
  return $ compiledLHS <+> compiledRel <+> compiledRHS

compileRel :: Bool -> Relation -> Doc a
compileRel _ Equal = "="
compileRel False LessThanOrEqualTo = "<="
compileRel True LessThanOrEqualTo = ">="
-- Suboptimal. Marabou doesn't currently support strict inequalities.
-- See https://github.com/vehicle-lang/vehicle/issues/74 for details.
compileRel False LessThan = "<="
compileRel True LessThan = ">="

compileVar :: Bool -> (Double, Name) -> Doc a
compileVar False (1, var) = pretty var
compileVar True (1, var) = "+" <> pretty var
compileVar _ (-1, var) = "-" <> pretty var
compileVar _ (coefficient, var) = prettyCoefficient coefficient <> pretty var
