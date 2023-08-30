module Vehicle.Verify.QueryFormat.VNNLib where

import Control.Monad (forM)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Vehicle.Backend.Queries.LinearExpr
import Vehicle.Backend.Queries.Variable
import Vehicle.Compile.Prelude
import Vehicle.Syntax.Builtin
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core
import Vehicle.Verify.QueryFormat.Interface

--------------------------------------------------------------------------------
-- Marabou query format

-- | The query format accepted by the Marabou verifier.
vnnlibQueryFormat :: QueryFormat
vnnlibQueryFormat =
  QueryFormat
    { queryFormatID = VNNLibQueries,
      compileQuery = compileVNNLibQuery,
      queryOutputFormat =
        ExternalOutputFormat
          { formatName = pretty VNNLibQueries,
            formatVersion = Nothing,
            commentToken = ";",
            emptyLines = True
          }
    }

-- | Compiles an expression representing a single Marabou query. The expression
-- passed should only have conjunctions and existential quantifiers at the boolean
-- level.
compileVNNLibQuery :: (MonadLogger m) => Name -> CLSTProblem -> m QueryText
compileVNNLibQuery _propertyName (CLSTProblem variables assertions) = do
  let variableNames = sequentialNetworkVariableNaming "X_" "Y_" variables
  let variableNamesMap = Map.fromList (zip variables variableNames)
  variableDocs <- forM variableNames compileVariable
  assertionDocs <- forM assertions (compileAssertion variableNamesMap)
  let assertionsDoc = vsep assertionDocs <> line <> vsep variableDocs
  return $ layoutAsText assertionsDoc

compileVariable :: (MonadLogger m) => Name -> m (Doc a)
compileVariable varName = return $ parens ("declare-fun" <+> pretty varName <+> "() Real")

compileAssertion ::
  (MonadLogger m) =>
  Map NetworkVariable Name ->
  Assertion NetworkVariable ->
  m (Doc a)
compileAssertion varNames assertion = do
  let (coeffVars, rel, constant) = convertToSparseFormat varNames assertion

  let compiledRel = compileRel rel
  let compiledLHS = foldl compileVar "" (NonEmpty.tail coeffVars)
  let compiledRHS = prettyRationalAsFloat constant
  return $ parens "assert" <+> parens (compiledRel <+> parens compiledLHS <+> compiledRHS)

compileRel :: Either () OrderOp -> Doc a
compileRel = \case
  Left () -> "=="
  Right Le -> "<="
  Right Ge -> ">="
  Right Lt -> "<"
  Right Gt -> ">"

compileVar :: Doc a -> (Coefficient, Name) -> Doc a
compileVar r (coef, var)
  | coef == 1 = "+" <+> parens r <+> pretty var
  | coef == -1 = "-" <+> parens r <+> pretty var
  | coef < 0 = "-" <+> parens r <+> parens ("*" <+> prettyRationalAsFloat (-coef) <+> pretty var)
  | otherwise = "+" <+> parens r <+> parens ("*" <+> prettyRationalAsFloat coef <+> pretty var)
