module Vehicle.Compile.Queries
  ( compileToQueries
  , QueryData(..)
  ) where

import Control.Monad (forM)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.Map qualified as Map (lookup)
import Data.Maybe (catMaybes)

import Vehicle.Backend.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise
import Vehicle.Compile.Prelude
import Vehicle.Compile.QuantifierAnalysis (checkQuantifiersAndNegateIfNecessary)
import Vehicle.Compile.Queries.DNF (convertToDNF)
import Vehicle.Compile.Queries.IfElimination (eliminateIfs)
import Vehicle.Compile.Queries.QuantifierLifting (liftQuantifiers)
import Vehicle.Compile.Queries.NetworkElimination
import Vehicle.Compile.Resource
import Vehicle.Verify.Core
import Vehicle.Verify.Specification
import Vehicle.Verify.Verifier.Interface
import Vehicle.Compile.Queries.VariableReconstruction

data QueryData = QueryData
  { queryText   :: Doc ()
  , metaNetwork :: MetaNetwork
  , userVar     :: UserVarReconstructionInfo
  }

--------------------------------------------------------------------------------
-- Compilation to individual queries

-- | Compiles the provided program to invidividual queries suitable for a
-- verifier.
compileToQueries :: MonadCompile m
                 => Verifier
                 -> CheckedProg
                 -> PropertyContext
                 -> NetworkContext
                 -> m (Specification QueryData)
compileToQueries verifier@Verifier{..} prog propertyCtx networkCtx =
  logCompilerPass MinDetail currentPass $ do
    if null propertyCtx
      then throwError NoPropertiesFound
      else do
        normProg <- normalise prog fullNormalisationOptions
        properties <- getProperties verifierIdentifier propertyCtx normProg

        xs <- forM properties $ \(name, expr) -> do
          logCompilerPass MinDetail ("property" <+> quotePretty name) $ do
            property <- runSupplyT (runReaderT (compileProperty expr) (verifier, name, networkCtx)) [1::Int ..]
            return (name, property)

        return $ Specification xs

--------------------------------------------------------------------------------
-- Algorithm

getProperties :: MonadCompile m
              => VerifierIdentifier
              -> PropertyContext
              -> CheckedProg
              -> m [(Name, CheckedExpr)]
getProperties verifier propertyCtx (Main ds) = catMaybes <$> traverse go ds
  where
    go :: MonadCompile m
       => CheckedDecl
       -> m (Maybe (Name, CheckedExpr))
    go d = case d of
      DefResource _ r _ _ ->
        normalisationError currentPass (pretty r <+> "declarations")

      DefPostulate{} ->
        normalisationError currentPass "postulates"

      DefFunction p ident _ expr -> do
        let maybePropertyInfo = Map.lookup ident propertyCtx
        case maybePropertyInfo of
          -- If it's not a property then we can discard it as all applications
          -- of it should have been normalised out by now.
          Nothing -> return Nothing
          -- Otherwise check the property information.
          Just propertyInfo -> case checkCompatibility (VerifierBackend verifier) (ident, p) propertyInfo of
            Just err -> throwError err
            Nothing  -> return $ Just (nameOf ident, expr)

type MonadCompileProperty m =
  ( MonadCompile m
  , MonadSupply QueryID m
  , MonadReader (Verifier, Name, NetworkContext) m
  )

compileProperty :: MonadCompileProperty m => CheckedExpr -> m (Property QueryData)
compileProperty = \case
  VecLiteral _ _ es -> MultiProperty  <$> traverse compileProperty es
  expr              -> SingleProperty <$> compileTopLevelPropertyStructure expr

compileTopLevelPropertyStructure :: MonadCompileProperty m
                                 => CheckedExpr
                                 -> m (PropertyExpr QueryData)
compileTopLevelPropertyStructure = \case
  AppliedAndExpr _ x y ->
    Conjunct <$> compileTopLevelPropertyStructure x <*> compileTopLevelPropertyStructure y

  AppliedOrExpr _ x y ->
    Disjunct <$> compileTopLevelPropertyStructure x <*> compileTopLevelPropertyStructure y

  queryExpr ->
    compileQuantifiedExpr queryExpr

compileQuantifiedExpr :: MonadCompileProperty m
                      => CheckedExpr
                      -> m (PropertyExpr QueryData)
compileQuantifiedExpr expr = do
  (verifier, ident, _) <- ask

  -- Check that we only have one type of quantifier in the expression
  -- and if it is universal then negate the expression
  (isPropertyNegated, possiblyNegatedExpr) <-
    checkQuantifiersAndNegateIfNecessary (verifierIdentifier verifier) (Identifier ident) expr

  -- Eliminate any if-expressions
  ifFreeExpr <- eliminateIfs possiblyNegatedExpr

  -- Convert to disjunctive normal form
  dnfExpr <- convertToDNF ifFreeExpr

  -- Split up into the individual queries needed for Marabou.
  let queryExprs = splitDisjunctions dnfExpr
  let numberOfQueries = length queryExprs
  logDebug MinDetail $ "Found" <+> pretty numberOfQueries <+> "queries" <> line

  -- Compile the individual queries
  queries <- traverse compileSingleQuery queryExprs

  let combinedQueries = disjunctQueries isPropertyNegated queries

  return combinedQueries

compileSingleQuery :: MonadCompileProperty m => CheckedExpr -> m (Query QueryData)
compileSingleQuery expr = do
  queryID <- demand
  (verifier, ident, networkCtx) <- ask

  logCompilerPass MinDetail ("query" <+> pretty (queryID :: QueryID)) $ do

    -- First lift all the quantifiers to the top-level
    quantLiftedExpr <- liftQuantifiers expr

    -- Convert all user variables and applications of networks into magic I/O variables
    clstQuery <- normUserVariables (Identifier ident) verifier networkCtx quantLiftedExpr

    flip traverseQuery clstQuery $ \(clstProblem, u, v) -> do
      queryDoc <- compileQuery verifier clstProblem
      logCompilerPassOutput queryDoc
      return (QueryData queryDoc u v)

splitDisjunctions :: Expr binder var -> [Expr binder var]
splitDisjunctions (OrExpr _ann [e1, e2]) =
  splitDisjunctions (argExpr e1) <> splitDisjunctions (argExpr e2)
splitDisjunctions e = [e]

currentPass :: Doc a
currentPass = "compilation to individual queries"

checkCompatibility :: Backend -> DeclProvenance -> PropertyInfo -> Maybe CompileError
checkCompatibility backend prov (PropertyInfo linearity polarity) =
  case (linearity, polarity) of
    (NonLinear p pp1 pp2, _)       ->
      Just $ UnsupportedNonLinearConstraint backend prov p pp1 pp2
    (_, MixedSequential q p pp2) ->
      Just $ UnsupportedAlternatingQuantifiers backend prov q p pp2
    _ -> Nothing
