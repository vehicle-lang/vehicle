module Vehicle.Compile.Queries
  ( compileToQueries,
  )
where

import Control.Monad (forM, forM_)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.Foldable (for_)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes)
import Vehicle.Backend.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources (expandResources)
import Vehicle.Compile.Normalise (fullNormalisationOptions, normaliseProg)
import Vehicle.Compile.Prelude
import Vehicle.Compile.QuantifierAnalysis (checkQuantifiersAndNegateIfNecessary)
import Vehicle.Compile.Queries.DNF (convertToDNF)
import Vehicle.Compile.Queries.IfElimination (eliminateIfs)
import Vehicle.Compile.Queries.NetworkElimination
import Vehicle.Compile.Queries.QuantifierLifting (liftQuantifiers)
import Vehicle.Compile.Resource
import Vehicle.Compile.Type (getPropertyInfo, getUnnormalised)
import Vehicle.Verify.Core
import Vehicle.Verify.Specification
import Vehicle.Verify.Verifier.Interface (Verifier (..))

--------------------------------------------------------------------------------
-- Compilation to individual queries

-- | Compiles the provided program to invidividual queries suitable for a
-- verifier.
compileToQueries ::
  (MonadIO m, MonadCompile m) =>
  Verifier ->
  TypedProg ->
  Resources ->
  m (VerificationPlan, VerificationQueries)
compileToQueries verifier@Verifier {..} typedProg resources =
  logCompilerPass MinDetail currentPass $ do
    (networkCtx, finalProg) <- expandResources resources typedProg
    checkProperties verifierIdentifier finalProg

    -- Again, horrible, we should push use of NormExpr through everywhere
    unnormalisedProg <- traverse getUnnormalised finalProg
    normalisedProg <- normaliseProg unnormalisedProg fullNormalisationOptions
    properties <- getProperties normalisedProg

    if null properties
      then throwError NoPropertiesFound
      else do
        xs <- forM properties $ \(name, expr) -> do
          logCompilerPass MinDetail ("property" <+> quotePretty name) $ do
            let propertyCtx = (verifier, name, networkCtx)
            property <- runSupplyT (runReaderT (compileProperty expr) propertyCtx) [1 :: Int ..]
            return (name, property)

        return $ NonEmpty.unzip $ Specification xs

--------------------------------------------------------------------------------
-- Algorithm

getProperties ::
  MonadCompile m =>
  CheckedProg ->
  m [(Identifier, CheckedExpr)]
getProperties (Main ds) = catMaybes <$> traverse go ds
  where
    go ::
      MonadCompile m =>
      CheckedDecl ->
      m (Maybe (Identifier, CheckedExpr))
    go d = case d of
      DefResource _ r _ _ ->
        normalisationError currentPass (pretty r <+> "declarations")
      DefPostulate {} ->
        normalisationError currentPass "postulates"
      DefFunction _ ident isProperty _ body
        -- If it's not a property then we can discard it as all applications
        -- of it should have been normalised out by now.
        | isProperty -> return $ Just (ident, body)
        | otherwise -> return Nothing

checkProperties ::
  MonadCompile m =>
  VerifierIdentifier ->
  TypedProg ->
  m ()
checkProperties verifier (Main ds) = for_ ds go
  where
    go ::
      MonadCompile m =>
      TypedDecl ->
      m ()
    go d = case d of
      DefResource _ r _ _ ->
        normalisationError currentPass (pretty r <+> "declarations")
      DefPostulate {} ->
        normalisationError currentPass "postulates"
      DefFunction p ident isProperty _ _
        -- If it's not a property then we can discard it as all applications
        -- of it should have been normalised out by now.
        | not isProperty -> return ()
        | otherwise -> do
            -- Otherwise check the property information.
            propertyInfo <- getPropertyInfo d
            let compatible = checkCompatibility (VerifierBackend verifier) (ident, p) propertyInfo
            forM_ compatible throwError

type MonadCompileProperty m =
  ( MonadCompile m,
    MonadSupply QueryID m,
    MonadReader (Verifier, Identifier, NetworkContext) m
  )

compileProperty :: MonadCompileProperty m => CheckedExpr -> m (Property (QueryMetaData, QueryText))
compileProperty = \case
  VecLiteral _ _ es -> MultiProperty <$> traverse compileProperty es
  expr -> SingleProperty <$> compileTopLevelPropertyStructure expr

compileTopLevelPropertyStructure ::
  MonadCompileProperty m =>
  CheckedExpr ->
  m (PropertyExpr (QueryMetaData, QueryText))
compileTopLevelPropertyStructure = \case
  AppliedAndExpr _ x y ->
    Conjunct <$> compileTopLevelPropertyStructure x <*> compileTopLevelPropertyStructure y
  AppliedOrExpr _ x y ->
    Disjunct <$> compileTopLevelPropertyStructure x <*> compileTopLevelPropertyStructure y
  queryExpr ->
    compileQuantifiedExpr queryExpr

compileQuantifiedExpr ::
  MonadCompileProperty m =>
  CheckedExpr ->
  m (PropertyExpr (QueryMetaData, QueryText))
compileQuantifiedExpr expr = do
  (verifier, ident, _) <- ask

  -- Check that we only have one type of quantifier in the expression
  -- and if it is universal then negate the expression
  (isPropertyNegated, possiblyNegatedExpr) <-
    checkQuantifiersAndNegateIfNecessary (verifierIdentifier verifier) ident expr

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

compileSingleQuery :: MonadCompileProperty m => CheckedExpr -> m (Query (QueryMetaData, QueryText))
compileSingleQuery expr = do
  queryID <- demand
  (verifier, ident, networkCtx) <- ask

  logCompilerPass MinDetail ("query" <+> pretty (queryID :: QueryID)) $ do
    -- First lift all the quantifiers to the top-level
    quantLiftedExpr <- liftQuantifiers expr

    -- Convert all user variables and applications of networks into magic I/O variables
    clstQuery <- normUserVariables ident verifier networkCtx quantLiftedExpr

    flip traverseQuery clstQuery $ \(clstProblem, u, v) -> do
      queryText <- compileQuery verifier clstProblem
      logCompilerPassOutput queryText
      return (QueryData u v, queryText)

splitDisjunctions :: Expr binder var -> [Expr binder var]
splitDisjunctions (OrExpr _ann [e1, e2]) =
  splitDisjunctions (argExpr e1) <> splitDisjunctions (argExpr e2)
splitDisjunctions e = [e]

currentPass :: Doc a
currentPass = "compilation to individual queries"

checkCompatibility :: Backend -> DeclProvenance -> PropertyInfo -> Maybe CompileError
checkCompatibility backend prov (PropertyInfo linearity polarity) =
  case (linearity, polarity) of
    (NonLinear p pp1 pp2, _) ->
      Just $ UnsupportedNonLinearConstraint backend prov p pp1 pp2
    (_, MixedSequential q p pp2) ->
      Just $ UnsupportedAlternatingQuantifiers backend prov q p pp2
    _ -> Nothing
