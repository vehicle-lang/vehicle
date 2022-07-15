module Vehicle.Backend.Marabou.Compile
  ( compile
  ) where

import Control.Monad.Except (MonadError(..))
import Control.Monad (forM)
import Data.Maybe (catMaybes)
import Data.Vector.Unboxed qualified as Vector

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise (normalise, NormalisationOptions(..), defaultNormalisationOptions)
import Vehicle.Compile.Normalise.UserVariables
import Vehicle.Compile.Normalise.IfElimination (eliminateIfs)
import Vehicle.Compile.Normalise.DNF (convertToDNF, splitDisjunctions)
import Vehicle.Compile.QuantifierAnalysis (checkQuantifiersAndNegateIfNecessary)
import Vehicle.Backend.Prelude
import Vehicle.Backend.Marabou.Core
import Vehicle.Compile.Resource
import Vehicle.Compile.Linearity

--------------------------------------------------------------------------------
-- Compatibility

checkCompatibility :: Identifier -> Provenance -> PropertyInfo -> Maybe CompileError
checkCompatibility ident declProv (PropertyInfo linearity polarity) =
  case linearity of
    NonLinear pp1 pp2 -> Just $
      UnsupportedNonLinearConstraint MarabouBackend ident declProv pp1 pp2
    _ -> case polarity of
      MixedSequential q p pp2 -> Just $
        UnsupportedSequentialQuantifiers MarabouBackend ident declProv q p pp2
      _                         -> Nothing


--------------------------------------------------------------------------------
-- Compilation to Marabou

-- | Compiles the provided program to Marabou queries.
compile :: MonadCompile m => NetworkContext -> CheckedProg -> m [(Symbol, MarabouProperty)]
compile networkCtx prog = logCompilerPass MinDetail "compilation to Marabou" $ do
  results <- compileProg networkCtx prog
  if null results then
    throwError NoPropertiesFound
  else
    return results

--------------------------------------------------------------------------------
-- Algorithm

compileProg :: MonadCompile m => NetworkContext -> CheckedProg -> m [(Symbol, MarabouProperty)]
compileProg networkCtx (Main ds) = catMaybes <$> traverse (compileDecl networkCtx) ds

compileDecl :: MonadCompile m => NetworkContext -> CheckedDecl -> m (Maybe (Symbol, MarabouProperty))
compileDecl networkCtx d = case d of
  DefResource _ r _ _ -> normalisationError currentPass (pretty r <+> "declarations")

  DefFunction p maybePropertyInfo ident _ expr ->
    case maybePropertyInfo of
      -- If it's not a property then we can discard it as all applications
      -- of it should have been normalised out by now.
      Nothing -> return Nothing
      -- Otherwise check the property information.
      Just propertyInfo -> case checkCompatibility ident p propertyInfo of
        Just err -> throwError err
        Nothing -> do
          property <- compileProperty ident networkCtx expr
          return (Just (nameOf ident, property))

compileProperty :: MonadCompile m
                => Identifier
                -> NetworkContext
                -> CheckedExpr
                -> m MarabouProperty
compileProperty ident networkCtx (SeqExpr _ _ _ es) =
  MultiProperty <$> traverse (compileProperty ident networkCtx) es
compileProperty ident networkCtx expr =
  logCompilerPass MinDetail ("property" <+> squotes (pretty ident)) $ do

    -- Check that we only have one type of quantifier in the property
    -- and if it is universal then negate the property
    (isPropertyNegated, possiblyNegatedExpr) <-
      checkQuantifiersAndNegateIfNecessary MarabouBackend ident expr

    -- Normalise the expression to push through the negation.
    normExpr <- normalise possiblyNegatedExpr $ defaultNormalisationOptions
      { implicationsToDisjunctions = True
      , subtractionToAddition      = True
      , expandOutPolynomials       = True
      }

    -- Eliminate any if-expressions
    ifFreeExpr <- eliminateIfs normExpr

    -- Convert to disjunctive normal form
    dnfExpr <- convertToDNF ifFreeExpr

    -- Split up into the individual queries needed for Marabou.
    let queryExprs = splitDisjunctions dnfExpr
    let numberOfQueries = length queryExprs
    logDebug MinDetail $ "Found" <+> pretty numberOfQueries <+> "queries" <> line

    -- Compile the individual queries
    let compileQ = compileQuery ident networkCtx
    queries <- traverse compileQ (zip [1..] queryExprs)

    return $ SingleProperty isPropertyNegated queries

compileQuery :: MonadCompile m
             => Identifier
             -> NetworkContext
             -> (Int, CheckedExpr)
             -> m MarabouQuery
compileQuery ident networkCtx (queryId, expr) =
  logCompilerPass MinDetail ("query" <+> pretty queryId) $ do

    -- Convert all user varaibles and applications of networks into magic I/O variables
    (CLSTProblem varNames assertions, metaNetwork, userVarReconstruction) <-
      normUserVariables ident Marabou networkCtx expr

    (vars, doc) <- logCompilerPass MinDetail "compiling assertions" $ do
      assertionDocs <- forM assertions (compileAssertion varNames)
      let assertionsDoc = vsep assertionDocs
      logCompilerPassOutput assertionsDoc
      return (varNames, assertionsDoc)

    return $ MarabouQuery doc vars metaNetwork userVarReconstruction


compileAssertion :: MonadCompile m
                 => VariableNames
                 -> Assertion
                 -> m (Doc a)
compileAssertion varNames (Assertion rel linearExpr) = do
  let (coefficientsVec, constant) = splitOutConstant linearExpr
  let coefficients = Vector.toList coefficientsVec
  let allCoeffVars = zip coefficients varNames
  let coeffVars = filter (\(c,_) -> c /= 0) allCoeffVars

  -- Make the properties a tiny bit nicer by checking if all the vars are
  -- negative and if so negating everything.
  let allCoefficientsNegative = all (\(c,_) -> c < 0) coeffVars
  let (finalCoefVars, constant', flipRel) = if allCoefficientsNegative
        then (fmap (\(c,n) -> (-c,n)) coeffVars, -constant, True)
        else (coeffVars, constant, False)

  -- Marabou always has the constants on the RHS so we need to negate the constant.
  let negatedConstant = -constant'
  -- Also check for and remove `-0.0`s for cleanliness.
  let finalConstant = if isNegativeZero negatedConstant then 0.0 else negatedConstant

  let compiledRel = compileRel flipRel rel
  let compiledLHS = hsep (fmap (compileVar (length finalCoefVars > 1)) finalCoefVars)
  let compiledRHS = pretty finalConstant
  return $ compiledLHS <+> compiledRel <+> compiledRHS
  where
    compileRel :: Bool -> Relation -> Doc a
    compileRel _     Equals            = "="
    compileRel False LessThanOrEqualTo = "<="
    compileRel True  LessThanOrEqualTo = ">="
    -- Suboptimal. Marabou doesn't currently support strict inequalities.
    -- See https://github.com/vehicle-lang/vehicle/issues/74 for details.
    compileRel False LessThan          = "<="
    compileRel True  LessThan          = ">="

    compileVar :: Bool -> (Double, Symbol) -> Doc a
    compileVar False (1,           var) = pretty var
    compileVar True  (1,           var) = "+" <> pretty var
    compileVar _     (-1,          var) = "-" <> pretty var
    compileVar _     (coefficient, var) = pretty coefficient <> pretty var

currentPass :: Doc a
currentPass = "compilation to Marabou"