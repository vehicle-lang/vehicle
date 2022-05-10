module Vehicle.Backend.Marabou.Compile
  ( compile
  ) where

import Control.Monad.Except (MonadError(..))
import Data.Maybe (catMaybes)
import Data.Bifunctor (Bifunctor(first))

import Vehicle.Language.Print (prettySimple)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise (normalise, NormalisationOptions(..))
import Vehicle.Compile.Normalise.NetworkApplications
import Vehicle.Compile.Normalise.IfElimination (liftAndEliminateIfs)
import Vehicle.Compile.Normalise.DNF (convertToDNF, splitDisjunctions)
import Vehicle.Compile.Descope (runDescope)
import Vehicle.Compile.SupplyNames (supplyDBNames)
import Vehicle.Compile.QuantifierAnalysis (checkQuantifiersAndNegateIfNecessary)
import Vehicle.Backend.Prelude
import Vehicle.Backend.Marabou.Core
import Vehicle.Resource.NeuralNetwork

--------------------------------------------------------------------------------
-- Compilation to Marabou

-- | Compiles the provided program to Marabou queries.
compile :: MonadCompile m => NetworkCtx -> CheckedProg -> m [MarabouProperty]
compile networkCtx prog = logCompilerPass "compilation to Marabou" $
  compileProg networkCtx prog

--------------------------------------------------------------------------------
-- Algorithm

compileProg :: MonadCompile m => NetworkCtx -> CheckedProg -> m [MarabouProperty]
compileProg networkCtx (Main ds) = do
  results <- catMaybes <$> traverse (compileDecl networkCtx) ds
  if null results then
    throwError NoPropertiesFound
  else
    return results

compileDecl :: MonadCompile m => NetworkCtx -> CheckedDecl -> m (Maybe MarabouProperty)
compileDecl networkCtx d = case d of
  DefResource _ r _ _ -> normalisationError currentPass (pretty r <+> "declarations")

  DefFunction _p _ ident t expr ->
    if not $ isProperty t
      -- If it's not a property then we can discard it as all applications
      -- of it should have been normalised out by now.
      then return Nothing
      else Just <$> compileProperty ident networkCtx expr

compileProperty :: MonadCompile m
                => Identifier
                -> NetworkCtx
                -> CheckedExpr
                -> m MarabouProperty
compileProperty ident networkCtx expr =
  logCompilerPass ("property" <+> squotes (pretty ident)) $ do

    -- Check that we only have one type of quantifier in the property
    -- and if it is universal then negate the property
    (isPropertyNegated, originalQuantifier, possiblyNegatedExpr) <-
      checkQuantifiersAndNegateIfNecessary MarabouBackend ident expr

    -- Normalise the expression to push through the negation.
    normExpr <- normalise (Options
      { implicationsToDisjunctions = True
      , subtractionToAddition      = True
      , expandOutPolynomials       = True
      }) possiblyNegatedExpr

    -- Eliminate any if-expressions
    ifFreeExpr <- liftAndEliminateIfs normExpr

    -- Normalise again to push through the introduced nots. Can definitely be
    -- more efficient here and just push in the not, when we introduce
    -- it during if elimination.
    normExpr2 <- normalise (Options
      { implicationsToDisjunctions = True
      , subtractionToAddition      = True
      , expandOutPolynomials       = True
      }) ifFreeExpr

    -- Convert to disjunctive normal form
    dnfExpr <- convertToDNF normExpr2

    -- Split up into the individual queries needed for Marabou.
    let queryExprs = splitDisjunctions dnfExpr
    let numberOfQueries = length queryExprs
    logDebug MinDetail $ "Found" <+> pretty numberOfQueries <+> "queries" <> line

    -- Compile the individual queries
    let compileQ = compileQuery ident networkCtx originalQuantifier
    queries <- traverse compileQ (zip [1..] queryExprs)

    return $ MarabouProperty (nameOf ident) isPropertyNegated queries

compileQuery :: MonadCompile m
             => Identifier
             -> NetworkCtx
             -> Quantifier
             -> (Int, CheckedExpr)
             -> m MarabouQuery
compileQuery ident networkCtx originalQuantifier (queryId, expr) =
  logCompilerPass ("query" <+> pretty queryId) $ do

    -- Convert all applications of networks into magic variables
    (networklessExpr, metaNetwork) <-
      convertNetworkAppsToMagicVars Marabou networkCtx Any expr

    -- Normalise the expression to remove any implications, push the negations through
    -- and expand out any multiplication.
    normExpr <- normalise (Options
      { implicationsToDisjunctions = True
      , subtractionToAddition      = True
      , expandOutPolynomials       = True
      }) networklessExpr

    -- Descope the expression, converting from DeBruijn indices to names
    let descopedExpr = runDescope [] (supplyDBNames normExpr)

    (vars, doc) <- logCompilerPass "compiling assertions" $ do
      (vars, assertionDocs) <- compileAssertions ident originalQuantifier descopedExpr
      let assertionsDoc = vsep assertionDocs
      logCompilerPassOutput assertionsDoc
      return (vars, assertionsDoc)

    return $ MarabouQuery doc vars metaNetwork

compileAssertions :: MonadCompile m
                  => Identifier
                  -> Quantifier
                  -> OutputExpr
                  -> m ([MarabouVar], [Doc a])
compileAssertions ident quantifier expr = case expr of
  Type{}     -> typeError          currentPass "Type"
  Pi{}       -> typeError          currentPass "Pi"
  Hole{}     -> resolutionError    currentPass "Hole"
  Meta{}     -> resolutionError    currentPass "Meta"
  Ann{}      -> normalisationError currentPass "Ann"
  Lam{}      -> normalisationError currentPass "Lam"
  Let{}      -> normalisationError currentPass "Let"
  LSeq{}     -> normalisationError currentPass "LSeq"
  PrimDict{} -> visibilityError    currentPass "PrimDict"
  Builtin{}  -> normalisationError currentPass "LSeq"

  Var _ann v -> return ([], [pretty v])

  Literal _ann l -> case l of
    LBool _ -> normalisationError currentPass "LBool"
    _       -> caseError currentPass "Literal" ["AndExpr"]

  QuantifierExpr _ _ binder body -> do
    var <- compileBinder ident binder
    (vars, docs) <- compileAssertions ident quantifier body
    return (var : vars, docs)

  AndExpr _ [e1, e2] -> do
    (vars1, docs1) <- compileAssertions ident quantifier (argExpr e1)
    (vars2, docs2) <- compileAssertions ident quantifier (argExpr e2)
    return (vars1 <> vars2, docs1 <> docs2)

  OrderExpr order ann _ [lhs, rhs] -> do
    assertion <- compileAssertion ann ident quantifier (OrderRel order) (argExpr lhs) (argExpr rhs)
    return ([], [assertion])

  EqualityExpr eq ann _ [lhs, rhs] -> do
    assertion <- compileAssertion ann ident quantifier (EqualityRel eq) (argExpr lhs) (argExpr rhs)
    return ([], [assertion])

  App{} -> unexpectedExprError currentPass (prettySimple expr)

compileBinder :: MonadCompile m => Identifier -> OutputBinder -> m MarabouVar
compileBinder ident binder =
  let p = provenanceOf binder in
  let n = nameOf binder in
  let t = typeOf binder in
  case typeOf binder of
    RatType  _ -> return $ MarabouVar n MReal
    RealType _ -> return $ MarabouVar n MReal
    _ -> throwError $ UnsupportedVariableType MarabouBackend p ident n t supportedTypes

compileAssertion :: MonadCompile m
                 => CheckedAnn
                 -> Identifier
                 -> Quantifier
                 -> Relation
                 -> OutputExpr
                 -> OutputExpr
                 -> m (Doc a)
compileAssertion ann ident quantifier rel lhs rhs = do
  (lhsVars, lhsConstants) <- compileSide lhs
  (rhsVars, rhsConstants) <- compileSide rhs
  let vars = lhsVars <> flipVars rhsVars
  let constant = sum (flipConstants lhsConstants <> rhsConstants)

  -- Make the properties a tiny bit nicer by checking if all the vars are
  -- negative and if so negating everything.
  let (finalVars, finalConstant, finalRel) = if all (\x -> fst x < 0) vars
        then (flipVars vars, -1 * constant, flipRel rel)
        else (vars, constant, rel)

  compiledRel <- compileRel finalRel
  let compiledLHS = hsep (fmap (compileVar (length finalVars > 1)) finalVars)
  let compiledRHS = pretty finalConstant
  return $ compiledLHS <+> compiledRel <+> compiledRHS
  where
    flipConstants :: [Double] -> [Double]
    flipConstants = fmap ((-1) *)

    flipVars :: [(Double, Symbol)] -> [(Double, Symbol)]
    flipVars = fmap (first ((-1) *))

    compileSide :: MonadCompile m => OutputExpr -> m ([(Double, Symbol)], [Double])
    compileSide = \case
      Var     _ v                           -> return ([(1, v)], [])
      NegExpr _ _ [ExplicitArg _ (Var _ v)] -> return ([(-1, v)], [])
      LiteralExpr _ _ _ l                     -> do
        cl <- compileLiteral l
        return ([], [cl])
      AddExpr _ _ _ [arg1, arg2]            -> do
        xs <- compileSide (argExpr arg1)
        ys <- compileSide (argExpr arg2)
        return (xs <> ys)
      MulExpr ann1 _ _ [arg1, arg2] -> case (argExpr arg1, argExpr arg2) of
        (LiteralExpr _ _ _ l, Var _ v) -> do
          cl <- compileLiteral l
          return ([(cl, v)],[])
        (Var _ v, LiteralExpr _ _ _ l) -> do
          cl <- compileLiteral l
          return ([(cl, v)],[])
        (e1, e2) -> throwError $ NonLinearConstraint MarabouBackend (provenanceOf ann1) ident e1 e2
      e -> unexpectedExprError currentPass $ prettySimple e

    compileLiteral :: MonadCompile m => Literal -> m Double
    compileLiteral (LBool _) = normalisationError currentPass "LBool"
    compileLiteral (LNat  n) = return $ fromIntegral n
    compileLiteral (LInt  i) = return $ fromIntegral i
    compileLiteral (LRat  q) = return $ fromRational q

    compileRel :: MonadCompile m => Relation -> m (Doc a)
    compileRel (EqualityRel Eq)  = return "="
    compileRel (EqualityRel Neq) =
      throwError $ UnsupportedEquality MarabouBackend (provenanceOf ann) quantifier Neq
    compileRel (OrderRel order)
      -- Suboptimal. See https://github.com/vehicle-lang/vehicle/issues/74 for details.
      | isStrict order = return (pretty $ flipStrictness order)
      | otherwise      = return (pretty order)

    compileVar :: Bool -> (Double, Symbol) -> Doc a
    compileVar False (1,           var) = pretty var
    compileVar True  (1,           var) = "+" <> pretty var
    compileVar _     (-1,          var) = "-" <> pretty var
    compileVar _     (coefficient, var) = pretty coefficient <> pretty var

supportedTypes :: [Builtin]
supportedTypes =
  [ NumericType Real
  , NumericType Rat
  ]

currentPass :: Doc a
currentPass = "compilation to Marabou"