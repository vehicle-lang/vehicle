module Vehicle.Backend.Marabou.Compile
  ( compile
  ) where

import Control.Monad.Except (MonadError(..))
import Data.Maybe (catMaybes)
import Data.Bifunctor (Bifunctor(first))

import Vehicle.Language.Print (prettySimple, prettyFriendly)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise (normalise, NormalisationOptions(..))
import Vehicle.Compile.Normalise.NetworkApplications
import Vehicle.Compile.Normalise.IfElimination (liftAndEliminateIfs)
import Vehicle.Compile.Normalise.DNF (convertToDNF, splitDisjunctions)
import Vehicle.Compile.Descope (runDescope)
import Vehicle.Compile.SupplyNames (supplyDBNames)
import Vehicle.Compile.QuantifierAnalysis (checkQuantifiersAreHomogeneous)
import Vehicle.Backend.Prelude
import Vehicle.Backend.Marabou.Core
import Vehicle.NeuralNetwork

--------------------------------------------------------------------------------
-- Compilation to Marabou

-- | Compiles the provided program to Marabou queries.
compile :: MonadCompile m => NetworkMap -> CheckedProg -> m [MarabouProperty]
compile networkMap prog = do
  logDebug "Beginning compilation to Marabou"
  incrCallDepth

  result <- compileProg networkMap prog

  decrCallDepth
  logDebug "Finished compilation to Marabou"
  return result

--------------------------------------------------------------------------------
-- Algorithm

compileProg :: MonadCompile m => NetworkMap -> CheckedProg -> m [MarabouProperty]
compileProg networkMap (Main ds) = do
  results <- catMaybes <$> traverse (compileDecl networkMap) ds
  if null results then
    throwError NoPropertiesFound
  else
    return results

compileDecl :: MonadCompile m => NetworkMap -> CheckedDecl -> m (Maybe MarabouProperty)
compileDecl networkMap d = case d of
  DeclData{} -> normalisationError currentPass "Dataset declarations"
  DeclNetw{} -> normalisationError currentPass "Network declarations"

  DefFun _p ident t expr ->
    if not $ isProperty t
      -- If it's not a property then we can discard it as all applications
      -- of it should have been normalised out by now.
      then return Nothing
      else Just <$> compileProperty ident networkMap expr

compileProperty :: MonadCompile m
                => Identifier
                -> NetworkMap
                -> CheckedExpr
                -> m MarabouProperty
compileProperty ident networkMap expr = do
  let identDoc = squotes (pretty ident)
  let ann = annotationOf expr
  logDebug $ "Beginning compilation of property" <+> identDoc
  incrCallDepth

  -- Check that we only have one type of quantifier in the property
  quantifier <- checkQuantifiersAreHomogeneous MarabouBackend ident expr

  logDebug $ line <> "Quantifier type: " <> pretty (Quant quantifier)

  -- If the property is universally quantified then we need to negate the expression
  let (isPropertyNegated, possiblyNegatedExpr) =
        if quantifier == Any
          then (False, expr)
          else (True,  NotExpr ann Prop [ExplicitArg ann expr])

  -- Eliminate any if-expressions
  ifFreeExpr <- liftAndEliminateIfs possiblyNegatedExpr

  logDebug $ line <> "After if-elimination: " <> prettyFriendly ifFreeExpr

  -- Normalise the expression to remove any implications, push the negations through
  -- and expand out any multiplication.
  normExpr <- normalise (Options
    { implicationsToDisjunctions = True
    , subtractionToAddition      = True
    , expandOutPolynomials       = True
    }) ifFreeExpr

  -- Convert to disjunctive normal form
  dnfExpr <- convertToDNF normExpr

  logDebug $ line <> "After conversion to DNF: " <> prettyFriendly dnfExpr

  -- Split up into the individual queries needed for Marabou.
  let queryExprs = splitDisjunctions dnfExpr

  -- Compile the individual queries
  queries <- traverse (compileQuery ident networkMap quantifier) queryExprs

  decrCallDepth
  logDebug $ "Finished compilation of property" <+> identDoc

  return $ MarabouProperty (nameOf ident) isPropertyNegated queries

compileQuery :: MonadCompile m
             => Identifier
             -> NetworkMap
             -> Quantifier
             -> CheckedExpr
             -> m MarabouQuery
compileQuery ident networkMap originalQuantifier expr = do
  -- Convert all applications of networks into magic variables
  (networklessExpr, metaNetwork) <- convertNetworkAppsToMagicVars Marabou networkMap Any expr

  -- Normalise the expression to remove any implications, push the negations through
  -- and expand out any multiplication.
  normExpr <- normalise (Options
    { implicationsToDisjunctions = True
    , subtractionToAddition      = True
    , expandOutPolynomials       = True
    }) networklessExpr

  -- Descope the expression, converting from DeBruijn indices to names
  let descopedExpr = runDescope [] (supplyDBNames normExpr)

  (vars, assertionDocs) <- compileAssertions ident originalQuantifier descopedExpr
  let doc = vsep assertionDocs

  logDebug $ "Output:" <> align (line <> doc)

  decrCallDepth
  logDebug $ "Finished compilation of SMTLib query" <+> squotes (pretty ident)

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

  Literal _ann l -> return $ case l of
    LBool _ -> normalisationError currentPass "LBool"
    _       -> caseError currentPass "Literal" ["AndExpr"]

  QuantifierExpr _ _ binder body -> do
    var <- compileBinder ident binder
    (vars, docs) <- compileAssertions ident quantifier body
    return (var : vars, docs)

  AndExpr _ _ [e1, e2] -> do
    (vars1, docs1) <- compileAssertions ident quantifier (argExpr e1)
    (vars2, docs2) <- compileAssertions ident quantifier (argExpr e2)
    return (vars1 <> vars2, docs1 <> docs2)

  OrderExpr order ann _ _ [lhs, rhs] -> do
    assertion <- compileAssertion ann ident quantifier (OrderRel order) (argExpr lhs) (argExpr rhs)
    return ([], [assertion])

  EqualityExpr eq ann _ _ [lhs, rhs] -> do
    assertion <- compileAssertion ann ident quantifier (EqualityRel eq) (argExpr lhs) (argExpr rhs)
    return ([], [assertion])

  App{} -> developerError $ unexpectedExprError currentPass (prettySimple expr)

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
  return $ hsep (fmap compileVar finalVars) <+> compiledRel <+> pretty finalConstant
  where
    flipConstants :: [Double] -> [Double]
    flipConstants = fmap ((-1) *)

    flipVars :: [(Double, Symbol)] -> [(Double, Symbol)]
    flipVars = fmap (first ((-1) *))

    compileSide :: MonadCompile m => OutputExpr -> m ([(Double, Symbol)], [Double])
    compileSide = \case
      Var     _ v                           -> return ([(1, v)], [])
      NegExpr _ _ [ExplicitArg _ (Var _ v)] -> return ([(-1, v)], [])
      LiteralExpr _ _ l                     -> return ([], [compileLiteral l])
      AddExpr _ _ _ [arg1, arg2]            -> do
        xs <- compileSide (argExpr arg1)
        ys <- compileSide (argExpr arg2)
        return (xs <> ys)
      MulExpr ann1 _ _ [arg1, arg2] -> case (argExpr arg1, argExpr arg2) of
        (LiteralExpr _ _ l, Var _ v) -> return ([(compileLiteral l, v)],[])
        (Var _ v, LiteralExpr _ _ l) -> return ([(compileLiteral l, v)],[])
        (e1, e2) -> throwError $ NonLinearConstraint MarabouBackend (provenanceOf ann1) ident e1 e2
      e -> developerError $ unexpectedExprError currentPass $ prettySimple e

    compileLiteral :: Literal -> Double
    compileLiteral (LBool _) = normalisationError currentPass "LBool"
    compileLiteral (LNat  n) = fromIntegral n
    compileLiteral (LInt  i) = fromIntegral i
    compileLiteral (LRat  q) = fromRational q

    compileRel :: MonadCompile m => Relation -> m (Doc a)
    compileRel (EqualityRel Eq)  = return "="
    compileRel (EqualityRel Neq) =
      throwError $ UnsupportedEquality MarabouBackend (provenanceOf ann) quantifier Neq
    compileRel (OrderRel order)
      | isStrict order = do
        throwError $ UnsupportedOrder MarabouBackend (provenanceOf ann) quantifier order
        {-
        let nonStrictOrder = flipStrictness order
        logWarning $
          "Performed possibly unsound conversion of" <+> squotes (pretty order) <+>
          "to" <+> squotes (pretty nonStrictOrder) <> " in order to ensure Marabou" <+>
          "support."
        compileRel $ OrderRel nonStrictOrder
        -}
      | otherwise = return $ pretty order

    compileVar :: (Double, Symbol) -> Doc a
    compileVar (-1,          var) = "-" <> pretty var
    compileVar (1,           var) = "+" <> pretty var
    compileVar (coefficient, var) = pretty coefficient <> pretty var

supportedTypes :: [Builtin]
supportedTypes =
  [ NumericType Real
  , NumericType Rat
  ]

currentPass :: Doc a
currentPass = "compilation to Marabou"