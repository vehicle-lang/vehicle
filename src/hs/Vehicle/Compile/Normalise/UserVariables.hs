module Vehicle.Compile.Normalise.UserVariables
  ( MetaNetwork
  , normUserVariables
  ) where

import Control.Monad.Except (MonadError(..))
import Control.Monad.Reader (MonadReader(..), runReaderT)
import Data.Map (Map)
import Data.Map qualified as Map (lookup, singleton, unionWith)
import Data.Traversable (forM)
import Data.Text (Text)

import Vehicle.Backend.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Language.Print (prettySimple)
import Vehicle.Compile.LetInsertion (insertLets)
import Vehicle.Compile.Resource
import Vehicle.Compile.Linearity
import Vehicle.Compile.Normalise.QuantifierLifting (liftQuantifiers)
import Vehicle.Compile.Normalise

--------------------------------------------------------------------------------
-- Removing network applications
--
-- Okay so this is a wild ride. The Marabou query format has special variable
-- names for input and output variables, namely x1 ... xN and y1 ... yM but
-- otherwise has the standard SMTLib syntax. We refer to these variables as
-- "magic variables".
--
-- This means that in theory you can only reason about a single network applied
-- to a single input per property. We get around this restriction by combining
-- multiple networks, or multiple applications of the same network into a
-- single "meta" network. Concretely this process goes as follows for each
-- property we identify in the program.
--
--
-- Consider the example property
--
--   exists a1 a2 . a1 <= a2 => f (a1 + 1) <= f a2
--
-- Compilation therefore proceeds as follows:
--
-- 1. We perform let-lifting of network applications so that forall application
-- of a network to a unique input sits in its own let binding underneath a
-- universal quantifier.
--
--  exists a1 a2 . let z1 = f (a1 + 1) in let z2 = f a2 in a1 <= a2 => z1 <= z2
--
-- 2. We traverse the expression compiling a list of all network applications,
-- which we refer to as the "meta-network". From this we can generate a list
-- of the magic variables we need.
--
--  meta-network: [f,f]
--  magic-variables: x0 y0 x1 y1
--
-- 3. We traverse the resulting expression finding all let-bound
-- applications of the network and equate the inputs with the vector
-- of magic input variables and subsitute the vector of magic output
-- variables into the body of the let expression. For example after
-- processing both let expressions we get:
--
--  exists a1 . exists a2 . a1 <= a2 => y0 <= y1
--
-- with equations:
--
--  1) x0 == a1 + 1
--  2) x1 == a2
--
-- 4. We then use Gaussian elimination to solve for the user variables, e.g.
--
--  1) a1 == x0 - 1
--  2) a2 == x1
--
-- 5. We then substitute these solutions through the remaining equations to get:
--
--  x0 - 1 <= x1 => y0 <= y1

-- | Converts all user quantified variables to magic I/O variables.
normUserVariables :: MonadCompile m
                  => Identifier
                  -> Verifier
                  -> NetworkContext
                  -> CheckedExpr
                  -> m (CLSTProblem, MetaNetwork, UserVarReconstructionInfo)
normUserVariables ident verifier networkCtx expr =
  logCompilerPass MinDetail "input/output variable insertion" $ do
    -- First lift all the quantifiers to the top-level
    quantLiftedExpr <- liftQuantifiers expr

    -- Next let-lift all the network applications to avoid duplicates.
    liftedNetworkAppExpr <- liftNetworkApplications quantLiftedExpr

    -- We can now calculate the meta-network.
    let metaNetwork = generateMetaNetwork liftedNetworkAppExpr
    metaNetworkDetails <- traverse (getNetworkDetailsFromCtx networkCtx) metaNetwork
    logDebug MinDetail $ "Generated meta-network" <+> pretty metaNetwork <> line

    -- Next remove all the user quantifiers which we assume are at the top-level.
    (quantifierlessExpr, userVariables) <- removeUserQuantifiers ident liftedNetworkAppExpr

    -- We prepend user variables with a special character to distinguish them in the
    -- logs in the case that the user variables coincide with the magic variables.
    let userVariableNames = fmap ("u'" <>) userVariables
    let magicVariableNames = getMagicVariablesNames verifier metaNetworkDetails

    -- Then generate teh
    flip runReaderT (networkCtx, ident, metaNetwork, verifier, userVariableNames, magicVariableNames) $ do
      generateCLSTProblem quantifierlessExpr

generateCLSTProblem :: MonadSMT m
                    => CheckedExpr
                    -> m (CLSTProblem, MetaNetwork, UserVarReconstructionInfo)
generateCLSTProblem assertionsExpr = do
  (_, _, metaNetwork, _, userVariables, _) <- ask
  variableNames <- getVariableNames

  -- Substitute through the let-bound applications.
  (userExprBody, inputEqualityAssertions) <-
    replaceNetworkApplications (IOVarState 0 0) assertionsExpr

  -- Normalise to remove newly introduced lookups into tensors of
  -- output variables
  boundCtx <- getBoundContext
  normExprBody <- normalise userExprBody $ defaultNormalisationOptions
    { implicationsToDisjunctions = True
    , subtractionToAddition      = True
    , expandOutPolynomials       = True
    , boundContext               = boundCtx
    }

  userAssertions <- compileAssertions normExprBody

  let assertions = inputEqualityAssertions <> userAssertions
  let clst = CLSTProblem variableNames assertions

  (solvedCLST, userVarReconstruction) <-
    solveForUserVariables (length userVariables) clst

  logCompilerPassOutput $ pretty solvedCLST
  return (solvedCLST, metaNetwork, userVarReconstruction)

--------------------------------------------------------------------------------
-- Monad

type UserVariableNames = [Symbol]
type MagicVariableNames = [Symbol]

type MonadSMT m =
  ( MonadCompile m
  , MonadReader
    ( NetworkContext
    , Identifier
    , MetaNetwork
    , Verifier
    , UserVariableNames
    , MagicVariableNames
    ) m
  )

getNetworkDetailsFromCtx :: MonadCompile m => NetworkContext -> Symbol -> m NetworkType
getNetworkDetailsFromCtx networkCtx name = do
  case Map.lookup name networkCtx of
    Just details -> return details
    Nothing      -> compilerDeveloperError $
      "Either" <+> squotes (pretty name) <+> "is not a network or it is not in scope"

getBoundContext :: MonadSMT m => m [DBBinding]
getBoundContext = do
  (_, _, _, _, userVariableNames, magicVariableNames) <- ask
  return $ fmap Just (reverse userVariableNames <> magicVariableNames)

getNumberOfUserVariables :: MonadSMT m => m Int
getNumberOfUserVariables = do
  (_, _, _, _, userVariables, _) <- ask
  return $ length userVariables

getMetaNetworkType :: MonadSMT m => m [NetworkType]
getMetaNetworkType = do
  (networkCtx, _, metaNetwork, _, _, _) <- ask
  traverse (getNetworkDetailsFromCtx networkCtx) metaNetwork

getNumberOfMagicVariables :: MonadSMT m => m Int
getNumberOfMagicVariables = sum . fmap networkSize <$> getMetaNetworkType

getTotalNumberOfVariables :: MonadSMT m => m Int
getTotalNumberOfVariables = do
  numberOfUserVariables <- getNumberOfUserVariables
  numberOfMagicVariables <- getNumberOfMagicVariables
  return $ numberOfUserVariables + numberOfMagicVariables

getExprSize :: MonadSMT m => m Int
getExprSize =
  -- Add one more for the constant term.
  (1 +) <$> getTotalNumberOfVariables

getVariableNames :: MonadSMT m => m VariableNames
getVariableNames = do
  (_, _, _, _, userVariableNames, magicVariableNames) <- ask
  return $ userVariableNames <> magicVariableNames

getExprConstantIndex :: MonadSMT m => m Int
getExprConstantIndex =
  -- The contant in the linear expression is stored in the last index.
  getTotalNumberOfVariables

--------------------------------------------------------------------------------
-- Algorithm
--------------------------------------------------------------------------------
-- Remove user quantifiers

-- | Strip off user quantifiers
removeUserQuantifiers :: MonadCompile m
                      => Identifier
                      -> CheckedExpr
                      -> m (CheckedExpr, [Symbol])
removeUserQuantifiers ident (ExistsExpr ann binder body) = do
  let n = getBinderSymbol binder
  case typeOf binder of
    RatType{}-> do
      (result, binders) <- removeUserQuantifiers ident body
      return (result, n : binders)
    t -> do
      throwError $ UnsupportedVariableType MarabouBackend ident ann n t supportedTypes
removeUserQuantifiers _ e = return (e, [])

-- | We lift all network applications regardless if they are duplicated or not to
-- ensure that they are at the top-level underneath a quantifier and hence have
-- a body with the type `Bool`.
liftNetworkApplications :: MonadCompile m => CheckedExpr -> m CheckedExpr
liftNetworkApplications = insertLets isNetworkApplication False
  where
    isNetworkApplication :: CheckedCoDBExpr -> Int -> Bool
    isNetworkApplication (App _ (Var _ (CoDBFree _)) _, _) _quantity = True
    isNetworkApplication _                                 _quantity = False

--------------------------------------------------------------------------------
-- Generate the meta-network

-- |As we've normalised out all function applications and dataset declarations,
-- the only free names left should be network applications.
generateMetaNetwork :: CheckedExpr -> MetaNetwork
generateMetaNetwork e = fmap nameOf (freeNamesIn e)

--------------------------------------------------------------------------------
-- Steps 3 & 4: replace network applications

-- |The state propagated downwards during the pass replacing neural network
-- applications with magic variables.
data IOVarState = IOVarState
  { magicInputVarCount  :: Int
  , magicOutputVarCount :: Int
  }

pattern NetworkApp :: Provenance -> Identifier -> CheckedExpr -> CheckedExpr
pattern NetworkApp ann ident inputs <- App ann (Var _ (Free ident)) [ExplicitArg _ inputs]

-- Takes in the expression to process and returns a function
-- from the current binding depth to the altered expression.
replaceNetworkApplications :: MonadSMT m
                           => IOVarState
                           -> CheckedExpr
                           -> m (CheckedExpr, [Assertion])
replaceNetworkApplications IOVarState{..} (Let _ (NetworkApp ann ident inputExprs) _binder body) = do
  logDebug MaxDetail $ "Replacing application:" <+> pretty ident <+> prettySimple inputExprs
  incrCallDepth
  (networkCtx, _, _, _, _, _) <- ask

  NetworkType inputs outputs <- getNetworkDetailsFromCtx networkCtx (nameOf ident)
  let inputSize  = size inputs
  let outputSize = size outputs
  let outputType = tElem outputs

  numberOfUserVariables <- getNumberOfUserVariables
  let inputStartingDBIndex    = numberOfUserVariables + magicInputVarCount + magicOutputVarCount
  let outputStartingDBIndex   = inputStartingDBIndex  + inputSize
  let outputEndingDBIndex     = outputStartingDBIndex + outputSize
  let inputVarIndices         = [inputStartingDBIndex   .. outputStartingDBIndex-1]
  let outputVarIndices        = [outputStartingDBIndex  .. outputEndingDBIndex-1]

  inputVarEqualities <- createInputVarEqualities inputVarIndices inputExprs
  let outputVarsExpr = mkMagicVariableSeq ann outputType outputVarIndices

  logDebug MaxDetail $ "starting index:            " <+> pretty inputStartingDBIndex
  logDebug MaxDetail $ "number of input variables: " <+> pretty inputSize
  logDebug MaxDetail $ "number of output variables:" <+> pretty outputSize
  logDebug MaxDetail $ "input indices:             " <+> pretty inputVarIndices
  logDebug MaxDetail $ "output indices:            " <+> pretty outputVarIndices
  variableNames <- getVariableNames
  logDebug MidDetail $ "input variable equalities:" <> line <>
    pretty (CLSTProblem variableNames inputVarEqualities) <> line

  let newBody = outputVarsExpr `substInto` body
  (result, equalities) <- flip replaceNetworkApplications newBody $ IOVarState
    { magicInputVarCount  = magicInputVarCount + inputSize
    , magicOutputVarCount = magicInputVarCount + outputSize
    }

  decrCallDepth
  return (result, inputVarEqualities <> equalities)

replaceNetworkApplications _ e = return (e, [])

createInputVarEqualities :: MonadSMT m => [Int] -> CheckedExpr -> m [Assertion]
createInputVarEqualities inputVarIndices (SeqExpr _ _ _ xs) = do
  forM (zip xs inputVarIndices) $ \(e, i) -> do
    -- Create linear expression equating the magic variable `x_i`
    -- with the expression `e` in the relevant point = xs_i`
    exprSize <- getExprSize
    let lhs = linearExprFromMap exprSize (Map.singleton i 1)
    rhs <- compileLinearExpr e

    return $ constructAssertion (lhs, Equals, rhs)
createInputVarEqualities _ _ = normalisationError currentPass "non-Seq"

mkMagicVariableSeq :: Provenance -> NetworkBaseType -> [Int] -> CheckedExpr
mkMagicVariableSeq ann tElem indices = tensorExpr
  where
  tensorElemType   = reconstructNetworkBaseType ann tElem
  variables        = map (Var ann . Bound) indices
  dim              = NatLiteralExpr ann (NatType ann) (length indices)
  tensorExpr       = mkTensor ann tensorElemType [dim] variables

compileAssertions :: MonadSMT m
                  => CheckedExpr
                  -> m [Assertion]
compileAssertions expr = case expr of
  Universe{}             -> typeError          currentPass "Universe"
  Pi{}                   -> typeError          currentPass "Pi"
  Hole{}                 -> resolutionError    currentPass "Hole"
  Meta{}                 -> resolutionError    currentPass "Meta"
  Ann{}                  -> normalisationError currentPass "Ann"
  Lam{}                  -> normalisationError currentPass "Lam"
  Let{}                  -> normalisationError currentPass "Let"
  LSeq{}                 -> normalisationError currentPass "LSeq"
  PrimDict{}             -> visibilityError    currentPass "PrimDict"
  Builtin{}              -> normalisationError currentPass "LSeq"
  QuantifierExpr q _ _ _ -> normalisationError currentPass (pretty q)
  Var{}                  -> caseError          currentPass "Var" ["Order", "Eq"]

  Literal _ann l -> case l of
    LBool _ -> normalisationError currentPass "LBool"
    _       -> caseError currentPass "Literal" ["AndExpr"]

  AndExpr _ [ExplicitArg _ e1, ExplicitArg _ e2] -> do
    as1 <- compileAssertions e1
    as2 <- compileAssertions e2
    return (as1 <> as2)

  OrderExpr _ ord _ [ExplicitArg _ e1, ExplicitArg _ e2] -> do
    let (rel, lhs, rhs) = case ord of
          Lt -> (LessThan,          e1, e2)
          Le -> (LessThanOrEqualTo, e1, e2)
          Gt -> (LessThan,          e2, e1)
          Ge -> (LessThanOrEqualTo, e2, e1)
    assertion <- compileAssertion rel lhs rhs
    return [assertion]

  EqualityExpr ann eq _ [ExplicitArg _ e1, ExplicitArg _ e2] -> case eq of
    Neq -> do
      (_, ident, _, _, _, _) <- ask
      throwError $ UnsupportedInequality MarabouBackend ident ann
    Eq  -> do
      assertion <- compileAssertion Equals e1 e2
      return [assertion]

  App{} -> unexpectedExprError currentPass (prettySimple expr)

compileAssertion :: MonadSMT m
                 => Relation
                 -> CheckedExpr
                 -> CheckedExpr
                 -> m Assertion
compileAssertion rel lhs rhs = do
  lhsLinExpr <- compileLinearExpr lhs
  rhsLinExpr <- compileLinearExpr rhs
  return $ constructAssertion (lhsLinExpr, rel, rhsLinExpr)

compileLinearExpr :: MonadSMT m => CheckedExpr -> m LinearExpr
compileLinearExpr expr = do
  varToCoefficientMap <- go expr
  exprSize <- getExprSize
  return $ linearExprFromMap exprSize varToCoefficientMap
  where
  singletonVar :: MonadSMT m => DBVar -> Coefficient -> m (Map Int Coefficient)
  singletonVar Free{}    _ = normalisationError currentPass "FreeVar"
  singletonVar (Bound v) c = do
    numberOfUserVariables <- getNumberOfUserVariables
    let i = if v < numberOfUserVariables then numberOfUserVariables - v - 1 else v
    return $ Map.singleton i c

  go :: MonadSMT m => CheckedExpr -> m (Map Int Coefficient)
  go e = case e of
    Var     _ v                           -> singletonVar v 1
    NegExpr _ _ _ [ExplicitArg _ (Var _ v)] -> singletonVar v (-1)
    LiteralExpr _ _ _ l                   -> do
      constIndex <- getExprConstantIndex
      singletonVar (Bound constIndex) =<< compileLiteral l
    AddExpr _ _ _ _ _ [ExplicitArg _ e1, ExplicitArg _ e2] -> do
      Map.unionWith (+) <$> go e1 <*> go e2
    MulExpr _ _ _ _ _ [ExplicitArg _ e1, ExplicitArg _ e2] ->
      case (e1, e2) of
        (LiteralExpr _ _ _ l, Var _ v) -> singletonVar v =<< compileLiteral l
        (Var _ v, LiteralExpr _ _ _ l) -> singletonVar v =<< compileLiteral l
        _ -> do
          (_, _ident, _, _, _, _) <- ask
          _ctx <- getBoundContext
          compilerDeveloperError $
            "Unexpected non-linear constraint that should have been caught by the" <+>
            "linearity analysis during type-checking."
    ex -> unexpectedExprError currentPass $ prettySimple ex

compileLiteral :: MonadSMT m => Literal -> m Coefficient
compileLiteral (LBool _) = normalisationError currentPass "LBool"
compileLiteral (LNat  n) = return $ fromIntegral n
compileLiteral (LInt  i) = return $ fromIntegral i
compileLiteral (LRat  q) = return $ fromRational q

--------------------------------------------------------------------------------
-- Step 6: quantification over magic variables

getMagicVariablesNames :: Verifier
                       -> [NetworkType]
                       -> [Text]
getMagicVariablesNames verifier metaNetworkDetails =
  let (_, _, result) = foldr forNetwork (0, 0, []) metaNetworkDetails in result
  where
    (inputPrefix, outputPrefix) = magicVariablePrefixes verifier

    forNetwork :: NetworkType -> (Int, Int, [Text]) -> (Int, Int, [Text])
    forNetwork (NetworkType inputs outputs) (inputIndex, outputIndex, result) =
      let nextInputIndex  = inputIndex  + size inputs in
      let nextOutputIndex = outputIndex + size outputs in
      let inputNames  = forTensor inputPrefix  inputIndex  inputs in
      let outputNames = forTensor outputPrefix outputIndex outputs in
      (nextInputIndex, nextOutputIndex, result <> inputNames <> outputNames)

    forTensor :: Text -> Int -> NetworkTensorType -> [Text]
    forTensor prefix startingIndex (NetworkTensorType size _) =
      let indices = [startingIndex .. startingIndex + size-1] in
      [mkNameWithIndices prefix [i] | i <- indices]

currentPass :: Doc a
currentPass = "insertion of magic network variables"

supportedTypes :: [Builtin]
supportedTypes =
  [ NumericType Rat
  ]