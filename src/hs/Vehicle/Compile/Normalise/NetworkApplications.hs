module Vehicle.Compile.Normalise.NetworkApplications
  ( MetaNetwork
  , convertNetworkAppsToMagicVars
  ) where

import Control.Monad.Except (MonadError(..))
import Control.Monad.Reader (MonadReader(..), runReaderT, asks)
import Control.Monad.State (MonadState(..), runStateT, gets, modify)
import Data.Map qualified as Map (lookup)
import Data.Maybe (fromMaybe)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Bifunctor(Bifunctor(..))

import Vehicle.Backend.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Language.Print (prettySimple, prettyVerbose)
import Vehicle.Compile.LetInsertion (insertLets)
import Vehicle.NeuralNetwork
import Data.Text (Text)

--------------------------------------------------------------------------------
-- Okay so this is a wild ride. The VNNLib format has special variable names for
-- input and output variables, namely X1 ... XN and Y1 ... YM but otherwise has
-- the standard SMTLib syntax. We refer to these variables as "magic variables".
--
-- This means that in theory you can only reason about a single network applied
-- to a single input per property. We get around this restriction by combining
-- multiple networks, or multiple applications of the same network into a
-- single "meta" network. Concretely this process goes as follows for each
-- property we identify in the program.
--
-- It also doesn't (currently) support anything but existential quantifiers.
-- Obviously if a property with just universal quantifiers can be negated to
-- turn them into existentials. However, it does mean it can't handle
-- expressions with mixed quantifiers.
--
-- Consider the example property
--
--   every x1 x2 . x1 <= x2 => f x1 <= f x2
--
-- Compilation therefore proceeds as follows:
--
-- 1. We perform let-lifting of network applications so that every application
-- of a network to a unique input sits in its own let binding underneath a
-- universal quantifier.
--
--  every x1 . let z1 = f x1 in every x2 . let z2 = f x2 in x1 <= x2 => z1 <= z2
--
-- 2. We traverse the expression compiling a list of all network applications,
-- which we refer to as the "meta-network". From this we can generate a list
-- of the magic variables we need.
--
--  meta-network: [f,f]
--  magic-variables: X0 Y0 X1 Y1
--
-- 3. We traverse the resulting expression finding all let-bound
-- applications of the network and equate the inputs with the vector
-- of magic input variables and subsitute the vector of magic output
-- variables into the body of the let expression. For example after
-- processing both let expressions we get:
--
--  every x1 . X0 == x1 => every x2 . X1 == x2 => x1 <= x2 => Y0 <= Y1
--
-- 4. During the process of replacing an application we make note
-- of any locally bound variables that are equated directly to
-- magic variables. On the way back up the traversal of the tree
-- we remove the quantification over these variables and substitute
-- through the magic input variable. We then have:
--
--  X0 == X0 => X1 == X1 => X0 <= X1 => Y0 <= Y1
--
-- 5. We then normalise the expression a second time to get:
--
--  X0 <= X1 => Y0 <= Y1
--
-- 6. Finally we quantify over the magic inputs and outputs to get
--
-- forall X0 Y0 X1 Y1 . X0 <= X1 => Y0 <= Y1

-- | Compiles a given program to a VNNLib script.
-- Assumes the program has already been normalised and that it only
-- contains quantifiers of the provided type.
convertNetworkAppsToMagicVars :: MonadCompile m
                              => Verifier
                              -> NetworkMap
                              -> Quantifier
                              -> CheckedExpr
                              -> m (CheckedExpr, MetaNetwork)
convertNetworkAppsToMagicVars verifier networkMap quantifier expr = do
  logDebug "Beginning conversion of network applications to magic variables"
  incrCallDepth

  networkAppLiftedExpr <- liftNetworkApplications expr
  let metaNetwork = generateMetaNetwork networkAppLiftedExpr
  logDebug $ "Generated meta-network" <+> pretty metaNetwork <> line

  finalExpr <- if null metaNetwork
    then return networkAppLiftedExpr
    else do
      let e = normExpr metaNetwork quantifier networkAppLiftedExpr
      runReaderT e (networkMap, verifier)

  decrCallDepth
  logDebug "Finished compilation to VNNLib"
  return (finalExpr, metaNetwork)

--------------------------------------------------------------------------------
-- Data

type MetaNetwork = [Identifier]

--------------------------------------------------------------------------------
-- Monad

type MonadNetworkApp m =
  ( MonadCompile m
  , MonadReader (NetworkMap, Verifier) m
  )

getNetworkDetailsFromCtx :: MonadNetworkApp m => Identifier -> m NetworkDetails
getNetworkDetailsFromCtx ident = do
  networkMap <- asks fst
  return $ fromMaybe outOfScopeError (Map.lookup ident networkMap)
  where
    outOfScopeError :: a
    outOfScopeError = developerError $
      "Either" <+> squotes (pretty ident) <+> "is not a network or it is not in scope"

--------------------------------------------------------------------------------
-- Algorithm
--------------------------------------------------------------------------------

normExpr :: MonadNetworkApp m
         => MetaNetwork
         -> Quantifier
         -> CheckedExpr
         -> m CheckedExpr
normExpr metaNetwork quantifier expr = do
  metaNetworkDetails <- traverse getNetworkDetailsFromCtx metaNetwork

  -- Replace all applications of neural networks with the magic VNNLib variables
  let numberOfMagicVariables = sum (map networkSize metaNetworkDetails)

  let initialDownwardsState = DownwardsReplacementState 0 0 numberOfMagicVariables
  let initialState = (initialDownwardsState, mempty)
  (updatedExpr, (_down, _up)) <- runStateT (replaceNetworkApplications 0 expr) initialState

  -- Append quantifiers over the magic variables so that it becomes a valid SMTLib expression
  quantifiedExpr <- quantifyOverMagicVariables quantifier metaNetworkDetails updatedExpr
  logDebug $ "Replaced network applications:" <+> prettySimple quantifiedExpr <> line
  return quantifiedExpr

--------------------------------------------------------------------------------
-- Step 1: Lift and combine network applications

-- We lift all network applications regardless if they are duplicated or not to
-- ensure that they are at the top-level underneath a quantifier and hence have
-- a body with the type `Prop`.
liftNetworkApplications :: MonadLogger m => CheckedExpr -> m CheckedExpr
liftNetworkApplications = insertLets isNetworkApplication
  where
    isNetworkApplication :: CheckedCoDBExpr -> Int -> Bool
    isNetworkApplication (App _ (Var _ (CoDBFree _)) _, _) _quantity = True
    isNetworkApplication _                                 _quantity = False

--------------------------------------------------------------------------------
-- Step 2: Generate the meta-network

-- |As we've normalised out all function applications and dataset declarations,
-- the only free names left should be network applications.
generateMetaNetwork :: CheckedExpr -> [Identifier]
generateMetaNetwork = freeNames

--------------------------------------------------------------------------------
-- Steps 3 & 4: instantiating network applications

-- |The state propagated downwards during the pass replacing neural network
-- applications with magic variables.
data DownwardsReplacementState = DownwardsReplacementState
  { magicInputVarCount  :: Int
  , magicOutputVarCount :: Int
  -- This shouldn't really live in the state monad, but we're already within
  -- a reader monad so tough.
  , numberOfMagicVariables :: Int
  }

-- |The state propagated upwards during the pass replacing neural network
-- applications with magic variables.
type UpwardsReplacementState = IntMap Int

type ReplacementState = (DownwardsReplacementState, UpwardsReplacementState)

type MonadReplacement m = (MonadNetworkApp m, MonadState ReplacementState m)

-- Takes in the expression to process and returns a function
-- from the current binding depth to the altered expression.
replaceNetworkApplications :: MonadReplacement m
                           => BindingDepth
                           -> CheckedExpr
                           -> m CheckedExpr
replaceNetworkApplications d e = do
  showEntry d e
  result <- case e of
    Hole _p _      -> resolutionError    currentPass "Hole"
    Meta _p _      -> resolutionError    currentPass "Meta"
    Ann _ann _ _   -> normalisationError currentPass "Ann"
    Lam _ann _ _   -> normalisationError currentPass "Non-quantified Lam"

    Type{}     -> return e
    Pi{}       -> return e
    PrimDict{} -> return e
    Builtin{}  -> return e
    Literal{}  -> return e
    Var{}      -> return e

    LSeq ann dict xs -> do
      dict' <- replaceNetworkApplications d dict
      xs'   <- traverse (replaceNetworkApplications d) xs
      return $ LSeq ann dict' xs'

    Let ann (App _ (Var _ (Free ident)) [inputArg]) _ body -> do
      (newBody, replaceableBoundVars)  <- replaceNetworkApplication ann ident (argExpr inputArg) body d
      -- Don't increment binding depth when we recurse as we've removed the let
      newBody' <- replaceNetworkApplications d newBody
      addReplacableBoundVars replaceableBoundVars
      return newBody'

    Let ann bound binder body -> do
      bound' <- replaceNetworkApplications d bound
      body'  <- replaceNetworkApplications (d + 1) body
      traverseUpOverBinder
      return $ Let ann bound' binder body'

    QuantifierExpr _q ann binder body -> do
      -- Recurse, increasing the binding depth by 1
      body' <- replaceNetworkApplications (d + 1) body

      magicVariableToSubstitute <- processQuantifierBinding
      traverseUpOverBinder

      case magicVariableToSubstitute of
        -- Then this bound variable is not equal to one of the magic variables,
        -- which is not currently supported.
        Nothing -> do
          verifier <- asks (Verifier . snd)
          let symbol = getQuantifierSymbol binder
          throwError $ UnsupportedNonMagicVariable verifier (provenanceOf ann) symbol

        -- Then this bound variable is equal to one of the magic variables so this variable
        -- is redundant and we can substitute through.
        Just magicVarIndex -> do
          let magicVar = Var ann (Bound (magicVarIndex-1))
          let result = magicVar `substInto` body'
          return result

    App ann fun args -> do
      fun'  <- replaceNetworkApplications d fun
      args' <- traverse (traverseExplicitArgExpr (replaceNetworkApplications d)) args
      return $ App ann fun' args'

  showExit result
  return result

replaceNetworkApplication :: MonadReplacement m
                          => CheckedAnn
                          -> Identifier
                          -> CheckedExpr
                          -> CheckedExpr
                          -> BindingDepth
                          -> m (CheckedExpr, IntMap Int)
replaceNetworkApplication ann ident networkInput letBody bindingDepth  = do
  logDebug $ "replacing-application" <+> pretty bindingDepth <+> pretty ident <+> prettySimple networkInput

  network@(NetworkDetails inputs outputs) <- getNetworkDetailsFromCtx ident
  let inputSize  = size inputs
  let inputType  = tElem inputs
  let outputSize = size outputs
  let outputType = tElem outputs

  totalNumberOfMagicVariables <- getNumberOfMagicVariables
  (inputStartingIndex, outputStartingIndex, replaceableBoundVars)
    <- processNetworkApplication bindingDepth network networkInput
  let totalNumberOfMagicVariablesSoFar = inputStartingIndex + outputStartingIndex

  let inputStartingDBIndex  = totalNumberOfMagicVariables + bindingDepth - totalNumberOfMagicVariablesSoFar
  let outputStartingDBIndex = inputStartingDBIndex - inputSize
  let outputEndingDBIndex   = outputStartingDBIndex - outputSize

  let inputVarIndices           = reverse [outputStartingDBIndex .. inputStartingDBIndex-1]
  let outputVarIndices          = reverse [outputEndingDBIndex   .. outputStartingDBIndex-1]
  let (inputsExpr,  inputsType) = mkMagicVariableSeq inputType  inputVarIndices
  let (outputsExpr, _)          = mkMagicVariableSeq outputType outputVarIndices

  logDebug $ pretty (replicate bindingDepth ("." :: String) <> replicate totalNumberOfMagicVariables "_")
  logDebug $ pretty totalNumberOfMagicVariablesSoFar <+> pretty bindingDepth
  logDebug $ pretty inputSize <+> pretty outputSize
  logDebug $ pretty outputEndingDBIndex <+> pretty outputStartingDBIndex <+> pretty inputStartingDBIndex
  logDebug $ pretty inputVarIndices <+> pretty outputVarIndices

  let body'         = outputsExpr `substInto` letBody
  let inputEquality = EqualityExpr Eq ann inputsType Prop (map (ExplicitArg ann) [inputsExpr, networkInput])
  let newBody       = ImplExpr ann Prop (map (ExplicitArg ann) [inputEquality, body'])

  return (newBody, replaceableBoundVars)
  where
    mkMagicVariableSeq :: Builtin -> [Int] -> (CheckedExpr, CheckedExpr)
    mkMagicVariableSeq tElem indices = (tensorExpr, tensorType)
      where
        tensorElemType   = Builtin ann tElem
        tensorType       = mkTensorType ann tensorElemType [length indices]
        variables        = map (Var ann . Bound) indices
        tensorExpr       = SeqExpr ann tensorElemType tensorType variables

-- |Called at the site of an application of a neural network.
-- It updates the number of magic variables used and identifies any
-- locally bound variables that can be replaced with magic variables.
processNetworkApplication :: MonadReplacement m
                          => BindingDepth
                          -> NetworkDetails
                          -> CheckedExpr
                          -> m (Int, Int, IntMap Int)
processNetworkApplication d network input = do
  (DownwardsReplacementState{..}, up) <- get

  let totalMagicVarCountSoFar  = magicInputVarCount + magicOutputVarCount
  let newMagicInputVarCount  = magicInputVarCount + size (inputTensor network)
  let newMagicOutputVarCount = magicInputVarCount + size (outputTensor network)

  totalNumberOfMagicVariables <- getNumberOfMagicVariables
  let magicInputVarStartingIndex = totalNumberOfMagicVariables + d - totalMagicVarCountSoFar - 1

  logDebug $ prettyVerbose input
  let localReplacableBoundVars = case input of
          SeqExpr _ _ _ xs -> getReplacableBoundVars magicInputVarStartingIndex xs
          _                -> normalisationError currentPass "LSeq"

  let down = DownwardsReplacementState
        { magicInputVarCount  = newMagicInputVarCount
        , magicOutputVarCount = newMagicOutputVarCount
        , ..
        }

  put (down, up)
  return (magicInputVarCount, magicOutputVarCount, localReplacableBoundVars)
    where
      getReplacableBoundVars :: Int -> [CheckedExpr] -> IntMap Int
      getReplacableBoundVars _             []       = mempty
      getReplacableBoundVars magicVarIndex (x : xs) =
        let recRes = getReplacableBoundVars (magicVarIndex - 1) xs in
          case x of
            Var _ (Bound i) -> IntMap.insert i magicVarIndex recRes
            _               -> recRes

processQuantifierBinding :: MonadReplacement m => m (Maybe Int)
processQuantifierBinding = do
  (down, replacableBoundVars) <- get
  let (magicVarUsingBinder, newMapping) = IntMap.updateLookupWithKey (\_ _ -> Nothing) 0 replacableBoundVars
  put (down, newMapping)
  return magicVarUsingBinder

addReplacableBoundVars :: MonadReplacement m => IntMap Int -> m ()
addReplacableBoundVars vars = modify (second (vars <>))

traverseUpOverBinder :: MonadReplacement m => m ()
traverseUpOverBinder = modify (second (incrementKeys . incrementValues))
  where
    incrementKeys   = ((\x -> x-1) <$>)
    incrementValues = IntMap.mapKeysMonotonic (\x -> x-1)

getNumberOfMagicVariables :: MonadReplacement m => m Int
getNumberOfMagicVariables = gets (numberOfMagicVariables . fst)


showEntry :: MonadReplacement m
          => BindingDepth
          -> CheckedExpr
          -> m ()
showEntry d e = do
  logDebug $ "replace-entry:" <+> pretty d <+> prettySimple e
  incrCallDepth

showExit :: MonadReplacement m
         => CheckedExpr
         -> m ()
showExit e = do
  (_, replacableBoundVars) <- get

  decrCallDepth
  logDebug $ "replace-exit: " <+> align (
    prettySimple e <> if IntMap.null replacableBoundVars
      then ""
      else softline <> parens ("replacableBoundVars =" <+> pretty (IntMap.toAscList replacableBoundVars)))

--------------------------------------------------------------------------------
-- Step 6: quantification over magic variables

quantifyOverMagicVariables :: MonadNetworkApp m
                           => Quantifier
                           -> [NetworkDetails]
                           -> CheckedExpr
                           -> m CheckedExpr
quantifyOverMagicVariables q metaNetwork prop = do
  verifier <- asks snd
  let totalInputs  = sum (map (size . inputTensor)  metaNetwork)
  let totalOutputs = sum (map (size . outputTensor) metaNetwork)
  let (_, _, result) = foldr (forNetwork verifier) (totalInputs, totalOutputs, prop) metaNetwork
  return result
  where
    forNetwork :: Verifier -> NetworkDetails -> (Int, Int, CheckedExpr) -> (Int, Int, CheckedExpr)
    forNetwork verifier (NetworkDetails inputs outputs) (inputIndex, outputIndex, body) =
      let (inputPrefix, outputPrefix) = magicVariablePrefixes verifier in
      let startingInputIndex = inputIndex - size inputs in
      let startingOutputIndex = outputIndex - size outputs in
      let body' = forTensor mempty inputPrefix  startingInputIndex  inputs $
                  forTensor mempty outputPrefix startingOutputIndex outputs body in
      (startingInputIndex, startingOutputIndex, body')

    forTensor :: CheckedAnn
              -> Text
              -> Int
              -> TensorDetails
              -> CheckedExpr
              -> CheckedExpr
    forTensor ann prefix startingIndex (TensorDetails size tElem) body =
      let indices = reverse [startingIndex .. startingIndex + size-1] in
      let names   = mkMagicVariableNames prefix indices in
      let varType = Builtin ann tElem in
      mkQuantifierSeq q ann (map Just names) varType body

    mkMagicVariableNames :: Text -> [Int] -> [Symbol]
    mkMagicVariableNames prefix indices = [mkNameWithIndices prefix [i] | i <- indices]

currentPass :: Doc a
currentPass = "insertion of magic network variables"