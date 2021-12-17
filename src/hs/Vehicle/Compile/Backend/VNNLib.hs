module Vehicle.Compile.Backend.VNNLib
  ( VNNLibDoc(..)
  , compileToVNNLib
  ) where

import Control.Monad.Reader (MonadReader(..), runReaderT, asks)
import Control.Monad.Except (MonadError(..))
import Control.Monad.State (MonadState(..), runStateT, gets)
import Data.Map qualified as Map (lookup)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Maybe (catMaybes, fromMaybe)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap

import Vehicle.Prelude
import Vehicle.Compile.Error
import Vehicle.Language.AST hiding (Map)
import Vehicle.Language.Print (prettySimple, prettyVerbose)
import Vehicle.Compile.Normalise (normaliseInternal)
import Vehicle.Compile.StandardiseNetworks
import Vehicle.Compile.Backend.SMTLib (SMTDoc)
import Vehicle.Compile.Backend.SMTLib qualified as SMTLib (compileProp)

--------------------------------------------------------------------------------
-- Compilation to VNNLib
--
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
-- universal quantifier. (STILL TO DO)
--
--  every x1 x2 . let z1 = f x1 in let z2 = f x2 in x1 <= x2 => z1 <= z2
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
--  every x1 x2 . X0 == x1 => X1 == x2 => x1 <= x2 => Y0 <= Y1
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
--
-- 7. The property should then be a valid SMTLib expression so we now
-- compile it to SMTLib as normal.
--
-- 8. We return the meta-network composition so that we can actually
-- perform the required hackery on the network files elsewhere.

-- | Compiles a given program to a VNNLib script.
-- Assumes the program has already been normalised.
compileToVNNLib :: (AsSMTLibError e, MonadLogger m, MonadError e m)
                => NetworkMap
                -> CheckedProg
                -> m [VNNLibDoc]
compileToVNNLib networkMap prog = do
  logDebug "Beginning compilation to VNNLib"
  incrCallDepth
  result <- runReaderT (compileProg prog) networkMap
  decrCallDepth
  logDebug "Finished compilation to VNNLib"
  return result
  --return []

--------------------------------------------------------------------------------
-- Data

data VNNLibDoc = VNNLibDoc
  { smtDoc      :: SMTDoc
  , metaNetwork :: MetaNetwork
  }

type MetaNetwork = [Identifier]

--------------------------------------------------------------------------------
-- Monad

type MonadVNNLib e m =
  ( AsSMTLibError e
  , MonadLogger m
  , MonadError e m
  , MonadReader NetworkMap m
  )

getNetworkDetailsFromCtx :: MonadVNNLib e m => Identifier -> m NetworkDetails
getNetworkDetailsFromCtx ident = asks (fromMaybe outOfScopeError . Map.lookup ident)
  where
    outOfScopeError :: a
    outOfScopeError = developerError $
      "Either" <+> squotes (pretty ident) <+> "is not a network or it is not in scope"

--------------------------------------------------------------------------------
-- Algorithm
--------------------------------------------------------------------------------

compileProg :: MonadVNNLib e m => CheckedProg -> m [VNNLibDoc]
compileProg (Main ds) = do
  results <- catMaybes <$> traverse compileDecl ds
  if null results then
    throwError mkNoPropertiesFound
  else
    return results

compileDecl :: MonadVNNLib e m => CheckedDecl -> m (Maybe VNNLibDoc)
compileDecl d = case d of
  DeclData{} ->
    normalisationError "Dataset declarations"

  DeclNetw{} -> do
    normalisationError "Network declarations"

  DefFun p ident t e ->
    let identDoc = squotes (pretty ident) in
    if not $ isProperty t then
      -- If it's not a property then we can discard it as all applications
      -- of it should have been normalised out by now.
      return Nothing
    else do
      logDebug $ "Beginning compilation of VNNLib property" <+> identDoc
      incrCallDepth

      let metaNetwork = generateMetaNetwork e
      logDebug $ "Generated meta-network" <+> pretty metaNetwork <> line

      if null metaNetwork then
        throwError $ mkNoNetworkUsedInProperty (p, TheUser) ident
      else do
        metaNetworkDetails <- traverse getNetworkDetailsFromCtx metaNetwork

        -- Replace all applications of neural networks with the magic VNNLib variables
        let numberOfMagicVariables = sum (map networkSize metaNetworkDetails)
        (networklessExpr, quantifiers) <- runNetworkApplicationReplacement numberOfMagicVariables e

        quantifier <-
          if Set.size quantifiers == 1 then
            return $ Set.findMin quantifiers
          else if Set.null quantifiers then
            return All
          else
            throwError $ mkUnsupportedQuantifierSequence (p, TheUser) ident

        -- Normalise the resulting expression now that we've replaced the network
        -- applications with tensors of output variables.
        let normNetworklessExpr = normaliseInternal networklessExpr

        -- Append quantifiers over the magic variables so that it becomes a valid SMTLib expression
        let quantifiedExpr = quantifyOverMagicVariables quantifier metaNetworkDetails normNetworklessExpr
        logDebug $ "Replaced network applications:" <+> prettySimple quantifiedExpr <> line

        -- Compile to SMTLib
        smtDoc <- SMTLib.compileProp ident quantifiedExpr

        decrCallDepth
        logDebug $ "Finished compilation of VNNLib property" <+> identDoc

        return $ Just $ VNNLibDoc smtDoc metaNetwork

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
data UpwardsReplacementState = UpwardsReplacementState
  { replacableBoundVars  :: IntMap Int
  , quantifiers          :: Set Quantifier
  }

type ReplacementState = (DownwardsReplacementState, UpwardsReplacementState)

type MonadReplacement e m = (MonadVNNLib e m, MonadState ReplacementState m)

runNetworkApplicationReplacement :: MonadVNNLib e m => Int -> CheckedExpr -> m (CheckedExpr, Set Quantifier)
runNetworkApplicationReplacement numberOfMagicVariables e = do
  let result = replaceNetworkApplications 0 e
  let initialState =
        ( DownwardsReplacementState 0 0 numberOfMagicVariables
        , UpwardsReplacementState mempty mempty
        )
  (x, (_down, UpwardsReplacementState{..})) <- runStateT result initialState

  return (x, quantifiers)

-- |Called at the site of an application of a neural network.
-- It updates the number of magic variables used and identifies any
-- locally bound variables that can be replaced with magic variables.
processNetworkApplication :: MonadReplacement e m => NetworkDetails -> CheckedExpr -> m (Int, Int)
processNetworkApplication network input = do
  (DownwardsReplacementState{..}, UpwardsReplacementState{..}) <- get

  let magicVarCount = magicInputVarCount + magicOutputVarCount
  let localReplacableBoundVars = case input of
          SeqExpr _ _ _ xs -> getReplacableBoundVars magicVarCount xs
          _                -> developerError $
            "It is assumed that no non-LSeq literals exist after normalisation." <+>
            "However, found the expression" <+> squotes (prettyVerbose input)

  let down = DownwardsReplacementState
        { magicInputVarCount  = magicInputVarCount + size (inputTensor network)
        , magicOutputVarCount = magicInputVarCount + size (inputTensor network)
        , ..
        }

  let up = UpwardsReplacementState
        { replacableBoundVars = replacableBoundVars <> localReplacableBoundVars
        , ..
        }

  put (down, up)
  return (magicInputVarCount, magicOutputVarCount)
    where
      getReplacableBoundVars :: Int -> [CheckedExpr] -> IntMap Int
      getReplacableBoundVars _             []       = mempty
      getReplacableBoundVars magicVarIndex (x : xs) =
        let recRes = getReplacableBoundVars (magicVarIndex + 1) xs in
          case x of
            Var _ (Bound i) -> IntMap.insert i magicVarIndex recRes
            _               -> recRes

processQuantifierBinding :: MonadReplacement e m => Quantifier -> m (Maybe Int)
processQuantifierBinding q = do
  (down, UpwardsReplacementState{..}) <- get

  let (magicVarUsingBinder, newMapping) = IntMap.updateLookupWithKey (\_ _ -> Nothing) 0 replacableBoundVars
  let newQuantifiers = Set.insert q quantifiers
  put (down, UpwardsReplacementState newMapping newQuantifiers)
  return magicVarUsingBinder

traverseUpOverBinder :: MonadReplacement e m => m ()
traverseUpOverBinder = do
  (down, UpwardsReplacementState{..}) <- get
  put (down, UpwardsReplacementState
    { replacableBoundVars = IntMap.mapKeysMonotonic (\x -> x-1) replacableBoundVars
    , ..
    })

getNumberOfMagicVariables :: MonadReplacement e m => m Int
getNumberOfMagicVariables = gets (numberOfMagicVariables . fst)


-- Takes in the expression to process and returns a function
-- from the current binding depth to the altered expression.
replaceNetworkApplications :: MonadReplacement e m
                           => BindingDepth
                           -> CheckedExpr
                           -> m CheckedExpr
replaceNetworkApplications d e =
  case e of
    Hole _p _      -> resolutionError "Hole"
    Meta _p _      -> resolutionError "Meta"
    Ann _ann _ _   -> normalisationError "Ann"
    Lam _ann _ _   -> normalisationError "Non-quantified Lam"

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
      newBody  <- replaceNetworkApplication ann ident (argExpr inputArg) body d
      newBody' <- replaceNetworkApplications d newBody
      return newBody'

    Let ann bound binder body -> do
      bound' <- replaceNetworkApplications d bound
      body'  <- replaceNetworkApplications d body
      traverseUpOverBinder
      return $ Let ann bound' binder body'

    QuantifierExpr q ann binder body -> do
      -- Recurse, increasing the binding depth by 1
      body' <- replaceNetworkApplications (d + 1) body

      magicVariableToSubstitute <- processQuantifierBinding q

      case magicVariableToSubstitute of
        -- Then this bound variable is not equal to one of the magic variables so retain
        -- the quantifier.
        Nothing -> do
          traverseUpOverBinder
          return $ QuantifierExpr q ann binder body'

        -- Then this bound variable is equal to one of the magic variables so this variable
        -- is redundant and we can substitute through.
        Just magicVarIndex -> do
          totalNumberOfMagicVariables <- getNumberOfMagicVariables
          let index = (totalNumberOfMagicVariables - 1 - magicVarIndex) + d
          let magicVar = Var ann (Bound index)
          let result = magicVar `substInto` body'
          traverseUpOverBinder
          return result

    App ann fun args -> do
      fun'  <- replaceNetworkApplications d fun
      args' <- traverse (traverseArgExpr (replaceNetworkApplications d)) args
      return $ App ann fun' args'

{-
    -- EXAMPLE:
    --
    --           (E)              (D)
    -- |-----------------------|
    --          (Net1)   (Net2)  (Net3)  (Net4)
    --
    -- forall (X0 X1 Y0) (X2 Y1) (X3 Y2) (X4 Y3) . forall x,y,z . ... Net2 x ...
    -- |                                       |   |          |      |      |
    -- -----------------------------------------   ------------      |------|
    --                          (A)                     (B)             (C)
    --
    -- (A) Inserted quantifiers over all Magic variables for meta-network.
    --
    -- (B) User quantifiers in the original program.
    --
    -- (C) The location of the network application in the program, which is pointed
    -- to by the variable `currentBindingDepth`.
    --
    -- (D) The location of the current network that's being applied in the quantifiers.
    --
    -- (E) Inserted quantifiers over the meta-network so far.
-}
replaceNetworkApplication :: MonadReplacement e m
                          => CheckedAnn
                          -> Identifier
                          -> CheckedExpr
                          -> CheckedExpr
                          -> BindingDepth
                          -> m CheckedExpr
replaceNetworkApplication ann ident networkInput letBody bindingDepth  = do
  network@(NetworkDetails inputs outputs) <- getNetworkDetailsFromCtx ident
  let inputSize  = size inputs
  let inputType  = tElem outputs
  let outputSize = size inputs
  let outputType = tElem outputs

  totalNumberOfMagicVariables <- getNumberOfMagicVariables
  (inputStartingIndex, outputStartingIndex) <- processNetworkApplication network networkInput
  let totalNumberOfMagicVariablesSoFar = inputStartingIndex + outputStartingIndex

  -- In the example points to X3
  let inputStartingDBIndex  = totalNumberOfMagicVariables + bindingDepth - totalNumberOfMagicVariablesSoFar
  -- In the example points to Y2
  let outputStartingDBIndex = inputStartingDBIndex - inputSize
  -- In the examples points to X4
  let outputEndingDBIndex       = outputStartingDBIndex - outputSize
  let inputVarIndices           = reverse [outputStartingDBIndex .. inputStartingDBIndex-1]
  let outputVarIndices          = reverse [outputEndingDBIndex   .. outputStartingDBIndex-1]
  let (inputsExpr,  inputsType) = mkMagicVariableSeq inputType  inputVarIndices
  let (outputsExpr, _)          = mkMagicVariableSeq outputType outputVarIndices

  let body'         = outputsExpr `substInto` letBody
  let inputEquality = EqualityExpr Eq ann inputsType Prop (map (ExplicitArg ann) [inputsExpr, networkInput])
  let newBody       = BooleanOp2Expr Impl ann Prop (map (ExplicitArg ann) [inputEquality, body'])

  return newBody
  where
    mkMagicVariableSeq :: Builtin -> [Int] -> (CheckedExpr, CheckedExpr)
    mkMagicVariableSeq tElem indices = (tensorExpr, tensorType)
      where
        tensorElemType   = Builtin ann tElem
        tensorType       = mkTensorType ann tensorElemType [length indices]
        variables        = map (Var ann . Bound) indices
        tensorExpr       = SeqExpr ann tensorElemType tensorType variables

--------------------------------------------------------------------------------
-- Step 6: quantification over magic variables

quantifyOverMagicVariables :: Quantifier -> [NetworkDetails] -> CheckedExpr -> CheckedExpr
quantifyOverMagicVariables q metaNetwork prop =
  let totalInputs  = sum (map (size . inputTensor)  metaNetwork) in
  let totalOutputs = sum (map (size . outputTensor) metaNetwork) in
  let (_, _, result) = foldl forNetwork (totalInputs, totalOutputs, prop) metaNetwork in result
  where
    forNetwork :: (Int, Int, CheckedExpr) -> NetworkDetails -> (Int, Int, CheckedExpr)
    forNetwork (inputIndex, outputIndex, body) (NetworkDetails inputs outputs)  =
      let startingInputIndex = inputIndex - size inputs in
      let startingOutputIndex = outputIndex - size outputs in
      let body' = forTensor mempty Input  startingInputIndex  inputs $
                  forTensor mempty Output startingOutputIndex outputs body in
      (startingInputIndex, startingOutputIndex, body')

    forTensor :: CheckedAnn
              -> InputOrOutput
              -> Int
              -> TensorDetails
              -> CheckedExpr
              -> CheckedExpr
    forTensor ann io startingIndex (TensorDetails size tElem) body =
      let indices = reverse [startingIndex .. startingIndex + size-1] in
      let names   = mkMagicVariableNames io indices in
      let varType = Builtin ann tElem in
      mkQuantifierSeq q ann (map Just names) varType body

    mkMagicVariableNames :: InputOrOutput -> [Int] -> [Symbol]
    mkMagicVariableNames io indices = [mkNameWithIndices baseName [i] | i <- indices]
      where baseName = if io == Input then "X" else "Y"