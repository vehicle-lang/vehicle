module Vehicle.Backend.Verifier.VNNLib
  ( VNNLibDoc(..)
  , compileToVNNLib
  ) where

import Control.Monad.Reader (MonadReader(..), runReaderT, asks)
import Control.Monad.Except (MonadError(..), runExcept)
import Control.Monad.State (MonadState(..), runStateT, gets)
import Data.Map (Map)
import Data.Map qualified as Map (insert, lookup)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Maybe (catMaybes, fromMaybe)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Debug.Trace

import Vehicle.Prelude
import Vehicle.Language.AST hiding (Map)
import Vehicle.Language.Print (prettySimple, prettyVerbose)
import Vehicle.Language.Normalise (normaliseInternal)
import Vehicle.Backend.Verifier.Core
import Vehicle.Backend.Verifier.SMTLib (SMTLibError, SMTDoc, SMTLibError(..), InputOrOutput(..), UnsupportedNetworkType(..))
import Vehicle.Backend.Verifier.SMTLib qualified as SMTLib (compileProp)

--------------------------------------------------------------------------------
-- Compilation to VNNLib
--
-- Okay so this is a wild ride. The VNNLib format has special variable names for
-- input and output variables, namely X1 ... XN and Y1 ... YM but otherwise has
-- the standard SMTLib syntax.
--
-- This means that in theory you can only reason about a single network applied
-- to a single input per property. We get around this restriction by combining
-- multiple networks, or multiple applications of the same network into a
-- single "meta" network. Concretely this process goes as follows for each
-- property we identify in the program.
--
-- 1. We perform let-lifting of network applications so that every application
-- of a network to a unique input sits in its own let binding underneath a
-- universal quantifier. (STILL TO DO)
--
-- 2. We traverse the resulting expression finding all let-bound
-- applications of the network e.g.
--
--   let y = f xs in e
--
-- and perform the following substitution for the application:
--
--   (X4 == a and X5 == b && X6 == c) and ([Y2, Y3] `substInto` e)
--
-- and add each application to the meta-network.
--
-- 3. For every input and output of the meta-network we insert
-- universal quantifiers over the inputs X1 ... XN and outputs
-- Y1 ... YN to the network at the top-level of the property.
--
-- 5. The property should then be a valid SMTLib expression so we now
-- compile it to SMTLib as normal.
--
-- 6. We return the meta-network composition so that we can actually
-- perform the required hackery on the network files elsewhere.

-- | Compiles a given program to a VNNLib script.
-- Assumes the program has already been normalised.
compileToVNNLib :: (MonadLogger m, MonadError SMTLibError m)
                => CheckedProg
                -> m [VNNLibDoc]
compileToVNNLib prog = do
  logDebug "Beginning compilation to VNNLib"
  incrCallDepth
  result <- runReaderT (compileProg prog) []
  decrCallDepth
  logDebug "Finished compilation to VNNLib"
  return result

--------------------------------------------------------------------------------
-- Data

data VNNLibDoc = VNNLibDoc
  { smtDoc      :: SMTDoc
  , metaNetwork :: MetaNetwork
  }

instance Pretty TensorDetails where
  pretty (TensorDetails size tElem) = "Tensor" <+> pretty tElem <+> "[" <> pretty size <> "]"

type NetworkCtx = Map Identifier CheckedDecl

type MetaNetwork = [Identifier]

--------------------------------------------------------------------------------
-- Monad

type MonadVNNLib m =
  ( MonadLogger m
  , MonadError SMTLibError m
  , MonadReader NetworkCtx m
  )

getNetworkDetailsFromCtx :: MonadVNNLib m => Identifier -> m NetworkDetails
getNetworkDetailsFromCtx ident = do
  networkDecl <- asks (fromMaybe outOfScopeError . Map.lookup ident)
  getNetworkDetails (provenanceOf networkDecl, TheUser) (identifierOf networkDecl) (typeOf networkDecl)
  where
    outOfScopeError :: a
    outOfScopeError = developerError $
      "Either" <+> squotes (pretty ident) <+> "is not a network or it is not in scope"

--------------------------------------------------------------------------------
-- Algorithm
--------------------------------------------------------------------------------

compileProg :: MonadVNNLib m => CheckedProg -> m [VNNLibDoc]
compileProg (Main ds) = do
  results <- catMaybes <$> compileDecls ds
  if null results then
    throwError NoPropertiesFound
  else
    return results

compileDecls :: MonadVNNLib m => [CheckedDecl] -> m [Maybe VNNLibDoc]
compileDecls []       = return []
compileDecls (d : ds) = do
    (doc, alterCtx) <- compileDecl d
    docs <- local alterCtx (compileDecls ds)
    return (doc : docs)

compileDecl :: MonadVNNLib m => CheckedDecl -> m (Maybe VNNLibDoc, NetworkCtx -> NetworkCtx)
compileDecl d = case d of
  DeclData{} ->
    normalisationError "Dataset declarations"

  DeclNetw _ ident _ -> do
    -- Insert the network into the context
    let alterCtx = Map.insert ident d
    -- Remove the declaration, as SMTLib does not support it.
    return (Nothing, alterCtx)

  DefFun p ident t e ->
    let alterCtx = id in
    let identDoc = squotes (pretty ident) in
    if not $ isProperty t then
      -- If it's not a property then we can discard it as all applications
      -- of it should have been normalised out by now.
      return (Nothing, alterCtx)
    else do
      logDebug $ "Beginning compilation of VNNLib property" <+> identDoc
      incrCallDepth

      let metaNetwork = freeNames e
      logDebug $ "Generated meta-network" <+> pretty metaNetwork <> line

      if null metaNetwork then
        throwError $ NoNetworkUsedInProperty (p, TheUser) ident
      else do
        metaNetworkDetails <- traverse getNetworkDetailsFromCtx metaNetwork

        -- Replace all applications of neural networks with the magic VNNLib variables
        let numberOfMagicVariables = sum (map networkSize metaNetworkDetails)
        (networklessExpr, _) <- runNetworkApplicationReplacement numberOfMagicVariables e

        -- Normalise the resulting expression now that we've replaced the network
        -- applications with tensors of output variables.
        let normNetworklessExpr = normaliseInternal networklessExpr

        -- Append quantifiers over the magic variables so that it becomes a valid SMTLib expression
        let quantifiedExpr = quantifyOverMagicVariables metaNetworkDetails normNetworklessExpr
        logDebug $ "Replaced network applications:" <+> prettySimple quantifiedExpr <> line

        -- Compile to SMTLib
        smtDoc <- SMTLib.compileProp ident quantifiedExpr

        decrCallDepth
        logDebug $ "Finished compilation of VNNLib property" <+> identDoc

        return (Just $ VNNLibDoc smtDoc metaNetwork, alterCtx)

--------------------------------------------------------------------------------
-- Pass 1: instantiating network applications

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

type MonadReplacement m = (MonadVNNLib m, MonadState ReplacementState m)

runNetworkApplicationReplacement :: MonadVNNLib m => Int -> CheckedExpr -> m (CheckedExpr, UpwardsReplacementState)
runNetworkApplicationReplacement numberOfMagicVariables e = do
  let result = replaceNetworkApplications 0 e
  let initialState =
        ( DownwardsReplacementState 0 0 numberOfMagicVariables
        , UpwardsReplacementState mempty mempty
        )
  (x, (_down, up)) <- runStateT result initialState
  return (x, up)

-- |Called at the site of an application of a neural network.
-- It updates the number of magic variables used and identifies any
-- locally bound variables that can be replaced with magic variables.
processNetworkApplication :: MonadReplacement m => NetworkDetails -> CheckedExpr -> m (Int, Int)
processNetworkApplication network input = do
  (DownwardsReplacementState{..}, UpwardsReplacementState{..}) <- get

  let magicVarCount = magicInputVarCount + magicOutputVarCount
  let localReplacableBoundVars = case input of
          SeqExpr _ _ _ xs -> getReplacableBoundVars magicVarCount xs
          _                -> developerError $
            "It is assumed that no non-Seq literals exist after normalisation." <+>
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

processQuantifierBinding :: MonadReplacement m => Quantifier -> m (Maybe Int)
processQuantifierBinding q = do
  (down, UpwardsReplacementState{..}) <- get
  logDebug $ "qb" <+> pretty (show replacableBoundVars)

  let (magicVarUsingBinder, newMapping) = IntMap.updateLookupWithKey (\_ _ -> Nothing) 0 replacableBoundVars
  let newQuantifiers = Set.insert q quantifiers
  put (down, UpwardsReplacementState newMapping newQuantifiers)
  return magicVarUsingBinder

traverseUpOverBinder :: MonadReplacement m => m ()
traverseUpOverBinder = do
  (down, UpwardsReplacementState{..}) <- get
  logDebug "tb"
  put (down, UpwardsReplacementState
    { replacableBoundVars = IntMap.mapKeysMonotonic (\x -> x-1) replacableBoundVars
    , ..
    })

getNumberOfMagicVariables :: MonadReplacement m => m Int
getNumberOfMagicVariables = gets (numberOfMagicVariables . fst)


-- Takes in the expression to process and returns a function
-- from the current binding depth to the altered expression.
replaceNetworkApplications :: MonadReplacement m
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

    Seq ann xs -> Seq ann <$> traverse (replaceNetworkApplications d) xs

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
          logDebug "Replacement"
          logDebug $ pretty totalNumberOfMagicVariables
          logDebug $ pretty magicVarIndex
          logDebug $ pretty d
          logDebug $ pretty index
          logDebug $ prettySimple body'
          logDebug $ prettySimple result
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
replaceNetworkApplication :: MonadReplacement m
                          => CheckedAnn
                          -> Identifier
                          -> CheckedExpr
                          -> CheckedExpr
                          -> BindingDepth
                          -> m CheckedExpr
replaceNetworkApplication ann ident networkInput letBody bindingDepth  = do
  network@(NetworkDetails _ _ inputs outputs) <- getNetworkDetailsFromCtx ident
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

  let prop          = BuiltinBooleanType ann Prop
  let body'         = outputsExpr `substInto` letBody
  let inputEquality = EqualityExpr Eq ann inputsType prop (map (ExplicitArg ann) [inputsExpr, networkInput])
  let newBody       = BooleanOp2Expr Impl ann prop (map (ExplicitArg ann) [inputEquality, body'])

  return newBody
  where
    mkMagicVariableSeq :: Builtin -> [Int] -> (CheckedExpr, CheckedExpr)
    mkMagicVariableSeq tElem indices = (tensorExpr, tensorType)
      where
        tensorElemType   = Builtin ann tElem
        tensorType       = mkTensor ann tensorElemType [length indices]
        variables        = map (Var ann . Bound) indices
        tensorExpr       = SeqExpr ann tensorElemType tensorType variables

--------------------------------------------------------------------------------
-- Pass 2: quantification over magic variables

quantifyOverMagicVariables :: [NetworkDetails] -> CheckedExpr -> CheckedExpr
quantifyOverMagicVariables metaNetwork prop =
  let totalInputs  = sum (map (size . inputTensor)  metaNetwork) in
  let totalOutputs = sum (map (size . outputTensor) metaNetwork) in
  let (_, _, result) = foldl forNetwork (totalInputs, totalOutputs, prop) metaNetwork in result
  where
    forNetwork :: (Int, Int, CheckedExpr) -> NetworkDetails -> (Int, Int, CheckedExpr)
    forNetwork (inputIndex, outputIndex, body) (NetworkDetails p _ inputs outputs)  =
      let startingInputIndex = inputIndex - size inputs in
      let startingOutputIndex = outputIndex - size outputs in
      let body' = forTensor p Input  startingInputIndex  inputs $
                  forTensor p Output startingOutputIndex outputs body in
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
      mkQuantifierSeq All ann (map Just names) varType body

    mkMagicVariableNames :: InputOrOutput -> [Int] -> [Symbol]
    mkMagicVariableNames io indices = [mkNameWithIndices baseName [i] | i <- indices]
      where baseName = if io == Input then "X" else "Y"

--------------------------------------------------------------------------------
-- Network type validation

data NetworkDetails = NetworkDetails
  { _annotation   :: CheckedAnn
  , _ident        :: Identifier
  , inputTensor  :: TensorDetails
  , outputTensor :: TensorDetails
  }

networkSize :: NetworkDetails -> Int
networkSize network = size (inputTensor network) + size (outputTensor network)

data TensorDetails = TensorDetails
  { size  :: Int
  , tElem :: Builtin
  }

getNetworkDetails :: MonadVNNLib m
                  => CheckedAnn
                  -> Identifier
                  -> CheckedExpr
                  -> m NetworkDetails
getNetworkDetails ann ident t@(Pi _ inputBinder output) =
  either
    (throwError . UnsupportedNetworkType ann ident t)
    return
    $ runExcept $ do
      inputDetails  <- getTensorDetails Input  (typeOf inputBinder)
      outputDetails <- getTensorDetails Output output
      return $ NetworkDetails ann ident inputDetails outputDetails
getNetworkDetails ann ident t                                  =
  throwError $ UnsupportedNetworkType ann ident t NotAFunction

getTensorDetails :: MonadError UnsupportedNetworkType m
                 => InputOrOutput
                 -> CheckedExpr
                 -> m TensorDetails
getTensorDetails io (App _ (BuiltinContainerType _ Tensor) [tElemArg, tDimsArg]) = do
  typ   <- getTensorType io (argExpr tElemArg)
  size  <- getTensorSize io (argExpr tDimsArg)
  return $ TensorDetails size typ
getTensorDetails io _ = throwError $ NotATensor io

getTensorType :: MonadError UnsupportedNetworkType m
              => InputOrOutput
              -> CheckedExpr
              -> m Builtin
getTensorType _  (BuiltinNumericType _ Real) = return (NumericType Real)
getTensorType io _                           = throwError $ WrongTensorType io

getTensorSize :: MonadError UnsupportedNetworkType m
              => InputOrOutput
              -> CheckedExpr
              -> m Int
getTensorSize io tDims = case exprHead tDims of
  (Seq _ [d]) -> case exprHead d of
    (Literal _ (LNat n)) -> return n
    _                    -> throwError $ VariableSizeTensor io
  _           -> throwError $ MultidimensionalTensor io