{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Vehicle.Backend.Verifier.VNNLib
  ( VNNLibDoc(..)
  , NetworkDetails(..)
  , compileToVNNLib
  ) where

import Control.Monad.Reader (MonadReader(..), runReaderT, asks)
import Control.Monad.Except (MonadError(..))
import Control.Monad.State (MonadState(..), runStateT, modify, gets)
import Data.Map (Map)
import Data.Map qualified as Map (insert, lookup)
import Data.Maybe (catMaybes, fromMaybe)

import Vehicle.Prelude hiding (Network)
import Vehicle.Core.AST hiding (Map)
import Vehicle.Core.Print (prettyVerbose, prettySimple)
import Vehicle.Core.Print.Friendly (prettyFriendly)
import Vehicle.Core.Normalise (normaliseInternal)
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
-- to a single input per property. We hack around this restriction by combining
--  multiple networks, or multiple applications of the same network into a
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

data TensorDetails = TensorDetails
  { size  :: Int
  , tElem :: Builtin
  }

instance Pretty TensorDetails where
  pretty (TensorDetails size tElem) = "Tensor" <+> pretty tElem <+> "[" <> pretty size <> "]"

data NetworkDetails = NetworkDetails
  { ident        :: WithProvenance Identifier
  , inputTensor  :: TensorDetails
  , outputTensor :: TensorDetails
  }

instance Pretty NetworkDetails where
  pretty (NetworkDetails ident input output) =
    pretty ident <+> ":" <+> pretty input <+> "->" <+> pretty output

networkSize :: NetworkDetails -> Int
networkSize n = size (inputTensor n) + size (outputTensor n)

type NetworkCtx = Map Identifier NetworkDetails

type MetaNetwork = [NetworkDetails]

metaNetworkSize :: MetaNetwork -> Int
metaNetworkSize = sum . map networkSize

--------------------------------------------------------------------------------
-- Monad

type MonadVNNLib m =
  ( MonadLogger m
  , MonadError SMTLibError m
  , MonadReader NetworkCtx m
  )

getNetworkFromCtx :: MonadVNNLib m => Identifier -> m NetworkDetails
getNetworkFromCtx ident = asks (fromMaybe outOfScopeError . Map.lookup ident)
  where
    outOfScopeError :: a
    outOfScopeError = developerError $
      "Network" <+> squotes (pretty ident) <+> "erroneously not in scope"

type MonadVNNLibProp m =
  ( MonadVNNLib m
  , MonadState MetaNetwork m
  )

addToMetaNetwork :: MonadVNNLibProp m => Identifier -> m (NetworkDetails, MetaNetwork)
addToMetaNetwork ident = do
  network <- getNetworkFromCtx ident
  metaNetwork <- get
  modify (network :)
  return (network, metaNetwork)

--------------------------------------------------------------------------------
-- Algorithm

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

  DeclNetw _ ident t -> do
    -- Extract the details for the network
    (inputDetails, outputDetails) <- getNetworkDetails ident t
    let networkDetails = NetworkDetails ident inputDetails outputDetails
    let alterCtx = Map.insert (deProv ident) networkDetails
    -- Remove the declaration, as SMTLib does not support it.
    return (Nothing, alterCtx)

  DefFun _ ident t e ->
    let alterCtx = id in
    if not $ isProperty t then
      -- If it's not a property then we can discard it as all applications
      -- of it should have been normalised out by now.
      return (Nothing, alterCtx)
    else do
      logDebug $ "Beginning compilation of property" <+> squotes (pretty (deProv ident))
      incrCallDepth
      -- TODO Lift all network-bindings to quantifier level
      (dslExpr, metaNetwork) <- runStateT (processNetworkApplications e) []
      logDebug $ "Generated meta-network" <+> pretty metaNetwork
      if null metaNetwork then
        throwError $ NoNetworkUsedInProperty ident
      else do
        let size = metaNetworkSize metaNetwork
        let networklessExpr = dslExpr size
        let normMetworklessExpr = normaliseInternal networklessExpr
        let quantifiedExpr = quantifyOverMetaNetworkIO metaNetwork normMetworklessExpr

        logDebug $ "Finished conversion to SMTLib:" <+> prettyFriendly mempty quantifiedExpr

        smtDoc <- SMTLib.compileProp (deProv ident) quantifiedExpr
        decrCallDepth

        return (Just $ VNNLibDoc smtDoc metaNetwork, alterCtx)


quantifyOverMetaNetworkIO :: MetaNetwork -> CheckedExpr -> CheckedExpr
quantifyOverMetaNetworkIO metaNetwork prop = result
  where
    (_, _, result) = foldr quantifyOverNetworkIO (0, 0, prop) metaNetwork

    quantifyOverNetworkIO :: NetworkDetails -> (Int, Int, CheckedExpr) -> (Int, Int, CheckedExpr)
    quantifyOverNetworkIO (NetworkDetails ident inputs outputs) (inputIndex, outputIndex, body)  =
      let body' = quantifyOverTensorIO ident Input  inputIndex  inputs $
                  quantifyOverTensorIO ident Output outputIndex outputs body in
      (inputIndex + size inputs, outputIndex + size outputs, body')

    quantifyOverTensorIO :: WithProvenance Identifier
                         -> InputOrOutput
                         -> Int
                         -> TensorDetails
                         -> CheckedExpr
                         -> CheckedExpr
    quantifyOverTensorIO ident io startingIndex (TensorDetails size tElem) body =
      let ann = prov ident in
      let indices = reverse [startingIndex .. startingIndex + size-1] in
      let names = mkMagicVariableNames io indices in
      let varType = Builtin ann tElem in
      foldl (\res name -> mkQuantifier ann All name varType res) body names

mkMagicVariableNames :: InputOrOutput -> [Int] -> [Name]
mkMagicVariableNames io indices = [mkNameWithIndices (User baseName) [i] | i <- indices]
  where
    baseName = if io == Input then "X" else "Y"

-- Takes in the expression to process and returns a function
-- from the current binding depth to the altered expression.
--
-- NOTE that we don't need to adjust references to already bound variables
-- as the quantifiers are all added on the outside.
processNetworkApplications :: MonadVNNLibProp m
                           => CheckedExpr
                           -> m (BindingDepth -> CheckedExpr)
processNetworkApplications e = case e of
  Type _         -> typeError "Type"
  Pi   _ann _ _  -> typeError "Pi"
  Hole _p _      -> resolutionError "Hole"
  Meta _p _      -> resolutionError "Meta"
  PrimDict _tc   -> visibilityError "PrimDict"
  Ann _ann _ _   -> normalisationError "Ann"

  Builtin{} -> return $ const e
  Literal{} -> return $ const e
  Var{}     -> return $ const e

  Seq ann xs     -> do
    xs' <- traverse processNetworkApplications xs
    return (\d -> Seq ann (map ($ d) xs'))

  Lam ann binder body -> do
    body' <- processNetworkApplications body
    -- Increase the binding depth by 1
    return (\d -> Lam ann binder (body' (d + 1)))

  App ann fun args -> do
    fun'  <- processNetworkApplications fun
    args' <- traverse recurseArg args
    return (\d -> App ann (fun' d) (fmap ($ d) args'))

  Let _ (App _ fun [Arg _ _ arg]) _ body -> case (fun, exprHead arg) of
    (Var ann (Free ident), currentInput) -> do
      (networkDetails, currentMetaNetwork) <- addToMetaNetwork ident
      body' <- processNetworkApplications body
      return $ processNetworkApplication currentMetaNetwork networkDetails ann currentInput body'

    _ -> normalisationError "Let"

  Let{} -> normalisationError "Let"
  where
    recurseArg :: MonadVNNLibProp m => CheckedArg -> m (BindingDepth -> CheckedArg)
    recurseArg (Arg ann Explicit expr) = do
      expr' <- processNetworkApplications expr
      return (\d-> Arg ann Explicit (expr' d))
    recurseArg arg = return $ const arg

processNetworkApplication :: MetaNetwork    -- Current metanetwork
                          -> NetworkDetails -- Current network
                          -> CheckedAnn
                          -> CheckedExpr
                          -> (BindingDepth -> CheckedExpr)
                          -> BindingDepth
                          -> CheckedExpr
processNetworkApplication currentMetaNetwork network ann currentInput body
  currentBindingDepth = newBody
  where
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
    -- (A) Inserted quantifiers over all IO variables for meta-network.
    --
    -- (B) User quantifiers in the original program.
    --
    -- (C) The location of the network application in the program, which is pointed
    -- to by the variable `currentBindingDepth`.
    --
    -- (D) The location of the current network that's being applied in the quantifiers.
    --
    -- (E) Inserted quantifiers over the meta-network so far.

    -- Number of quantified variables in region (E)
    totalNumberOfIOVariablesSoFar = metaNetworkSize currentMetaNetwork

    inputT = inputTensor network
    inputSize = size inputT
    inputType = tElem inputT

    outputT = outputTensor network
    outputSize = size outputT
    outputType = tElem outputT

    -- In the example points to X3
    inputStartingDBIndex  = currentBindingDepth - totalNumberOfIOVariablesSoFar
    -- In the example points to Y2
    outputStartingDBIndex = inputStartingDBIndex - inputSize
    -- In the examples points to X4
    outputEndingDBIndex   = outputStartingDBIndex - outputSize

    inputVarIndices  = reverse [outputStartingDBIndex .. inputStartingDBIndex-1]
    outputVarIndices = reverse [outputEndingDBIndex   .. outputStartingDBIndex-1]

    (inputTensorExpr,  inputTensorType) = mkIOVariableSeq ann inputType  inputVarIndices
    (outputTensorExpr, _)               = mkIOVariableSeq ann outputType outputVarIndices
    body'                               = outputTensorExpr `substInto` body currentBindingDepth
    inputEquality                       = mkEq ann inputTensorType (Builtin ann Prop) inputTensorExpr currentInput
    newBody                             = mkAnd ann (Builtin ann Prop) body' inputEquality

mkIOVariableSeq :: CheckedAnn -> Builtin -> [Int] -> (CheckedExpr, CheckedExpr)
mkIOVariableSeq ann tElem indices = (tensorExpr, tensorType)
  where
    tensorElemType   = Builtin ann tElem
    tensorType       = mkTensorType ann tensorElemType [length indices]
    variables        = map (Var ann . Bound) indices
    tensorExpr       = mkSeq ann tensorElemType tensorType variables


--------------------------------------------------------------------------------
-- Network type validation

getNetworkDetails :: MonadVNNLib m
                  => WithProvenance Identifier
                  -> CheckedExpr
                  -> m (TensorDetails, TensorDetails)
getNetworkDetails ident (Pi _ (Binder _ _ _ input) output) = do
  inputDetails  <- getTensorDetails Input  ident input
  outputDetails <- getTensorDetails Output ident output
  return (inputDetails, outputDetails)
getNetworkDetails ident t                                  =
  throwError $ UnsupportedNetworkType (prov ident) (deProv ident) NotAFunction t

getTensorDetails :: MonadVNNLib m
                 => InputOrOutput
                 -> WithProvenance Identifier
                 -> CheckedExpr
                 -> m TensorDetails
getTensorDetails io ident (App _ (Builtin _ Tensor) [Arg _ _ tElem, Arg _ _ tDims]) = do
  typ   <- getTensorType io ident tElem
  size  <- getTensorSize io ident tDims
  return $ TensorDetails size typ
getTensorDetails io ident t =
  throwError $ UnsupportedNetworkType (prov ident) (deProv ident) (NotATensor io) t

getTensorType :: MonadVNNLib m
              => InputOrOutput
              -> WithProvenance Identifier
              -> CheckedExpr
              -> m Builtin
getTensorType io ident = \case
  Builtin _ Real  -> return Real
  t               -> throwError $ UnsupportedNetworkType (prov ident) (deProv ident) (WrongTensorType io) t

getTensorSize :: MonadVNNLib m
              => InputOrOutput
              -> WithProvenance Identifier
              -> CheckedExpr
              -> m Int
getTensorSize io ident tDims = case exprHead tDims of
  (Seq _ [d]) -> case exprHead d of
    (Literal _ (LNat n)) -> return n
    t                    -> throwError $ UnsupportedNetworkType (prov ident) (deProv ident) (VariableSizeTensor io) t
  t           -> throwError $ UnsupportedNetworkType (prov ident) (deProv ident) (MultidimensionalTensor io) t