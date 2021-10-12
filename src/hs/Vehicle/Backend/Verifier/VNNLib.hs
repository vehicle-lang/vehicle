{-# LANGUAGE OverloadedLists #-}

module Vehicle.Backend.Verifier.VNNLib where

import Control.Monad.State (MonadState(..), modify, runStateT)
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Except (MonadError(..))
import Data.Map (Map)
import Data.Map qualified as Map (insert)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty(..))

import Vehicle.Prelude hiding (Network)
import Vehicle.Core.AST hiding (Map)
import Vehicle.Core.Print ()
import Vehicle.Backend.Verifier.Core
import Vehicle.Backend.Verifier.SMTLib (compileToSMTLib)

-- | Compiles a given program to a VNNLib script.
-- Assumes the program has already been normalised.
compileToVNNLib :: CheckedProg -> m []
compileToVNNLib prog1 = do
  prog2 <- transformNetworkVars prog1
  compileToSMTLib prog2

data InputOrOutput
  = Input
  | Output

-- | Reasons why we might not support the network type.
-- Options with `Bool` type equate
data UnsupportedNetworkType
  = NotAFunction
  | NotATensor             InputOrOutput
  | MultidimensionalTensor InputOrOutput
  | VariableSizeTensor     InputOrOutput
  | WrongTensorType        InputOrOutput

data VNNLibError
  = UnsupportedNetworkType UnsupportedNetworkType (WithProvenance Identifier) CheckedExpr
  | NoNetworkUsedInProperty (WithProvenance Identifier)
  | MultipleNetworksUsedInProperty [WithProvenance Identifier]

data TensorDetails = TensorDetails
  { size  :: Int
  , tElem :: Builtin
  }

data NetworkDetails = NetworkDetails
  { ident        :: WithProvenance Identifier
  , inputTensor  :: TensorDetails
  , outputTensor :: TensorDetails
  }

type NetworkCtx = Map Identifier NetworkDetails

type MonadVNNLib m =
  ( MonadLogger m
  , MonadReader NetworkCtx m
  )

addNetworkToCtx :: NetworkDetails -> m ()
addNetworkToCtx d@(NetworkDetails ident _ _) =
  modify (Map.insert (deProv ident) d)

type MonadVNNLibProp m =
  ( MonadVNNLib m
  -- The identifier of the network declaration used in the property and
  -- whether or not we've discharged the equalities at the Prop level.
  , MonadState  (Maybe (Identifier, Bool, NonEmpty Arg)) m
  )

compileProg :: MonadVNNLib m => CheckedProg -> m CheckedProg
compileProg (Main ds) = do
  ds' <- traverse compileDecl ds
  return $ Main $ catMaybes ds'

compileDecl :: MonadVNNLib m => CheckedDecl -> m (Maybe CheckedDecl)
compileDecl d = case d of
  DeclData{} ->
    normalisationError "Dataset declarations"

  DeclNetw _ ident t -> do
    (inputDetails, outputDetails) <- getNetworkDetails ident t
    let networkDetails = NetworkDetails ident inputDetails outputDetails
    addNetworkToCtx networkDetails
    return Nothing

  DefFun ann ident t e ->
    if not $ isProperty t then
      return Nothing
    else do
      (updatedDef, maybeNetworkDetails) <- runStateT (alterNetworkApplications e) Nothing
      case maybeNetworkDetails of
        Nothing -> throwError $ NoNetworkUsedInProperty ident
        Just networkDetails -> do
          let quantifiedExpr = quantifyOverNetwork networkDetails updatedDef
          return $ Just $ DefFun ann ident t quantifiedExpr

quantifyOverNetwork :: NetworkDetails -> CheckedExpr -> CheckedExpr
quantifyOverNetwork (NetworkDetails ident inputs outputs) =
  quantifyOverTensor Input inputs . quantifyOverTensor Output outputs
  where
  quantifyOverTensor :: InputOrOutput -> TensorDetails -> CheckedExpr -> CheckedExpr
  quantifyOverTensor io (TensorDetails size tElem) body =
    let ann = prov ident in
    let names = [mkNameWithIndices (User (varBaseName io)) [i] | i <- [0 .. size]] in
    let varType = Builtin ann tElem in
    foldl (\res name -> mkQuantifier ann All name varType res) body names

  varBaseName :: InputOrOutput -> Text
  varBaseName Input  = "X"
  varBaseName Output = "Y"

alterNetworkApplications :: MonadVNNLibProp m => CheckedExpr -> m CheckedExpr
alterNetworkApplications e = case e of
  Type _         -> typeError "Type"
  Pi   _ann _ _  -> typeError "Pi"
  Hole _p _      -> resolutionError "Hole"
  Meta _p _      -> resolutionError "Meta"
  Ann _ann _ _   -> normalisationError "Ann"
  Let _ann _ _ _ -> normalisationError "Let"
  Lam _ann _ _   -> normalisationError "Lam"
  Seq _ann _     -> normalisationError "Seq"
  PrimDict _tc   -> visibilityError "PrimDict"

  Var{}          -> return e
  Builtin{}      -> return e
  Literal{}      -> return e

  App ann (Var ann (Free ident)) args -> alterNetworkApplication ident args
  _ -> _

alterNetworkApplication :: MonadVNNLibProp m
                        => Identifier
                        -> NonEmpty CheckedArg
                        -> m CheckedExpr
alterNetworkApplication ident args = do
  -- Lookup and see if this our first network application
  existingNetworkIdent <- get
  ident <- case existingNetworkIdent of
    Nothing     -> set (ident, False)
    Just (ident, equalitiesCreated, args) -> _


  -- Replace application with vector of output variables
  details <- getNetworkDetails ident -- Nothing -> developerError "Declaration reference found but either out of scope or not normalised."
  let outputSize = _
  let outputVector = [Var _ (Bound _) | i <- [0..outputSize]]
  return outputVector

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
  throwError $ UnsupportedNetworkType NotAFunction ident t

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
  throwError $ UnsupportedNetworkType (NotATensor io) ident t

getTensorType :: MonadVNNLib m
              => InputOrOutput
              -> WithProvenance Identifier
              -> CheckedExpr
              -> m Builtin
getTensorType io ident = \case
  Builtin _ Real  -> return Real
  t               -> throwError $ UnsupportedNetworkType (WrongTensorType io) ident t

getTensorSize :: MonadVNNLib m
              => InputOrOutput
              -> WithProvenance Identifier
              -> CheckedExpr
              -> m Int
getTensorSize io ident tDims = case exprHead tDims of
  (Seq _ [d]) -> case exprHead d of
    (Literal _ (LNat n)) -> return n
    t                    -> throwError $ UnsupportedNetworkType (VariableSizeTensor io) ident t
  t           -> throwError $ UnsupportedNetworkType (MultidimensionalTensor io) ident t