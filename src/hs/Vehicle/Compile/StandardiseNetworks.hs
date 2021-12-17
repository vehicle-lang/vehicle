module Vehicle.Compile.StandardiseNetworks
  ( standardiseNetworks
  , NetworkMap
  , NetworkDetails(..)
  , TensorDetails(..)
  , networkSize
  ) where

import Control.Monad.State (MonadState(..), runStateT, modify, gets)
import Control.Monad.Except (MonadError(..), runExcept)
import Data.Bifunctor (Bifunctor(..), first)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)

import Vehicle.Prelude
import Vehicle.Language.AST
import Vehicle.Language.Print
import Vehicle.Compile.Error
import Vehicle.Compile.AlphaEquivalence (alphaEq)

--------------------------------------------------------------------------------
-- Network standardisation

-- |This module
standardiseNetworks :: (AsNetworkStandardisationError e, MonadLogger m, MonadError e m)
                    => CheckedProg -> m (NetworkMap, CheckedProg)
standardiseNetworks prog1 = do
  logDebug "Beginning standardisation of networks"
  incrCallDepth

  (prog2, internalNetworkMap) <- runStateT (standardise prog1) mempty
  let networkMap = fmap (\(x,_,_) -> x) internalNetworkMap

  logDebug $ prettySimple prog2

  decrCallDepth
  logDebug $ "Finished standardising networks" <> line
  return (networkMap, prog2)

--------------------------------------------------------------------------------
-- Types

type NetworkMap = Map Identifier NetworkDetails
type InternalNetworkMap = Map Identifier (NetworkDetails, TransformInput, TransformOutput)

data NetworkDetails = NetworkDetails
  { inputTensor  :: TensorDetails
  , outputTensor :: TensorDetails
  }

data TensorDetails = TensorDetails
  { size  :: Int
  , tElem :: Builtin
  }

instance Pretty TensorDetails where
  pretty (TensorDetails size tElem) =
    "Tensor" <+> pretty tElem <+> "[" <> pretty size <> "]"

networkSize :: NetworkDetails -> Int
networkSize network = size (inputTensor network) + size (outputTensor network)

--------------------------------------------------------------------------------
-- Main

type MonadNetwork e m =
  ( AsNetworkStandardisationError e
  , MonadLogger m
  , MonadError e m
  , MonadState InternalNetworkMap m
  )

class Standardise t where
  standardise :: MonadNetwork e m
              => t CheckedBinding CheckedVar CheckedAnn
              -> m (t CheckedBinding CheckedVar CheckedAnn)

instance Standardise Prog where
  standardise (Main ds) = Main . catMaybes <$> traverse standariseDecl ds

standariseDecl :: MonadNetwork e m => CheckedDecl -> m (Maybe CheckedDecl)
standariseDecl d = case d of
  DeclData{} -> normalisationError "Dataset declarations"

  DeclNetw ann ident t -> do
    entry <- analyseNetworkType (ann, TheUser) ident t
    -- Insert the network into the context
    modify (Map.insert ident entry)
    -- Remove the declaration.
    return Nothing

  DefFun p ident t e -> Just . DefFun p ident t <$> standardise e

instance Standardise Expr where
  standardise = \case
    Type l                -> return $ Type l
    Meta p i              -> return $ Meta p i
    Hole     ann n        -> return $ Hole ann n
    Builtin  ann op       -> return $ Builtin ann op
    Var      ann v        -> return $ Var ann v
    Literal  ann l        -> return $ Literal ann l

    Ann      ann e t      -> Ann ann <$> standardise e <*> standardise t
    LSeq     ann dict es  -> LSeq ann <$> standardise dict <*> traverse standardise es
    PrimDict ann tc       -> PrimDict ann <$> standardise tc

    Pi  ann binder res        -> Pi ann <$> standardise binder <*> standardise res
    Lam ann binder body       -> Lam ann <$> standardise binder <*> standardise body
    Let ann bound binder body -> Let ann <$> standardise bound <*> standardise binder <*> standardise body

    App ann fun args -> case fun of
      (Var _ (Free ident)) -> do
        result <- gets (Map.lookup ident)
        case result of
          Nothing                      -> App ann <$> standardise fun <*> traverse standardise args
          Just (_, inputFn, outputFn)  -> do
            let inputArgs = inputFn args
            let outputRes = outputFn (App ann fun inputArgs)
            return outputRes
      _                    -> App ann <$> standardise fun <*> traverse standardise args

instance Standardise Binder where
  standardise = traverseBinderType standardise

instance Standardise Arg where
  standardise = traverseArgExpr standardise

--------------------------------------------------------------------------------
-- Analysis of the network type

type TransformInput  = NonEmpty CheckedArg -> NonEmpty CheckedArg
type TransformOutput = CheckedExpr -> CheckedExpr

allowedNetworkElementTypes :: [Builtin]
allowedNetworkElementTypes =
  [ BooleanType Bool
  , NumericType Nat
  , NumericType Int
  , NumericType Rat
  , NumericType Real
  ]

analyseNetworkType :: MonadNetwork e m
                   => CheckedAnn
                   -> Identifier
                   -> CheckedExpr
                   -> m (NetworkDetails, TransformInput, TransformOutput)
analyseNetworkType ann ident t = either
  (throwError . mkUnsupportedNetworkType ann ident t allowedNetworkElementTypes)
  return $ runExcept $ do
    (inputs, output)                 <- decomposeNetworkType Nothing t
    (inputDetails,  transformInput)  <- analyseNetworkInputTypes inputs
    (outputDetails, transformOutput) <- analyseNetworkOutputType output
    let networkDetails = NetworkDetails inputDetails outputDetails
    return (networkDetails, transformInput, transformOutput)

-- |Decomposes the Pis in a network type signature, checking that the
-- binders are explicit and their types are equal.
decomposeNetworkType :: MonadError UnsupportedNetworkType m
                     => Maybe CheckedExpr -> CheckedExpr -> m ([CheckedExpr], CheckedExpr)
decomposeNetworkType prevType networkType = case networkType of
  Pi _ binder result
    | visibilityOf binder /= Explicit -> throwError $ NonExplicitArguments binder
    | otherwise                       -> do
      t <- checkTypesEqual prevType (typeOf binder)
      first (t :) <$> decomposeNetworkType (Just t) result
  _ -> return ([], networkType)
  where
    checkTypesEqual :: MonadError UnsupportedNetworkType m
                    => Maybe CheckedExpr -> CheckedExpr -> m CheckedExpr
    checkTypesEqual Nothing      binderType = return binderType
    checkTypesEqual (Just pType) binderType =
      if alphaEq binderType pType
        then return binderType
        else throwError $ NonEqualArguments pType binderType

analyseNetworkInputTypes :: MonadError UnsupportedNetworkType m
                         => [CheckedExpr] -> m (TensorDetails, TransformInput)
analyseNetworkInputTypes []                         = throwError NotAFunction
analyseNetworkInputTypes [TensorType _ tElem tDims] = do
  tensorDetails <- getTensorDetails Input tElem tDims
  return (tensorDetails, id)
analyseNetworkInputTypes (x : _) = do
  builtinType <- getTensorType Input x
  let tensorDetails = TensorDetails 1 builtinType
  return (tensorDetails, transformInputNumericToTensor x)

analyseNetworkOutputType :: MonadError UnsupportedNetworkType m
                         => CheckedExpr -> m (TensorDetails, TransformOutput)
analyseNetworkOutputType (TensorType _ tElem tDims) = do
  tensorDetails <- getTensorDetails Output tElem tDims
  return (tensorDetails, id)
analyseNetworkOutputType t = do
  builtinType <- getTensorType Output t
  let tensorDetails = TensorDetails 1 builtinType
  let transformOutput = transformOutputNumericToTensor t
  return (tensorDetails, transformOutput)

transformInputNumericToTensor :: CheckedExpr -> NonEmpty CheckedArg -> NonEmpty CheckedArg
transformInputNumericToTensor tElem args =
  let ann = annotationOf tElem in
  let tTensor = mkTensorType ann tElem [NonEmpty.length args] in
  let tensorExpr = SeqExpr ann tElem tTensor (fmap argExpr (NonEmpty.toList args)) in
  (ExplicitArg ann tensorExpr :| [])

transformOutputNumericToTensor :: CheckedExpr -> CheckedExpr -> CheckedExpr
transformOutputNumericToTensor tElem e =
  let ann = annotationOf e in
  let tDims = mkTensorDims ann [1] in
  let atArgs = fmap (ExplicitArg ann) [e, NatLiteralExpr ann Nat 0] in
  AtExpr ann tElem tDims atArgs

getTensorDetails :: MonadError UnsupportedNetworkType m
                 => InputOrOutput
                 -> CheckedExpr
                 -> CheckedExpr
                 -> m TensorDetails
getTensorDetails io tElem tDims = do
  typ   <- getTensorType io tElem
  size  <- getTensorSize io tDims
  return $ TensorDetails size typ

getTensorType :: MonadError UnsupportedNetworkType m => InputOrOutput -> CheckedExpr -> m Builtin
getTensorType _  (Builtin _ t)
  | t `elem` allowedNetworkElementTypes = return t
getTensorType io _ = throwError $ WrongTensorType io

getTensorSize :: MonadError UnsupportedNetworkType m
              => InputOrOutput
              -> CheckedExpr
              -> m Int
getTensorSize io tDims = case exprHead tDims of
  (LSeq _ _ [d]) -> case exprHead d of
    (Literal _ (LNat n)) -> return n
    _                    -> throwError $ VariableSizeTensor io
  _           -> throwError $ MultidimensionalTensor io