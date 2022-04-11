module Vehicle.Compile.Resource.Network
  ( removeNetworkDecls
  ) where

import Control.Monad.State (MonadState(..), runStateT, modify, gets)
import Control.Monad.Except (MonadError(..))
import Control.Monad.Reader (MonadReader(..), ReaderT (runReaderT))
import Data.Bifunctor (Bifunctor(..), first)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)

import Vehicle.Language.Print
import Vehicle.Compile.Prelude
import Vehicle.Compile.AlphaEquivalence (alphaEq)
import Vehicle.Compile.Error
import Vehicle.Resource.NeuralNetwork

--------------------------------------------------------------------------------
-- Network standardisation

-- | This function removes all network declarations from the program and extracts
-- their types into the NetworkCtx. It also normalises all network types into
-- the standard form `Tensor A [m] -> Tensor B [n]`.
removeNetworkDecls :: MonadCompile m
                   => CheckedProg
                   -> m (NetworkCtx, CheckedProg)
removeNetworkDecls prog1 = do
  logDebug "Beginning normalisation of network types"
  incrCallDepth

  (prog2, internalNetworkMap) <- runStateT (standardise prog1) mempty
  let networkCtx = fmap (\(x,_,_) -> x) internalNetworkMap

  logDebug $ prettySimple prog2
  logDebug $ prettyMap networkCtx

  decrCallDepth
  logDebug $ "Finished normalisation of network types" <> line
  return (networkCtx, prog2)

--------------------------------------------------------------------------------
-- Types

type InternalNetworkMap = Map Symbol (NetworkDetails, TransformInput, TransformOutput)

--------------------------------------------------------------------------------
-- Main

type MonadNetwork m =
  ( MonadCompile m
  , MonadState InternalNetworkMap m
  )

class Standardise t where
  standardise :: MonadNetwork m
              => t CheckedBinding CheckedVar CheckedAnn
              -> m (t CheckedBinding CheckedVar CheckedAnn)

instance Standardise Prog where
  standardise (Main ds) = Main . catMaybes <$> traverse standariseDecl ds

standariseDecl :: MonadNetwork m => CheckedDecl -> m (Maybe CheckedDecl)
standariseDecl d = case d of
  DefResource ann Network ident t -> do
    entry <- analyseNetworkType (ann, TheUser) ident t
    -- Insert the network into the context
    modify (Map.insert (nameOf ident) entry)
    -- Remove the declaration.
    return Nothing

  DefResource p r ident t ->
    Just . DefResource p r ident <$> standardise t

  DefFunction p ident t e ->
    Just . DefFunction p ident t <$> standardise e

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
        result <- gets (Map.lookup (nameOf ident))
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

type MonadNetworkTypeAnalysis m =
  ( MonadCompile m
  , MonadState InternalNetworkMap m
  , MonadReader (Identifier, CheckedExpr) m
  )

analyseNetworkType :: MonadNetwork m
                   => CheckedAnn
                   -> Identifier
                   -> CheckedExpr
                   -> m (NetworkDetails, TransformInput, TransformOutput)
analyseNetworkType _ ident t = flip runReaderT (ident, t) $ do
  (inputs, output)                 <- decomposeNetworkType Nothing t
  (inputDetails,  transformInput)  <- analyseNetworkInputTypes inputs
  (outputDetails, transformOutput) <- analyseNetworkOutputType output
  let networkDetails = NetworkDetails inputDetails outputDetails
  return (networkDetails, transformInput, transformOutput)

-- |Decomposes the Pi types in a network type signature, checking that the
-- binders are explicit and their types are equal.
decomposeNetworkType :: MonadNetworkTypeAnalysis m
                     => Maybe CheckedExpr
                     -> CheckedExpr
                     -> m ([CheckedExpr], CheckedExpr)
decomposeNetworkType prevType networkType = case networkType of
  Pi _ binder result
    | visibilityOf binder /= Explicit -> do
      (ident, _) <- ask
      throwError $ NetworkTypeHasNonExplicitArguments ident networkType binder
    | otherwise  -> do
      t <- checkTypesEqual prevType (typeOf binder)
      first (t :) <$> decomposeNetworkType (Just t) result
  _ -> return ([], networkType)
  where
    checkTypesEqual :: MonadNetworkTypeAnalysis m
                    => Maybe CheckedExpr
                    -> CheckedExpr
                    -> m CheckedExpr
    checkTypesEqual Nothing  binderType = return binderType
    checkTypesEqual (Just t) binderType =
      if alphaEq binderType t
        then return binderType
        else do
        (ident, _) <- ask
        throwError $ NetworkTypeHasHeterogeneousInputTypes ident networkType t binderType

analyseNetworkInputTypes :: MonadNetworkTypeAnalysis m
                         => [CheckedExpr]
                         -> m (TensorDetails, TransformInput)
analyseNetworkInputTypes [] = do
  (ident, networkType) <- ask
  throwError $ NetworkTypeIsNotAFunction ident networkType
analyseNetworkInputTypes [tTensor@(TensorType _ tElem tDims)] = do
  tensorDetails <- getTensorDetails Input tTensor tElem tDims
  return (tensorDetails, id)
analyseNetworkInputTypes inputTypes@(x : _) = do
  elementType <- getElementType Input x
  let tensorDetails = TensorDetails (length inputTypes) elementType
  return (tensorDetails, transformInputNumericToTensor x)

analyseNetworkOutputType :: MonadNetworkTypeAnalysis m
                         => CheckedExpr -> m (TensorDetails, TransformOutput)
analyseNetworkOutputType tTensor@(TensorType _ tElem tDims) = do
  tensorDetails <- getTensorDetails Output tTensor tElem tDims
  return (tensorDetails, id)
analyseNetworkOutputType t = do
  elementType <- getElementType Output t
  let tensorDetails = TensorDetails 1 elementType
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
  let tDim  = NatLiteralExpr ann (NatType ann) 1 in
  let tDims = mkTensorDims ann [] in
  let atArgs = fmap (ExplicitArg ann) [e, NatLiteralExpr ann (NatType ann) 0] in
  AtExpr ann tElem tDim tDims atArgs

getTensorDetails :: MonadNetworkTypeAnalysis m
                 => InputOrOutput
                 -> CheckedExpr
                 -> CheckedExpr
                 -> CheckedExpr
                 -> m TensorDetails
getTensorDetails io tTensor tElem tDims = do
  typ   <- getElementType io tElem
  size  <- getTensorSize io tTensor tDims
  return $ TensorDetails size typ

getElementType :: MonadNetworkTypeAnalysis m => InputOrOutput -> CheckedExpr -> m Builtin
getElementType _  (Builtin _ t)
  | t `elem` allowedNetworkElementTypes = return t
getElementType io t = do
  (ident, _) <- ask
  throwError $ NetworkTypeHasUnsupportedElementType ident t io

getTensorSize :: MonadNetworkTypeAnalysis m
              => InputOrOutput
              -> CheckedExpr
              -> CheckedExpr
              -> m Int
getTensorSize io tTensor (SeqExpr _ _ _ [d]) = case d of
  NatLiteralExpr _ _ n -> return n
  _                    -> do
    (ident, _) <- ask
    throwError $ NetworkTypeHasVariableSizeTensor ident tTensor io
getTensorSize io tTensor _ = do
  (ident, _) <- ask
  throwError $ NetworkTypeHasMultidimensionalTensor ident tTensor io