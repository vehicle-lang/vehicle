module Vehicle.Compile.ExpandResources.Dataset.IDX
  ( readIDX,
  )
where

import Control.Exception (try)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (..))
import Data.IDX
  ( IDXData,
    decodeIDXFile,
    idxDimensions,
    idxDoubleContent,
    idxIntContent,
    isIDXIntegral,
  )
import Data.Map qualified as Map
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources.Core
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Data.BuiltinInterface
import Vehicle.Data.NormalisedExpr

-- | Reads the IDX dataset from the provided file, checking that the user type
-- matches the type of the stored data.
readIDX ::
  (MonadExpandResources m, MonadIO m) =>
  FilePath ->
  DeclProvenance ->
  GluedType Builtin ->
  m (WHNFValue Builtin)
readIDX file decl expectedType = do
  contents <- readIDXFile decl file
  case contents of
    Nothing -> throwError $ UnableToParseResource decl Dataset file
    Just idxData -> do
      let actualDimensions = Vector.toList $ idxDimensions idxData
      if isIDXIntegral idxData
        then do
          let elems = idxIntContent idxData
          let parser = intElemParser decl expectedType file
          let ctx = (decl, file, expectedType, actualDimensions, parser)
          parseIDX ctx elems
        else do
          let elems = idxDoubleContent idxData
          let parser = doubleElemParser decl expectedType file
          let ctx = (decl, file, expectedType, actualDimensions, parser)
          parseIDX ctx elems

readIDXFile ::
  (MonadCompile m, MonadIO m) =>
  DeclProvenance ->
  FilePath ->
  m (Maybe IDXData)
readIDXFile decl file = do
  result <- liftIO $ try (decodeIDXFile file)
  case result of
    Right idxData -> return idxData
    Left ioExcept -> do
      throwError $ ResourceIOError decl Dataset ioExcept

-- WARNING: There appears to be a pernicious bug with the
-- current version of the HLS (VSCode plugin v2.2.0, HLS v1.7.0)
-- where the below function causes the IDE to start spinning forever shortly
-- after changing things in this file. Can't currently find a workaround.
parseIDX ::
  (MonadExpandResources m, Vector.Unbox a) =>
  ParseContext m a ->
  Vector a ->
  m (WHNFValue Builtin)
parseIDX ctx@(_, _, expectedDatasetType, actualDatasetDims, _) elems = do
  parseContainer ctx True actualDatasetDims elems (normalised expectedDatasetType)

parseContainer ::
  (MonadExpandResources m, Vector.Unbox a) =>
  ParseContext m a ->
  Bool ->
  TensorDimensions ->
  Vector a ->
  WHNFType Builtin ->
  m (WHNFValue Builtin)
parseContainer ctx topLevel actualDims elems expectedType = case expectedType of
  VListType expectedElemType -> parseList ctx expectedElemType actualDims elems
  VVectorType expectedElemType expectedDim -> parseVector ctx actualDims elems expectedElemType expectedDim
  _ ->
    if topLevel
      then typingError ctx
      else parseElement ctx actualDims elems expectedType

parseVector ::
  (MonadExpandResources m, Vector.Unbox a) =>
  ParseContext m a ->
  TensorDimensions ->
  Vector a ->
  WHNFType Builtin ->
  WHNFValue Builtin ->
  m (WHNFValue Builtin)
parseVector ctx [] _ _ _ = dimensionMismatchError ctx
parseVector ctx@(decl, file, _, allDims, _) (actualDim : actualDims) elems expectedElemType expectedDim = do
  currentDim <- case expectedDim of
    VNatLiteral n ->
      if n == actualDim
        then return actualDim
        else throwError $ DatasetDimensionSizeMismatch decl file n actualDim allDims (actualDim : actualDims)
    VFreeVar dimIdent _ -> do
      implicitParams <- getInferableParameterContext
      let newEntry = (decl, Dataset, actualDim)
      case Map.lookup dimIdent implicitParams of
        Nothing -> variableSizeError ctx expectedDim
        Just Left {} -> do
          addPossibleInferableParameterSolution dimIdent newEntry
          return actualDim
        Just (Right existingEntry@(_, _, value)) ->
          if value == actualDim
            then return value
            else throwError $ InferableParameterContradictory dimIdent existingEntry newEntry
    _ -> variableSizeError ctx expectedDim

  let rows = partitionData currentDim actualDims elems
  rowExprs <- traverse (\es -> parseContainer ctx False actualDims es expectedElemType) rows
  return $ mkVLVec rowExprs

parseList ::
  (MonadExpandResources m, Vector.Unbox a) =>
  ParseContext m a ->
  WHNFType Builtin ->
  TensorDimensions ->
  Vector a ->
  m (WHNFValue Builtin)
parseList ctx expectedElemType actualDims actualElems =
  case actualDims of
    [] -> dimensionMismatchError ctx
    d : ds -> do
      let splitElems = partitionData d ds actualElems
      exprs <- traverse (\es -> parseContainer ctx False ds es expectedElemType) splitElems
      return $ mkVList exprs

parseElement ::
  (MonadExpandResources m, Vector.Unbox a) =>
  ParseContext m a ->
  TensorDimensions ->
  Vector a ->
  WHNFType Builtin ->
  m (WHNFValue Builtin)
parseElement ctx@(_, _, _, _, elemParser) dims elems expectedType
  | not (null dims) = dimensionMismatchError ctx
  | Vector.length elems /= 1 = compilerDeveloperError "Malformed IDX file: mismatch between dimensions and acutal data"
  | otherwise = elemParser (Vector.head elems) expectedType

type ParseContext m a =
  ( DeclProvenance, -- The provenance of the dataset declaration
    FilePath, -- The path of the dataset
    GluedType Builtin, -- The overall dataset type
    TensorDimensions, -- Actual dimensions of dataset
    ElemParser m a
  )

type ElemParser m a = a -> WHNFType Builtin -> m (WHNFValue Builtin)

doubleElemParser ::
  (MonadExpandResources m) =>
  DeclProvenance ->
  GluedType Builtin ->
  FilePath ->
  ElemParser m Double
doubleElemParser decl datasetType file value expectedElementType = case expectedElementType of
  VRatType {} ->
    return $ VRatLiteral (toRational value)
  _ -> do
    throwError $ DatasetTypeMismatch decl file datasetType expectedElementType VRatType

intElemParser ::
  (MonadExpandResources m) =>
  DeclProvenance ->
  GluedType Builtin ->
  FilePath ->
  ElemParser m Int
intElemParser decl datasetType file value expectedElementType = case expectedElementType of
  VIndexType (VNatLiteral n) ->
    if value >= 0 && value < n
      then return $ VIndexLiteral value
      else throwError $ DatasetInvalidIndex decl file value n
  VNatType {} ->
    if value >= 0
      then return $ VNatLiteral value
      else throwError $ DatasetInvalidNat decl file value
  VIntType {} ->
    return $ VIntLiteral value
  _ ->
    throwError $ DatasetTypeMismatch decl file datasetType expectedElementType VIntType

-- | Split data by the first dimension of the C-Array.
partitionData :: (Vector.Unbox a) => Int -> TensorDimensions -> Vector a -> [Vector a]
partitionData dim dims content = do
  let entrySize = product dims
  i <- [0 .. dim - 1]
  return $ Vector.slice (i * entrySize) entrySize content

variableSizeError :: (MonadCompile m) => ParseContext m a -> WHNFValue Builtin -> m b
variableSizeError (decl, _, expectedDatasetType, _, _) dim =
  throwError $ DatasetVariableSizeTensor decl expectedDatasetType dim

dimensionMismatchError :: (MonadCompile m) => ParseContext m a -> m b
dimensionMismatchError (decl, file, expectedDatasetType, actualDatasetDims, _) =
  throwError $ DatasetDimensionsMismatch decl file expectedDatasetType actualDatasetDims

typingError :: (MonadCompile m) => ParseContext m a -> m b
typingError (_, _, expectedDatasetType, _, _) =
  compilerDeveloperError $
    "Invalid parameter type"
      <+> squotes (prettyVerbose (unnormalised expectedDatasetType))
      <+> "should have been caught during type-checking"
