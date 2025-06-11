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
import Data.Vector.Generic qualified as V
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources.Core
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Value
import Vehicle.Data.Tensor as Tensor (Tensor, TensorShape, fromVector, mapTensor)

-- | Reads the IDX dataset from the provided file, checking that the user type
-- matches the type of the stored data.
readIDX ::
  (MonadExpandResources m, MonadIO m) =>
  FilePath ->
  DeclProvenance ->
  GluedType Builtin ->
  m (Value Builtin)
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
  m (Value Builtin)
parseIDX ctx@(_, _, expectedDatasetType, actualDatasetDims, _) elems =
  parseContainer ctx True actualDatasetDims elems (normalised expectedDatasetType)

parseContainer ::
  (MonadExpandResources m, Vector.Unbox a) =>
  ParseContext m a ->
  Bool ->
  TensorShape ->
  Vector a ->
  VType Builtin ->
  m (Value Builtin)
parseContainer ctx topLevel actualDims elems expectedType = case toTypeValue expectedType of
  VListType expectedElemType -> parseList ctx expectedElemType actualDims elems
  VBoolTensorType expectedDims -> parseTensor ctx actualDims elems (fromTypeValue VBoolType) expectedDims
  VRatTensorType expectedDims -> parseTensor ctx actualDims elems (fromTypeValue VRatType) expectedDims
  VNatTensorType expectedDims -> parseTensor ctx actualDims elems (fromTypeValue VNatType) expectedDims
  VIndexTensorType n expectedDims -> parseTensor ctx actualDims elems (fromTypeValue $ VIndexType n) expectedDims
  _ ->
    if topLevel
      then typingError ctx
      else parseElements ctx actualDims elems expectedType

parseTensor ::
  (MonadExpandResources m, Vector.Unbox a) =>
  ParseContext m a ->
  TensorShape ->
  Vector a ->
  VType Builtin ->
  Value Builtin ->
  m (Value Builtin)
parseTensor ctx actualDims elems expectedElemType expectedDims = do
  checkTensorDimensions 0 ctx expectedDims actualDims
  parseElements ctx actualDims elems expectedElemType

checkTensorDimensions :: (MonadExpandResources m) => Int -> ParseContext m a -> VType Builtin -> TensorShape -> m ()
checkTensorDimensions dimNo ctx expectedShape actualShape = case (toDimensionsValue expectedShape, actualShape) of
  (VDimsNil, []) -> return ()
  (VDimsCons dim dims, d : ds) -> do
    checkTensorDimension dimNo ctx dim d
    checkTensorDimensions (dimNo + 1) ctx dims ds
  _ -> dimensionMismatchError ctx

checkTensorDimension :: (MonadExpandResources m) => Int -> ParseContext m a -> VType Builtin -> Int -> m ()
checkTensorDimension dimNo ctx@(decl, file, _, _, _) expectedDimValue actualDim = do
  case toNatValue expectedDimValue of
    VNatLiteral expectedDim
      | expectedDim == actualDim -> return ()
      | otherwise -> do
          throwError $ DatasetDimensionSizeMismatch decl file expectedDim actualDim dimNo
    VNatParameter dimIdent -> do
      implicitParams <- getInferableParameterContext
      let newEntry = (decl, Dataset, actualDim)
      case Map.lookup dimIdent implicitParams of
        Nothing -> variableSizeError ctx expectedDimValue
        Just (p, declType, entry) -> case entry of
          Nothing -> addPossibleInferableParameterSolution dimIdent p declType newEntry
          Just existingEntry@(_, _, value)
            | value == actualDim -> return ()
            | otherwise -> throwError $ InferableParameterContradictory dimIdent existingEntry newEntry
    _ -> variableSizeError ctx expectedDimValue

parseList ::
  (MonadExpandResources m, Vector.Unbox a) =>
  ParseContext m a ->
  VType Builtin ->
  TensorShape ->
  Vector a ->
  m (Value Builtin)
parseList ctx expectedElemType actualDims actualElems =
  case actualDims of
    [] -> dimensionMismatchError ctx
    d : ds -> do
      let splitElems = partitionData d ds actualElems
      exprs <- traverse (\es -> parseContainer ctx False ds es expectedElemType) splitElems
      return $ mkListExpr expectedElemType exprs

parseElements ::
  (MonadExpandResources m, Vector.Unbox a) =>
  ParseContext m a ->
  TensorShape ->
  Vector a ->
  VType Builtin ->
  m (Value Builtin)
parseElements (_, _, _, _, elemParser) = elemParser

type ParseContext m a =
  ( DeclProvenance, -- The provenance of the dataset declaration
    FilePath, -- The path of the dataset
    GluedType Builtin, -- The overall dataset type
    TensorShape, -- Actual dimensions of dataset
    ElemParser m a
  )

type ElemParser m a = TensorShape -> Vector a -> VType Builtin -> m (Value Builtin)

doubleElemParser ::
  (MonadExpandResources m) =>
  DeclProvenance ->
  GluedType Builtin ->
  FilePath ->
  ElemParser m Double
doubleElemParser decl datasetType file dims values expectedElementType =
  case toTypeValue expectedElementType of
    VRatType {} -> do
      return $ IRatTensor (mapTensor toRational (toTensor dims values))
    _ -> do
      throwError $ DatasetTypeMismatch decl file datasetType expectedElementType "Rat"

intElemParser ::
  (MonadExpandResources m) =>
  DeclProvenance ->
  GluedType Builtin ->
  FilePath ->
  ElemParser m Int
intElemParser decl datasetType file dims values expectedElementType = do
  logDebug MaxDetail $ prettyVerbose expectedElementType
  case toTypeValue expectedElementType of
    VIndexType (INatLiteral n) -> do
      let invalid = Vector.filter (\value -> value < 0 || value >= n) values
      if Vector.null invalid
        then return $ IIndexTensor (toTensor dims values)
        else throwError $ DatasetInvalidIndex decl file (Vector.head invalid) n
    VNatType {} -> do
      let invalid = Vector.filter (< 0) values
      if Vector.null invalid
        then return $ INatTensor (toTensor dims values)
        else throwError $ DatasetInvalidNat decl file (Vector.head invalid)
    _ ->
      throwError $ DatasetTypeMismatch decl file datasetType expectedElementType "Int"

-- | Split data by the first dimension of the C-Array.
partitionData :: (Vector.Unbox a) => Int -> TensorShape -> Vector a -> [Vector a]
partitionData dim dims content = do
  let entrySize = product dims
  i <- [0 .. dim - 1]
  return $ Vector.slice (i * entrySize) entrySize content

toTensor :: (Eq a, Vector.Unbox a) => TensorShape -> Vector a -> Tensor a
toTensor shape values = Tensor.fromVector shape (V.convert values)

variableSizeError :: (MonadCompile m) => ParseContext m a -> Value Builtin -> m b
variableSizeError (decl, _, expectedDatasetType, _, _) dim =
  throwError $ DatasetVariableSizeTensor decl expectedDatasetType dim

dimensionMismatchError :: (MonadCompile m) => ParseContext m a -> m b
dimensionMismatchError (decl, file, expectedDatasetType, actualDatasetDims, _) =
  throwError $ DatasetDimensionsMismatch decl file expectedDatasetType actualDatasetDims

typingError :: (MonadCompile m) => ParseContext m a -> m b
typingError (_, _, expectedDatasetType, _, _) =
  compilerDeveloperError $
    "Invalid dataset type"
      <+> squotes (prettyVerbose (normalised expectedDatasetType))
      <+> "should have been caught during type-checking"
