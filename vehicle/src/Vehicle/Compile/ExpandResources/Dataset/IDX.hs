module Vehicle.Compile.ExpandResources.Dataset.IDX
  ( readIDX,
  )
where

import Control.Exception (try)
import Control.Monad (zipWithM)
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
import Data.Map.Ordered qualified as OMap
import Data.Vector.Generic qualified as V
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources.Core
import Vehicle.Compile.Normalise.Force
import Vehicle.Compile.Normalise.TypedValue (DimensionsValue (..), NatValue (..), toDimensionsValue, toNatValue)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Resource (DatasetElementType (..), DatasetType (..))
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Code.Interface
import Vehicle.Data.Real (ExtendedRational (..))
import Vehicle.Data.Tensor as Tensor (Tensor, TensorShape, fromVector, mapTensor)
import Vehicle.Data.Variable.Bound.Context.Name.Instance (runFreshNameBoundContextT)

-- The current dimension in the dataset being parsed
type CurrentDimension = Int

data ParseContext m a = ParseContext
  { declProv :: DeclProvenance, -- The provenance of the dataset declaration
    datasetFile :: FilePath, -- The path of the dataset
    expectedDatasetType :: GluedType Builtin, -- The overall dataset type
    actualDims :: TensorShape, -- Actual dimensions of dataset
    elemParser :: ElemParser m a
  }

-- | Reads the IDX dataset from the provided file, checking that the user type
-- matches the type of the stored data.
readIDX ::
  (MonadExpandResources m, MonadIO m) =>
  FilePath ->
  DeclProvenance ->
  Type Builtin ->
  DatasetType (Thunk Builtin) ->
  m (Thunk Builtin)
readIDX file decl rawDatasetType expectedType = do
  contents <- readIDXFile decl file
  case contents of
    Nothing -> throwError $ UnableToParseResource decl Dataset file
    Just idxData -> do
      let actualDimensions = Vector.toList $ idxDimensions idxData
      let gluedType = Glued rawDatasetType (fromDatasetType expectedType)
      let mkCtx :: ElemParser m a -> ParseContext m a
          mkCtx = ParseContext decl file gluedType actualDimensions
      if isIDXIntegral idxData
        then do
          let elems = idxIntContent idxData
          let parser = intElemParser decl gluedType file
          parseIDX (mkCtx parser) expectedType elems
        else do
          let elems = idxDoubleContent idxData
          let parser = doubleElemParser decl gluedType file
          parseIDX (mkCtx parser) expectedType elems

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
  DatasetType (Thunk Builtin) ->
  Vector a ->
  m (Thunk Builtin)
parseIDX ctx datasetType elems =
  parseContainer ctx 0 (actualDims ctx) elems datasetType

parseContainer ::
  (MonadExpandResources m, Vector.Unbox a) =>
  ParseContext m a ->
  CurrentDimension ->
  TensorShape ->
  Vector a ->
  DatasetType (Thunk Builtin) ->
  m (Thunk Builtin)
parseContainer ctx currentDim actualDims elems expectedType = case expectedType of
  DatasetListType expectedElemType ->
    parseList ctx currentDim expectedElemType actualDims elems
  DatasetVectorType expectedElemType dim ->
    parseVector ctx currentDim expectedElemType dim actualDims elems
  DatasetTensorType tElem expectedDims ->
    parseTensor ctx currentDim actualDims elems tElem expectedDims
  DatasetRecordType ident fields ->
    parseRecord ctx currentDim actualDims elems ident fields
  DatasetElementType typ ->
    elemParser ctx actualDims elems typ

parseRecord ::
  forall m a.
  (MonadExpandResources m, Vector.Unbox a) =>
  ParseContext m a ->
  CurrentDimension ->
  TensorShape ->
  Vector a ->
  Identifier ->
  GenericRecordFields (DatasetType (Thunk Builtin)) ->
  m (Thunk Builtin)
parseRecord ctx currentDim actualDims actualElems ident fields = do
  case actualDims of
    [] -> dimensionMismatchError ctx
    d : ds -> do
      checkDimension ctx currentDim (Forced $ INatLiteral $ length fields) d
      let splitElems = partitionData d ds actualElems
      exprs <- zipWithM (parseField ds) splitElems fields
      return $ Forced $ VRecord (Forced $ VFreeVar ident []) $ OMap.fromList exprs
  where
    parseField ::
      TensorShape ->
      Vector a ->
      GenericRecordField (DatasetType (Thunk Builtin)) ->
      m (FieldName, Thunk Builtin)
    parseField elemShape elems (fieldName, fieldType) = do
      fieldValue <- parseContainer ctx (currentDim + 1) elemShape elems fieldType
      return (fieldName, fieldValue)

parseTensor ::
  (MonadExpandResources m, Vector.Unbox a) =>
  ParseContext m a ->
  CurrentDimension ->
  TensorShape ->
  Vector a ->
  DatasetElementType (Thunk Builtin) ->
  Thunk Builtin ->
  m (Thunk Builtin)
parseTensor ctx currentDim actualDims elems expectedElemType expectedDims = do
  checkTensorDimensions ctx currentDim expectedDims actualDims
  elemParser ctx actualDims elems expectedElemType

checkTensorDimensions ::
  (MonadExpandResources m) =>
  ParseContext m a ->
  CurrentDimension ->
  UnforcedType Builtin ->
  TensorShape ->
  m ()
checkTensorDimensions ctx dimNo expectedShape actualShape = do
  forcedShape <- runFreshNameBoundContextT $ forceThunk expectedShape
  case (toDimensionsValue forcedShape, actualShape) of
    (VDimsNil, []) -> return ()
    (VDimsCons dim dims, d : ds) -> do
      checkDimension ctx dimNo dim d
      checkTensorDimensions ctx (dimNo + 1) dims ds
    _ -> dimensionMismatchError ctx

checkDimension ::
  (MonadExpandResources m) =>
  ParseContext m a ->
  CurrentDimension ->
  UnforcedType Builtin ->
  Int ->
  m ()
checkDimension ctx currentDim expectedDimValue actualDim = do
  forcedDim <- runFreshNameBoundContextT $ forceThunk expectedDimValue
  case toNatValue forcedDim of
    VNatLiteral expectedDim
      | expectedDim == actualDim -> return ()
      | otherwise -> do
          throwError $ DatasetDimensionSizeMismatch (declProv ctx) (datasetFile ctx) expectedDim actualDim currentDim
    VNatParameter dimIdent -> do
      implicitParams <- getInferableParameterContext
      let newEntry = (declProv ctx, Dataset, actualDim)
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
  CurrentDimension ->
  DatasetType (Thunk Builtin) ->
  TensorShape ->
  Vector a ->
  m (Thunk Builtin)
parseList ctx currentDim expectedElemType actualDims actualElems =
  case actualDims of
    [] -> dimensionMismatchError ctx
    d : ds -> do
      let splitElems = partitionData d ds actualElems
      exprs <- traverse (\es -> parseContainer ctx (currentDim + 1) ds es expectedElemType) splitElems
      return $ Forced $ mkListExpr (fromDatasetType expectedElemType) exprs

parseVector ::
  (MonadExpandResources m, Vector.Unbox a) =>
  ParseContext m a ->
  CurrentDimension ->
  DatasetType (Thunk Builtin) ->
  Thunk Builtin ->
  TensorShape ->
  Vector a ->
  m (Thunk Builtin)
parseVector ctx currentDim expectedElemType expectedDim actualDims actualElems =
  case actualDims of
    [] -> dimensionMismatchError ctx
    d : ds -> do
      checkDimension ctx currentDim expectedDim d
      let splitElems = partitionData d ds actualElems
      exprs <- traverse (\es -> parseContainer ctx (currentDim + 1) ds es expectedElemType) splitElems
      return $ Forced $ IVecLiteral (fromDatasetType expectedElemType) expectedDim exprs

type ElemParser m a =
  TensorShape ->
  Vector a ->
  DatasetElementType (Thunk Builtin) ->
  m (Thunk Builtin)

doubleElemParser ::
  (MonadExpandResources m) =>
  DeclProvenance ->
  GluedType Builtin ->
  FilePath ->
  ElemParser m Double
doubleElemParser decl datasetType file dims values expectedElementType =
  case expectedElementType of
    DatasetRealType {} -> do
      return $ Forced $ IRatTensor (mapTensor (Finite . toRational) (toTensor dims values))
    _ -> do
      throwError $ DatasetTypeMismatch decl file datasetType (fromDatasetElementType expectedElementType) "Rat"

intElemParser ::
  forall m.
  (MonadExpandResources m) =>
  DeclProvenance ->
  GluedType Builtin ->
  FilePath ->
  ElemParser m Int
intElemParser decl datasetType file dims values expectedElementType = do
  case expectedElementType of
    DatasetIndexType dim -> do
      forcedDim <- (runFreshNameBoundContextT $ forceThunk dim :: m (ForcedValue Builtin))
      case forcedDim of
        INatLiteral n ->
          case (dims, Vector.toList values) of
            ([], [value]) -> do
              if 0 <= value && value < n
                then return $ Forced $ IIndexLiteral value dim
                else throwError $ DatasetInvalidIndex decl file value n
            _ -> developerError "Should not be parsing tensors of indices"
        _ -> throwError $ DatasetTypeVariableSizeIndex decl datasetType forcedDim
    DatasetNatType {} -> do
      let invalid = Vector.filter (< 0) values
      if Vector.null invalid
        then return $ Forced $ INatTensor (toTensor dims values)
        else throwError $ DatasetInvalidNat decl file (Vector.head invalid)
    _ ->
      throwError $ DatasetTypeMismatch decl file datasetType (fromDatasetElementType expectedElementType) "Int"

-- | Split data by the first dimension of the C-Array.
partitionData :: (Vector.Unbox a) => Int -> TensorShape -> Vector a -> [Vector a]
partitionData dim dims content = do
  let entrySize = product dims
  i <- [0 .. dim - 1]
  return $ Vector.slice (i * entrySize) entrySize content

toTensor :: (Eq a, Vector.Unbox a) => TensorShape -> Vector a -> Tensor a
toTensor shape values = Tensor.fromVector shape (V.convert values)

variableSizeError :: (MonadCompile m) => ParseContext m a -> UnforcedType Builtin -> m b
variableSizeError ParseContext {..} dim =
  throwError $ DatasetVariableSizeTensor declProv expectedDatasetType dim

dimensionMismatchError :: (MonadCompile m) => ParseContext m a -> m b
dimensionMismatchError ParseContext {..} =
  throwError $ DatasetDimensionsMismatch declProv datasetFile expectedDatasetType actualDims
