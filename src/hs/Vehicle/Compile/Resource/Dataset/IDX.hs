
module Vehicle.Compile.Resource.Dataset.IDX
  ( readIDX
  ) where


import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.Reader
import Data.IDX
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Control.Exception
import Vehicle.Compile.Resource.Core

-- | Reads the IDX dataset from the provided file, checking that the user type
-- matches the type of the stored data.
readIDX :: (MonadCompile m, MonadIO m)
        => FilePath
        -> Identifier
        -> Provenance
        -> DatasetType
        -> m CheckedExpr
readIDX file ident prov datasetType =
  flip runReaderT (ident, prov) $ do
    contents <- readIDXFile file
    case contents of
      Nothing      -> throwError $ UnableToParseResource (ident, prov) Dataset file
      Just idxData -> do
        let baseType = datasetBaseType datasetType
        let dims = idxDimensions idxData

        if isIDXIntegral idxData then do
          let elems = idxIntContent idxData
          baseParser <- intElemParser baseType
          parseIDX prov ident datasetType baseParser dims elems
        else do
          let elems = idxDoubleContent idxData
          baseParser <- doubleElemParser baseType
          parseIDX prov ident datasetType baseParser dims elems

type MonadDataset m = (MonadReader (Identifier, Provenance) m, MonadCompile m)

readIDXFile :: (MonadIO m, MonadDataset m)
            => FilePath
            -> m (Maybe IDXData)
readIDXFile file = do
  result <- liftIO $ try (decodeIDXFile file)
  case result of
    Right idxData  -> return idxData
    Left  ioExcept -> do
      (ident, prov) <- ask
      throwError $ ResourceIOError (ident, prov) Dataset ioExcept

parseIDX :: forall m a . (MonadDataset m, Vector.Unbox a)
         => Provenance
         -> Identifier
         -> DatasetType
         -> (a -> m CheckedExpr)
         -> Vector Int
         -> Vector a
         -> m CheckedExpr
parseIDX ann ident datasetType elemParser actualDims =
  go actualDims datasetType
  where
    mismatchError :: CompileError
    mismatchError =
      let expectedType = reconstructDatasetType ann datasetType in
      let actualDimList = Vector.toList actualDims in
       DatasetDimensionMismatch (ident, ann) expectedType actualDimList

    go :: Vector Int
       -> DatasetType
       -> Vector a
       -> m CheckedExpr
    go dims DatasetBaseType{} elems
      | not (Vector.null dims)   = throwError mismatchError
      | Vector.length elems /= 1 = compilerDeveloperError "Malformed IDX file: mismatch between dimensions and acutal data"
      | otherwise                = elemParser (Vector.head elems)

    go dims (DatasetListType tElem) elems = case Vector.uncons dims of
      Nothing      -> throwError mismatchError
      Just (d, ds) -> do
        let splitElems = partitionData d ds elems
        exprs <- traverse (go ds tElem) splitElems
        let elemType = reconstructDatasetType ann tElem
        return $ mkList ann elemType exprs

    go dims (DatasetTensorType tElem tDims) elems = do
      let size = length tDims
      if size > Vector.length dims
        then throwError mismatchError
        else do
          let ds = Vector.toList (Vector.take size dims)
          if  ds /= tDims
            then throwError mismatchError
            else do
              goTensor (length ds) tElem dims elems

    goTensor :: Int
             -> DatasetType
             -> Vector Int
             -> Vector a
             -> m CheckedExpr
    goTensor 0 tElem dims contents = go dims tElem contents
    goTensor n tElem dims contents = case Vector.uncons dims of
      Nothing      -> throwError mismatchError
      Just (d, ds) -> do
        let rows = partitionData d ds contents
        rowExprs <- traverse (goTensor (n-1) tElem ds) rows
        let tensorDims = Vector.toList $ Vector.take n dims
        let baseElemType = reconstructDatasetType ann tElem
        return $ mkTensor ann baseElemType tensorDims rowExprs

elementAnn :: MonadDataset m => m Provenance
elementAnn = do
  (ident, _) <- ask
  return $ datasetProvenance (nameOf ident)

doubleElemParser :: MonadDataset m => DatasetBaseType -> m (Double -> m CheckedExpr)
doubleElemParser tElem = do
  (ident, prov) <- ask
  ann <- elementAnn
  let baseType = reconstructDatasetBaseType ann tElem
  case tElem of
    DatasetRatType -> do
      return (\v ->
        return $ RatLiteralExpr ann baseType (toRational v))
    _ -> do
      throwError $ DatasetTypeMismatch (ident, prov) baseType Rat

intElemParser :: MonadDataset m => DatasetBaseType -> m (Int -> m CheckedExpr)
intElemParser tElem = do
  (ident, prov) <- ask
  ann <- elementAnn
  let baseType = reconstructDatasetBaseType ann tElem
  case tElem of
    DatasetIntType -> return (\v ->
      return $ IntLiteralExpr ann baseType v)
    DatasetNatType -> return (\v ->
      if v >= 0
        then return $ NatLiteralExpr ann baseType v
        else throwError $ DatasetInvalidNat (ident, prov) v)
    DatasetIndexType n -> return (\v ->
      if v >= 0 && v < n
        then return $ NatLiteralExpr ann baseType v
        else throwError $ DatasetInvalidIndex (ident, ann) n v)
    _ -> throwError $ DatasetTypeMismatch (ident, prov) baseType Int

-- | Split data by the first dimension of the C-Array.
partitionData :: Vector.Unbox a => Int -> Vector Int -> Vector a -> [Vector a]
partitionData dim dims content = do
  let entrySize = Vector.product dims
  i <- [0 .. dim - 1]
  return $ Vector.slice (i * entrySize) entrySize content