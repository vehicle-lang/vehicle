
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

-- | Reads the IDX dataset from the provided file, checking that the user type
-- matches the type of the stored data.
readIDX :: (MonadCompile m, MonadIO m)
        => FilePath
        -> Identifier
        -> Provenance
        -> m InputExpr
readIDX file ident prov =
  flip runReaderT (ident, prov) $ do
    contents <- readIDXFile file
    case contents of
      Nothing      -> throwError $ UnableToParseResource ident prov Dataset file
      Just idxData -> parseIDX idxData (Vector.toList $ idxDimensions idxData)

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
      throwError $ ResourceIOError ident prov Dataset ioExcept

parseIDX :: MonadDataset m
         => IDXData
         -> [Int]
         -> m InputExpr
parseIDX idxData idxDims = do
  ann <- elementAnn
  if isIDXIntegral idxData then do
    let elems = idxIntContent idxData
    parseTensor idxDims (mkIntExpr ann) elems
  else do
    let elems = idxDoubleContent idxData
    parseTensor idxDims (mkDoubleExpr ann) elems

elementAnn :: MonadDataset m => m CheckedAnn
elementAnn = do
  (ident, _) <- ask
  return $ datasetProvenance (nameOf ident)

parseTensor :: (MonadDataset m, Vector.Unbox a)
           => [Int]
           -> (a -> InputExpr)
           -> Vector a
           -> m InputExpr
parseTensor []           elemParser contents =
  return $ elemParser (Vector.head contents)
parseTensor (dim : dims) elemParser contents = do
  let rows = partitionData dim dims contents
  rowExprs <- traverse (parseTensor dims elemParser) rows
  ann <- elementAnn
  return $ mkSeqExpr ann rowExprs

-- | Split data by the first dimension of the C-Array.
partitionData :: Vector.Unbox a => Int -> [Int] -> Vector a -> [Vector a]
partitionData dim dims content = do
  let entrySize = product dims
  i <- [0 .. dim - 1]
  return $ Vector.slice (i * entrySize) entrySize content