
module Vehicle.Resource.Dataset.IDX
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
        -> CheckedExpr
        -> m CheckedExpr
readIDX file ident prov expectedType =
  flip runReaderT (ident, prov) $ do
    contents <- readIDXFile file
    case contents of
      Nothing      -> throwError $ UnableToParseResource ident prov Dataset file
      Just idxData -> parseIDX idxData (idxDimensions idxData) expectedType

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
         -> Vector Int
         -> CheckedExpr
         -> m CheckedExpr
parseIDX idxData idxDims tList@(ListType _ tElem) = do
  (tInnerElem, dims) <- getTensorTypeArgs tElem
  -- If the user has given the dataset a list type then we ignore
  -- the first dimension as listed in the idx file.
  checkDimsMatch dims (Vector.tail idxDims)

  parsedElems <-
    if isIDXIntegral idxData then do
      let elems = partitionedIntData idxData
      innerElemParser <- intElemParser tInnerElem
      let parser = parseTensor tInnerElem dims innerElemParser
      traverse parser elems
    else do
      let elems = partitionedDoubleData idxData
      innerElemParser <- doubleElemParser tInnerElem
      let parser = parseTensor tInnerElem dims innerElemParser
      traverse parser elems

  (_, prov) <- ask
  return $ SeqExpr (prov, TheMachine) tElem tList parsedElems

parseIDX idxData idxDims tTensor@TensorType{} = do
  (tInnerElem, dims) <- getTensorTypeArgs tTensor
  checkDimsMatch dims idxDims
  if isIDXIntegral idxData then do
    let elems = idxIntContent idxData
    innerElemParser <- intElemParser tInnerElem
    parseTensor tInnerElem dims innerElemParser elems
  else do
    let elems = idxDoubleContent idxData
    innerElemParser <- doubleElemParser tInnerElem
    parseTensor tInnerElem dims innerElemParser elems

parseIDX _ _ tCont = do
  (ident, prov) <- ask
  throwError $ DatasetInvalidContainerType ident prov tCont

getTensorTypeArgs :: MonadDataset m => CheckedExpr -> m (CheckedExpr, [Int])
getTensorTypeArgs t@BuiltinNumericType{}    =
  return (t, [])
getTensorTypeArgs t@FinType{} =
  return (t, [])
getTensorTypeArgs (TensorType _ tElem dims) =
  case getDimensions dims of
    Just ds -> return (tElem, ds)
    Nothing -> do
      (ident, prov) <- ask
      throwError $ DatasetVariableSizeTensor ident prov dims
getTensorTypeArgs t = do
  (ident, prov) <- ask
  throwError $ DatasetInvalidElementType ident prov t

checkDimsMatch :: MonadDataset m => [Int] -> Vector Int -> m ()
checkDimsMatch userDims idxDims
  | userDims == Vector.toList idxDims = return ()
  | otherwise = do
    (ident, prov) <- ask
    throwError $ DatasetDimensionMismatch ident prov userDims (Vector.toList idxDims)

doubleElemParser :: MonadDataset m => CheckedExpr -> m (Double -> m CheckedExpr)
doubleElemParser tElem = do
  (ident, prov) <- ask
  let ann = (prov, TheMachine)
  case tElem of
    RatType _ -> return (\v ->
      return $ RatLiteralExpr ann Rat (toRational v))
    _         -> throwError $ DatasetTypeMismatch ident prov tElem Rat

intElemParser :: MonadDataset m => CheckedExpr -> m (Int -> m CheckedExpr)
intElemParser tElem = do
  (ident, prov) <- ask
  let ann = (prov, TheMachine)
  case tElem of
    IntType _   -> return (\v ->
      return $ IntLiteralExpr ann Int v)
    NatType _   -> return (\v ->
      if v >= 0
        then return $ NatLiteralExpr ann tElem v
        else throwError $ DatasetInvalidNat ident prov v)
    FinType _ (NatLiteralExpr _ _ n) -> return (\v ->
      if v >= 0 && v < n
        then return $ NatLiteralExpr ann tElem v
        else throwError $ DatasetInvalidFin ident prov n v)
    _ -> throwError $ DatasetTypeMismatch ident prov tElem Int

parseTensor :: (MonadDataset m, Vector.Unbox a)
           => CheckedExpr
           -> [Int]
           -> (a -> m CheckedExpr)
           -> Vector a
           -> m CheckedExpr
parseTensor _     []              elemParser contents =
  elemParser (Vector.head contents)
parseTensor tElem ds@(dim : dims) elemParser contents = do
  let rows = partitionData dim dims contents
  rowExprs <- traverse (parseTensor tElem dims elemParser) rows
  ann <- asks ((, TheMachine) . snd)
  return $ SeqExpr ann tElem (mkTensorType ann tElem ds) rowExprs

-- | Split data by the first dimension of the C-Array.
partitionData :: Vector.Unbox a => Int -> [Int] -> Vector a -> [Vector a]
partitionData dim dims content = do
  let entrySize = product dims
  i <- [0 .. dim - 1]
  return $ Vector.slice (i * entrySize) entrySize content