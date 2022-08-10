module Vehicle.Compile.ExpandResources.Dataset.IDX
  ( readIDX
  ) where

import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.State
import Control.Exception
import Data.IDX
import Data.Map qualified as Map
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Language.Print
import Vehicle.Compile.ExpandResources.Core

-- | Reads the IDX dataset from the provided file, checking that the user type
-- matches the type of the stored data.
readIDX :: (MonadExpandResources m, MonadIO m)
        => FilePath
        -> DeclProvenance
        -> CheckedType
        -> m CheckedExpr
readIDX file decl expectedType = do
  contents <- readIDXFile decl file
  case contents of
    Nothing      -> throwError $ UnableToParseResource decl Dataset file
    Just idxData -> do
      let actualDimensions = Vector.toList $ idxDimensions idxData
      if isIDXIntegral idxData then do
        let elems = idxIntContent idxData
        let parser = intElemParser decl file
        let ctx = (decl, file, expectedType, actualDimensions, parser)
        parseIDX ctx elems
      else do
        let elems = idxDoubleContent idxData
        let parser = doubleElemParser decl file
        let ctx = (decl, file, expectedType, actualDimensions, parser)
        parseIDX ctx elems

readIDXFile :: (MonadCompile m, MonadIO m)
            => DeclProvenance
            -> FilePath
            -> m (Maybe IDXData)
readIDXFile decl file = do
  result <- liftIO $ try (decodeIDXFile file)
  case result of
    Right idxData  -> return idxData
    Left  ioExcept -> do
      throwError $ ResourceIOError decl Dataset ioExcept

-- WARNING: There appears to be a pernicious bug with the
-- current version of the HLS (VSCode plugin v2.2.0, HLS v1.7.0)
-- where the below function causes the IDE to start spinning forever shortly
-- after changing things in this file. Can't currently find a workaround.
parseIDX ::  ( MonadExpandResources m, Vector.Unbox a)
            => ParseContext m a
            -> Vector a
            -> m CheckedExpr
parseIDX ctx@(_, _, expectedDatasetType, actualDatasetDims, _) elems = do
  parseContainer ctx True actualDatasetDims elems expectedDatasetType

parseContainer :: (MonadExpandResources m, Vector.Unbox a)
               => ParseContext m a
               -> Bool
               -> [Int]
               -> Vector a
               -> CheckedType
               -> m CheckedExpr
parseContainer ctx topLevel actualDims elems expectedType = case expectedType of
  ListType   _ expectedElemType             -> parseList ctx expectedElemType actualDims elems
  VectorType _ expectedElemType expectedDim -> parseVector ctx actualDims elems expectedElemType expectedDim
  _ -> if topLevel
    then typingError ctx
    else parseElement ctx actualDims elems expectedType


parseVector :: (MonadExpandResources m, Vector.Unbox a)
            => ParseContext m a
            -> [Int]
            -> Vector a
            -> CheckedType
            -> CheckedExpr
            -> m CheckedExpr
parseVector ctx [] _ _ _  = dimensionMismatchError ctx
parseVector ctx@(decl, file, _, allDims, _) (actualDim : actualDims) elems expectedElemType expectedDim = do
  currentDim <- case expectedDim of
    NatLiteral _ n ->
      if n == actualDim
        then return actualDim
        else throwError $ DatasetDimensionSizeMismatch decl file n actualDim allDims (actualDim : actualDims)

    FreeVar _ dimIdent -> do
      implicitParams <- get
      let newEntry = (decl, Dataset, actualDim)
      case Map.lookup (nameOf dimIdent) implicitParams of
        Nothing       -> variableSizeError ctx expectedDim

        Just Nothing -> do
          modify (Map.insert (nameOf dimIdent) (Just newEntry))
          return actualDim

        Just (Just existingEntry@(_, _, value)) ->
          if value == actualDim
            then return value
            else throwError $ InferableParameterContradictory dimIdent existingEntry newEntry

    _ -> variableSizeError ctx expectedDim

  let rows = partitionData currentDim actualDims elems
  rowExprs <- traverse (\es -> parseContainer ctx False actualDims es expectedElemType) rows
  return $ VecLiteral (snd decl) expectedElemType rowExprs

parseList :: (MonadExpandResources m, Vector.Unbox a)
          => ParseContext m a
          -> CheckedType
          -> [Int]
          -> Vector a
          -> m CheckedExpr
parseList ctx@(decl, _, _, _, _) expectedElemType actualDims actualElems =
  case actualDims of
    []     -> dimensionMismatchError ctx
    d : ds -> do
      let splitElems = partitionData d ds actualElems
      exprs <- traverse (\es -> parseContainer ctx False ds es expectedElemType) splitElems
      return $ mkList (snd decl) expectedElemType exprs

parseElement :: (MonadExpandResources m, Vector.Unbox a)
             => ParseContext m a
             -> [Int]
             -> Vector a
             -> CheckedType
             -> m CheckedExpr
parseElement ctx@(_, _, _, _, elemParser) dims elems expectedType
  | not (null dims)          = dimensionMismatchError ctx
  | Vector.length elems /= 1 = compilerDeveloperError "Malformed IDX file: mismatch between dimensions and acutal data"
  | otherwise                = elemParser (Vector.head elems) expectedType

type ParseContext m a =
  ( DeclProvenance
  , FilePath
  , CheckedType
  , [Int]           -- Actual dimensions of dataset
  , ElemParser m a
  )

type ElemParser m a = a -> CheckedType -> m CheckedExpr

doubleElemParser :: MonadExpandResources m
                 => DeclProvenance
                 -> FilePath
                 -> ElemParser m Double
doubleElemParser decl file value typeInProgram = do
  let p = freshProvenance decl
  case typeInProgram of
    RatType{} ->
      return $ RatLiteral p (toRational value)
    _ -> do
      throwError $ DatasetTypeMismatch decl file typeInProgram (RatType p)

intElemParser :: MonadExpandResources m
              => DeclProvenance
              -> FilePath
              -> ElemParser m Int
intElemParser decl file value typeInProgram = do
  let p = freshProvenance decl
  case typeInProgram of
    ConcreteIndexType _ n ->
      if value >= 0 && value < n
        then return $ IndexLiteral p n value
        else throwError $ DatasetInvalidIndex decl file value n
    NatType{} ->
      if value >= 0
        then return $ NatLiteral p value
        else throwError $ DatasetInvalidNat decl file value
    IntType{} ->
      return $ IntLiteral p value
    _ ->
      throwError $ DatasetTypeMismatch decl file typeInProgram (IntType p)

-- | Split data by the first dimension of the C-Array.
partitionData :: Vector.Unbox a => Int -> [Int] -> Vector a -> [Vector a]
partitionData dim dims content = do
  let entrySize = product dims
  i <- [0 .. dim - 1]
  return $ Vector.slice (i * entrySize) entrySize content

freshProvenance :: DeclProvenance -> Provenance
freshProvenance (ident, _) = datasetProvenance (nameOf ident)

variableSizeError :: MonadCompile m => ParseContext m a -> CheckedExpr -> m b
variableSizeError (decl, _, _, _, _) dim =
  throwError $ DatasetVariableSizeTensor decl dim

dimensionMismatchError :: MonadCompile m => ParseContext m a -> m b
dimensionMismatchError (decl, file, expectedDatasetType, actualDatasetDims, _) =
  throwError $ DatasetDimensionsMismatch decl file expectedDatasetType actualDatasetDims

typingError :: MonadCompile m => ParseContext m a -> m b
typingError (_, _, expectedDatasetType, _, _) = compilerDeveloperError $
    "Invalid parameter type" <+> squotes (prettySimple expectedDatasetType) <+>
    "should have been caught during type-checking"