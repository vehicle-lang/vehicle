module Vehicle.Backend.Queries
  ( compileToQueries,
  )
where

import Control.Monad (when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.Maybe (isNothing, maybeToList)
import System.Directory (createDirectoryIfMissing)
import Vehicle.Backend.Queries.Error
import Vehicle.Backend.Queries.UserVariableElimination (eliminateUserVariables)
import Vehicle.Backend.Queries.UserVariableElimination.Core
import Vehicle.Compile.Context.Free
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources (expandResources)
import Vehicle.Compile.ExpandResources.Core
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print.Warning ()
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Value
import Vehicle.Data.Tensor (TensorIndices, isZeroDimensional)
import Vehicle.Prelude.Warning (CompileWarning (..))
import Vehicle.Syntax.Tensor (unstack)
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat
import Vehicle.Verify.Specification
import Vehicle.Verify.Specification.IO

--------------------------------------------------------------------------------
-- Compilation to individual queries

-- | Compiles the provided program to individual queries suitable for a
-- verifier and outputs them. We need to output them as they are generated as
-- otherwise storing all the queries can result in an out-of-memory errors.
compileToQueries ::
  (MonadStdIO m, MonadCompile m) =>
  QueryFormat ->
  Prog Builtin ->
  Resources ->
  Maybe FilePath ->
  m ()
compileToQueries queryFormat typedProg resources maybeVerificationFolder = do
  -- Create the verification folder if required.
  case maybeVerificationFolder of
    Nothing -> return ()
    Just folder -> liftIO $ createDirectoryIfMissing True folder

  -- Expand out the external resources in the specification (datasets, networks etc.)
  (Main resourceFreeDecls, networkCtx, freeCtx, integrityInfo) <-
    expandResources resources typedProg

  -- Perform the actual compilation to queries
  properties <-
    runFreeContextT freeCtx $
      compileDecls typedProg queryFormat networkCtx 0 resourceFreeDecls maybeVerificationFolder

  -- Check that there were actually properties in the specification.
  when (null properties) $ do
    throwError NoPropertiesFound

  case maybeVerificationFolder of
    Nothing -> return ()
    Just folder -> do
      let verificationPlan = SpecificationCacheIndex integrityInfo properties
      writeSpecificationCache folder verificationPlan

--------------------------------------------------------------------------------
-- Getting properties

compileDecls ::
  (MonadStdIO m, MonadCompile m, MonadFreeContext Builtin m) =>
  Prog Builtin ->
  QueryFormat ->
  NetworkContext ->
  PropertyID ->
  [Decl Builtin] ->
  Maybe FilePath ->
  m [(Name, MultiProperty ())]
compileDecls _ _ _ _ [] _ = return []
compileDecls prog queryFormat networkCtx propertyID (d : ds) outputLocation = do
  property <- case d of
    DefFunction p ident anns _ body
      | isAnnotatedAsProperty anns -> do
          let propertyData = (queryFormat, networkCtx, (ident, p), propertyID, outputLocation)
          Just <$> compilePropertyDecl prog propertyData body
    _ -> return Nothing

  addDeclToContext d $ do
    let newPropertyID = if isNothing property then propertyID else propertyID + 1
    properties <- compileDecls prog queryFormat networkCtx newPropertyID ds outputLocation
    return $ maybeToList property ++ properties

type MultiPropertyMetaData =
  ( QueryFormat,
    NetworkContext,
    DeclProvenance,
    Int,
    Maybe FilePath
  )

updateMetaData :: MultiPropertyMetaData -> TensorIndices -> PropertyMetaData
updateMetaData (queryFormat, networkCtx, declProvenance, propertyID, outputLocation) indices =
  PropertyMetaData
    { networkCtx = networkCtx,
      queryFormat = queryFormat,
      propertyProvenance = declProvenance,
      propertyAddress = PropertyAddress propertyID (nameOf $ fst declProvenance) indices,
      outputLocation = outputLocation
    }

compilePropertyDecl ::
  (MonadStdIO m, MonadCompile m, MonadFreeContext Builtin m) =>
  Prog Builtin ->
  MultiPropertyMetaData ->
  Expr Builtin ->
  m (Name, MultiProperty ())
compilePropertyDecl prog propertyData@(_, _, declProv@(ident, _), _, _) expr = do
  logCompilerSection2 MinDetail ("property" <+> quotePretty ident) $ do
    normalisedExpr <- normaliseInEmptyEnv expr
    multiProperty <-
      compileMultiProperty propertyData normalisedExpr
        `catchError` handlePropertyCompileError prog propertyData
    return (nameOf (fst declProv), multiProperty)

handlePropertyCompileError :: (MonadCompile m) => Prog Builtin -> MultiPropertyMetaData -> CompileError -> m a
handlePropertyCompileError prog (queryFormat, _, declProv, _, _) e = do
  let formatID = queryFormatID queryFormat
  case e of
    UnsupportedNonLinearConstraint {} -> throwError =<< diagnoseNonLinearity formatID prog declProv
    UnsupportedAlternatingQuantifiers {} -> throwError =<< diagnoseAlternatingQuantifiers formatID prog declProv
    _ -> throwError e

-- | Compiles a property of type `Tensor Bool dims` for some variable `dims`,
-- by recursing through the levels of vectors until it reaches something of
-- type `Bool`.
compileMultiProperty ::
  forall m.
  (MonadStdIO m, MonadFreeContext Builtin m, MonadCompile m) =>
  MultiPropertyMetaData ->
  Value Builtin ->
  m (MultiProperty ())
compileMultiProperty multiPropertyMetaData = go []
  where
    go :: TensorIndices -> Value Builtin -> m (MultiProperty ())
    go indices expr = case expr of
      (getExpr accessStackTensor -> Just args) -> do
        let es' = zip [0 :: Int ..] $ stackElements args
        MultiProperty <$> traverse (\(i, e) -> go (i : indices) e) es'
      (getExpr accessBoolTensorLiteral -> Just bs) | not (isZeroDimensional bs) -> do
        -- Important to test for non-zero dimensionality otherwise we don't display the correct
        -- warnings for trivial tensors nor generate .vcl-plan file.
        let es' = zip [0 :: Int ..] (fromBoolTensorValue . VBoolTensorLiteral <$> unstack bs)
        MultiProperty <$> traverse (\(i, e) -> go (i : indices) e) es'
      (getExpr accessVecLit -> Just args) -> do
        let es' = zip [0 :: Int ..] $ vecLitElements args
        MultiProperty <$> traverse (\(i, e) -> go (i : indices) e) es'
      _ -> do
        let propertyMetaData@PropertyMetaData {..} = updateMetaData multiPropertyMetaData indices
        flip runReaderT propertyMetaData $ do
          logCompilerSection2 MinDetail ("property" <+> quotePretty propertyAddress) $ do
            compileSingleProperty expr
            return $ SingleProperty propertyAddress ()

-- Compiles an individual property
compileSingleProperty ::
  (MonadPropertyStructure m, MonadStdIO m) =>
  Value Builtin ->
  m ()
compileSingleProperty expr = do
  queries <- runSupplyT [1 :: QueryID ..] $ eliminateUserVariables expr

  PropertyMetaData {..} <- ask

  -- Warn if trivial.
  case queries of
    Trivial status -> logWarning (TrivialProperty propertyAddress status)
    _ -> return ()

  case outputLocation of
    Nothing -> return ()
    Just folder -> writePropertyVerificationPlan folder propertyAddress (PropertyVerificationPlan queries)
