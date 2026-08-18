{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Vehicle.Verify.Specification.Execute.Reporting
  ( MonadProgressReporter (..),
    ProgressEvent (..),
    runTextProgressReporterT,
    runJSONProgressReporterT,
    VerificationSettings (..),
  )
where

import Control.Monad (when)
import Control.Monad.Except (ExceptT, mapExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks, mapReaderT)
import Control.Monad.State (MonadState (..), StateT (..), evalStateT, gets, mapStateT, modify)
import Control.Monad.Trans (MonadTrans (..))
import Data.Aeson.Encode.Pretty (encodePretty')
import Data.Aeson.Types
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString.Lazy.Char8 qualified as ByteString (unpack)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (intercalate, pack)
import Data.Text.Lazy qualified as LazyText
import GHC.Generics (Generic)
import Prettyprinter (fill)
import System.Console.ANSI (Color (..))
import System.IO (stdout)
import System.ProgressBar
import Vehicle.Compile.Prelude
import Vehicle.Data.MaybeTrivial (MaybeTrivial (..))
import Vehicle.Data.Tensor (prettyTensor)
import Vehicle.Verify.Core
import Vehicle.Verify.Solver as Core
import Vehicle.Verify.Specification.Status

--------------------------------------------------------------------------------
-- Interface
--------------------------------------------------------------------------------
--
-- Mechanism for reporting events that happen during execution of a verification plan

data VerificationSettings = VerificationSettings
  { solver :: Solver,
    solverExtraArgs :: [String],
    specificationCache :: FilePath,
    noSatPrint :: Bool
  }

class (Monad m, MonadReader VerificationSettings m) => MonadProgressReporter m where
  reportMultiProperty :: PropertyName -> m () -> m ()
  reportProperty :: PropertyAddress -> Int -> m PropertyStatus -> m PropertyStatus
  reportQuery :: QueryAddress -> m (Either SolverError (QueryResult UserVariableAssignment)) -> m (Either SolverError (QueryResult UserVariableAssignment))

{-
instance (MonadProgressReporter m) => MonadProgressReporter (ReaderT a m) where
  reportMultiProperty a s = mapReaderT (reportMultiProperty a s)
  reportProperty a i = mapReaderT (reportProperty a i)
  reportQuery s q = mapReaderT (reportQuery s q)
-}

-- If error reporting is doing funny things, I have my doubts about this implementation...
instance (MonadProgressReporter m) => MonadProgressReporter (ExceptT a m) where
  reportMultiProperty a = mapExceptT (>>= traverse (reportMultiProperty a . return))
  reportProperty a i = mapExceptT (>>= traverse (reportProperty a i . return))
  reportQuery q = mapExceptT (>>= traverse (reportQuery q . return))

--------------------------------------------------------------------------------
-- Multi-property summary

data MultiPropertySummary = MultiPropertySummary
  { numberVerified :: Int,
    numberFalsified :: Int,
    numberTimedOut :: Int,
    numberErrored :: Int
  }
  deriving (Generic)

instance Semigroup MultiPropertySummary where
  s1 <> s2 =
    MultiPropertySummary
      { numberVerified = numberVerified s1 + numberVerified s2,
        numberFalsified = numberFalsified s1 + numberFalsified s2,
        numberTimedOut = numberTimedOut s1 + numberTimedOut s2,
        numberErrored = numberErrored s1 + numberErrored s2
      }

instance Monoid MultiPropertySummary where
  mempty =
    MultiPropertySummary
      { numberVerified = 0,
        numberFalsified = 0,
        numberTimedOut = 0,
        numberErrored = 0
      }

instance ToJSON MultiPropertySummary

makeMultiPropertyStatus :: PropertyStatus -> MultiPropertySummary
makeMultiPropertyStatus status = case status of
  PropertyErrored (_, SolverTimedOut) -> mempty {numberTimedOut = 1}
  PropertyErrored _ -> mempty {numberErrored = 1}
  _
    | isVerified status -> mempty {numberVerified = 1}
    | otherwise -> mempty {numberFalsified = 1}

--------------------------------------------------------------------------------
-- Query event

-- JSON event types
newtype QuerySummary = QuerySummary
  { satisfied :: Bool
  }
  deriving (Generic)

instance ToJSON QuerySummary

--------------------------------------------------------------------------------
-- Implementations
--------------------------------------------------------------------------------
-- Shared state

type SharedState = (MultiPropertySummary, Int)

runSharedStateT :: (Monad m) => StateT SharedState m a -> m a
runSharedStateT fn = evalStateT fn (mempty, 0)

getAndClearMultiPropertyState :: (MonadState SharedState m) => m MultiPropertySummary
getAndClearMultiPropertyState = do
  summary <- gets fst
  put (mempty, 0)
  return summary

getAndClearPropertyState :: (MonadState SharedState m) => PropertyStatus -> m Int
getAndClearPropertyState result = do
  (summary, queryCount) <- get
  put (summary <> makeMultiPropertyStatus result, 0)
  return queryCount

incrementQueryCount :: (MonadState SharedState m) => m ()
incrementQueryCount = modify (second (+ 1))

--------------------------------------------------------------------------------
-- Text progress reporter
--------------------------------------------------------------------------------

newtype TextReporterT m a = TextReporterT
  { unTextReporterT :: (ReaderT (Maybe (ProgressBar ())) (StateT SharedState m)) a
  }
  deriving (Functor, Applicative, Monad)

getProgressBar :: Maybe (ProgressBar ()) -> ProgressBar ()
getProgressBar = \case
  Nothing -> developerError "progress bar not initialised"
  Just pb -> pb

runTextProgressReporterT :: (MonadStdIO m) => TextReporterT m a -> m a
runTextProgressReporterT fn = do
  programOutput "Verifying properties:"
  result <- runSharedStateT $ runReaderT (unTextReporterT fn) Nothing
  return result

mapTextReporterT ::
  (m (a, SharedState) -> n (b, SharedState)) ->
  TextReporterT m a ->
  TextReporterT n b
mapTextReporterT f m = TextReporterT (mapReaderT (mapStateT f) (unTextReporterT m))

instance (MonadReader a m) => MonadReader a (TextReporterT m) where
  ask = lift ask
  local = mapTextReporterT . local

instance (MonadStdIO m, MonadReader VerificationSettings m) => MonadProgressReporter (TextReporterT m) where
  reportMultiProperty name checkMultiPropertyFn = TextReporterT $ do
    result <- unTextReporterT checkMultiPropertyFn
    summary <- getAndClearMultiPropertyState
    textMultiPropertyComplete name summary
    return result

  reportProperty propertyAddress numberOfQueries checkPropertyFn = do
    settings <- ask
    TextReporterT $ do
      progressBar <- createProgressBar propertyAddress numberOfQueries
      result <- local (const $ Just progressBar) (unTextReporterT checkPropertyFn)
      queriesVerified <- getAndClearPropertyState result
      propertyCompleteText settings result numberOfQueries queriesVerified progressBar
      return result

  reportQuery _queryAddress checkQueryFn = TextReporterT $ do
    progressBar <- asks getProgressBar
    result <- unTextReporterT checkQueryFn
    incrementQueryCount
    textQueryComplete progressBar
    return result

instance MonadTrans TextReporterT where
  lift = TextReporterT . lift . lift

instance (MonadLogger m) => MonadLogger (TextReporterT m) where
  runCompilerPass = mapTextReporterT . runCompilerPass
  runCompileDecl = mapTextReporterT . runCompileDecl
  setCallDepth = lift . setCallDepth
  getCallDepth = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage = lift . logMessage
  logWarning = lift . logWarning

instance (MonadIO m) => MonadIO (TextReporterT m) where
  liftIO = lift . liftIO

instance (MonadStdIO m) => MonadStdIO (TextReporterT m) where
  writeStdout = lift . writeStdout
  writeStderr = lift . writeStderr

createProgressBar :: (MonadStdIO m) => PropertyAddress -> Int -> m (ProgressBar ())
createProgressBar (PropertyAddress name indices) numberOfQueries = do
  let propertyName = LazyText.fromStrict $ intercalate "!" (name : fmap (pack . show) indices)
  let style =
        defStyle
          { stylePrefix = msg ("  " <> propertyName),
            stylePostfix = exact <> msg " queries",
            styleWidth = ConstantWidth 80
          }
  let initialProgress = Progress 0 numberOfQueries ()
  liftIO $ hNewProgressBar stdout style 10 initialProgress

propertyCompleteText ::
  (MonadStdIO m) =>
  VerificationSettings ->
  PropertyStatus ->
  Int ->
  Int ->
  ProgressBar () ->
  m ()
propertyCompleteText VerificationSettings {..} propertyStatus numberOfQueries queriesVerified progressBar = do
  -- Close progress bar if human mode and incomplete
  when (queriesVerified < numberOfQueries) $
    closeProgressBar progressBar

  -- Print result to command line
  let nameOfSolver = pretty $ solverName solver
  let (verified, evidenceText) = case propertyStatus of
        PropertyCompleted status -> do
          case status of
            Trivial value -> (Just value, "(trivial)")
            NonTrivial (negated, queryResult) -> do
              let witnessText = if negated then "counterexample" else "witness"
              case queryResult of
                UnSAT -> (Just negated, nameOfSolver <+> "proved no" <+> witnessText <+> "exists")
                SAT Nothing -> (Just (not negated), nameOfSolver <+> "found no" <+> witnessText)
                SAT (Just assignment) -> do
                  let mainResult = nameOfSolver <+> "found a" <+> witnessText
                  let witnessResult = if noSatPrint then "" else line <> indent 6 (prettyUserVariableAssignment assignment)
                  (Just (not negated), mainResult <> witnessResult)
        PropertyErrored (_, err) -> do
          let cause = if isTimeoutError err then "timed out" else "errored"
          (Nothing, nameOfSolver <+> cause)
  writeStdoutLn (layoutAsText $ "    result: " <> pretty (statusSymbol verified) <+> "-" <+> evidenceText)

statusSymbol :: Maybe Bool -> String
statusSymbol verified = do
  let (colour, symbol) = case verified of
        Just True -> (Green, "🗸")
        Nothing -> (Yellow, "?")
        Just False -> (Red, "✗")
  setTextColour colour symbol

prettyUserVariableAssignment :: UserVariableAssignment -> Doc a
prettyUserVariableAssignment (UserVariableAssignment assignment) = do
  vsep (fmap prettyLine assignment)
  where
    prettyLine a = do
      case a of
        (var, TensorValue value) -> pretty var <> ":" <+> pretty value
        (var, RecordValue fields) ->
          pretty var
            <> ":"
            <> lineIndent (prettyRecordValueEntries (map (Data.Bifunctor.bimap pretty (prettyTensor pretty)) (NonEmpty.toList fields)))

closeProgressBar :: (MonadStdIO m) => ProgressBar () -> m ()
closeProgressBar _ = writeStdoutLn ""

textQueryComplete :: (MonadStdIO m) => ProgressBar () -> m ()
textQueryComplete progressBar = liftIO $ incProgress progressBar 1

textMultiPropertyComplete :: (MonadStdIO m) => Name -> MultiPropertySummary -> m ()
textMultiPropertyComplete name MultiPropertySummary {..} = do
  let results =
        [ ("verified", numberVerified),
          ("falsified", numberFalsified),
          ("timed-out", numberTimedOut),
          ("errored", numberErrored)
        ] ::
          [(String, Int)]
  let totalSize = sum $ fmap snd results

  when (totalSize > 1) $ do
    let maxTextLength = maximum $ fmap (length . fst) results
    let prettyResult (t, x) = fill (maxTextLength + 1) (pretty t <> ":") <+> pretty x <> "/" <> pretty totalSize
    let finalDoc = pretty name <> ":" <> line <> indent 4 (vsep (fmap prettyResult results))
    programOutput finalDoc

--------------------------------------------------------------------------------
-- JSON progress reporter
--------------------------------------------------------------------------------

newtype JSONReporterT m a = JSONReporterT
  { unJSONReporterT :: StateT SharedState m a
  }
  deriving (Functor, Applicative, Monad)

runJSONProgressReporterT :: (MonadStdIO m) => JSONReporterT m a -> m a
runJSONProgressReporterT fn = do
  outputEvent VerificationStart
  result <- runSharedStateT $ unJSONReporterT fn
  outputEvent VerificationFinish
  return result

mapJSONReporterT ::
  (m (a, SharedState) -> n (b, SharedState)) ->
  JSONReporterT m a ->
  JSONReporterT n b
mapJSONReporterT f m = JSONReporterT (mapStateT f (unJSONReporterT m))

instance (MonadReader a m) => MonadReader a (JSONReporterT m) where
  ask = lift ask
  local = mapJSONReporterT . local

instance (MonadStdIO m, MonadReader VerificationSettings m) => MonadProgressReporter (JSONReporterT m) where
  reportMultiProperty name checkMultiProperty = JSONReporterT $ do
    outputEvent $ MultiPropertyStart name
    result <- unJSONReporterT checkMultiProperty
    _summary <- getAndClearMultiPropertyState
    outputEvent $ MultiPropertyFinish name
    return result

  reportProperty propertyAddress _numberOfQueries checkPropertyFn = JSONReporterT $ do
    outputEvent $ PropertyStart propertyAddress
    status <- unJSONReporterT checkPropertyFn
    _ <- getAndClearPropertyState status
    case status of
      PropertyCompleted {} -> outputEvent $ PropertyFinish propertyAddress (isVerified status)
      PropertyErrored {} -> return ()
    return status

  reportQuery queryAddress checkQueryFn = JSONReporterT $ do
    outputEvent $ QueryStart queryAddress
    errorOrResult <- unJSONReporterT checkQueryFn
    case errorOrResult of
      Right result -> outputEvent $ QueryFinish queryAddress (querySatisified result)
      Left err -> do
        solverUsed <- asks solver
        outputEvent $ QueryError queryAddress (layoutAsString $ verificationErrorMessage $ convertVerificationError solverUsed queryAddress err)
    return errorOrResult

instance MonadTrans JSONReporterT where
  lift = JSONReporterT . lift

instance (MonadLogger m) => MonadLogger (JSONReporterT m) where
  runCompilerPass = mapJSONReporterT . runCompilerPass
  runCompileDecl = mapJSONReporterT . runCompileDecl
  setCallDepth = lift . setCallDepth
  getCallDepth = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage = lift . logMessage
  logWarning = lift . logWarning

instance (MonadStdIO m) => MonadStdIO (JSONReporterT m) where
  writeStdout = lift . writeStdout
  writeStderr = lift . writeStderr

instance (MonadIO m) => MonadIO (JSONReporterT m) where
  liftIO = lift . liftIO

outputEvent :: (MonadStdIO m) => ProgressEvent -> m ()
outputEvent event = writeStdoutLn $ pack $ ByteString.unpack $ encodePretty' prettyJSONConfig event

--------------------------------------------------------------------------------
-- JSON Events

-- Warning: changing these will break compatibility with Vehicle GUI.

data ProgressEvent
  = VerificationStart
  | MultiPropertyStart PropertyName
  | MultiPropertyFinish PropertyName
  | PropertyStart PropertyAddress
  | PropertyFinish PropertyAddress Bool
  | QueryStart QueryAddress
  | QueryFinish QueryAddress Bool
  | QueryError QueryAddress String
  | VerificationFinish
  deriving (Generic)

instance ToJSON ProgressEvent where
  toJSON = genericToJSON jsonOptions
