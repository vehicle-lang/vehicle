{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import Control.Monad.State (MonadState (..), StateT (..), evalStateT, gets, modify)
import Control.Monad.Trans (MonadTrans (..))
import Data.Aeson.Encode.Pretty (encodePretty')
import Data.Aeson.Types
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString.Lazy.Char8 qualified as ByteString (unpack)
import Data.Text (intercalate, pack)
import Data.Text.Lazy qualified as LazyText
import GHC.Generics (Generic)
import Prettyprinter (fill)
import System.Console.ANSI (Color (..))
import System.IO (stdout)
import System.ProgressBar
import Vehicle.Compile.Prelude
import Vehicle.Data.Code.BooleanExpr (MaybeTrivial (..))
import Vehicle.Data.Tensor (TensorIndices)
import Vehicle.Verify.Core
import Vehicle.Verify.Specification (QueryMetaData (..))
import Vehicle.Verify.Specification.Status
import Vehicle.Verify.Verifier.Core as Core

--------------------------------------------------------------------------------
-- Interface
--------------------------------------------------------------------------------
--
-- Mechanism for reporting events that happen during execution of a verification plan

class (Monad m) => MonadProgressReporter m where
  reportMultiProperty :: Name -> m () -> m ()
  reportProperty :: VerificationSettings -> PropertyAddress -> Int -> m PropertyStatus -> m PropertyStatus
  reportQuery :: QueryAddress -> m (Either VerifierError (QueryResult UserVariableAssignment)) -> m (Either VerifierError (QueryResult UserVariableAssignment))

instance (MonadProgressReporter m) => MonadProgressReporter (ReaderT a m) where
  reportMultiProperty n = mapReaderT (reportMultiProperty n)
  reportProperty s d i = mapReaderT (reportProperty s d i)
  reportQuery q = mapReaderT (reportQuery q)

-- If error reporting is doing funny things, I have my doubts about this implementation...
instance (MonadProgressReporter m) => MonadProgressReporter (ExceptT a m) where
  reportMultiProperty n = mapExceptT (>>= traverse (reportMultiProperty n . return))
  reportProperty s d i = mapExceptT (>>= traverse (reportProperty s d i . return))
  reportQuery q = mapExceptT (>>= traverse (reportQuery q . return))

data VerificationSettings = VerificationSettings
  { verifier :: Verifier,
    verifierExecutable :: VerifierExecutable,
    verifierExtraArgs :: [String],
    specificationCache :: FilePath,
    noSatPrint :: Bool
  }

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
  PropertyErrored (_, VerifierTimedOut) -> mempty {numberTimedOut = 1}
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

runTextProgressReporterT :: (MonadIO m) => TextReporterT m a -> m a
runTextProgressReporterT fn = do
  programOutput "Verifying properties:"
  result <- runSharedStateT $ runReaderT (unTextReporterT fn) Nothing
  return result

instance (MonadStdIO m) => MonadProgressReporter (TextReporterT m) where
  reportMultiProperty name checkMultiPropertyFn = TextReporterT $ do
    result <- unTextReporterT checkMultiPropertyFn
    summary <- getAndClearMultiPropertyState
    textMultiPropertyComplete name summary
    return result

  reportProperty settings propertyAddress numberOfQueries checkPropertyFn = TextReporterT $ do
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
  enterCompilerPass = lift . enterCompilerPass
  exitCompilerPass = lift exitCompilerPass
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
createProgressBar (PropertyAddress _ name indices) numberOfQueries = do
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
  let verifierName = pretty (verifierID verifier)
  let (verified, evidenceText) = case propertyStatus of
        PropertyCompleted status -> do
          case status of
            Trivial value -> (Just value, "(trivial)")
            NonTrivial (negated, queryResult) -> do
              let witnessText = if negated then "counterexample" else "witness"
              case queryResult of
                UnSAT -> (Just negated, verifierName <+> "proved no" <+> witnessText <+> "exists")
                SAT Nothing -> (Just (not negated), verifierName <+> "found no" <> witnessText)
                SAT (Just assignment) -> do
                  let mainResult = verifierName <+> "found a" <+> witnessText
                  let witnessResult = if noSatPrint then "" else line <> indent 6 (prettyUserVariableAssignment assignment)
                  (Just (not negated), mainResult <> witnessResult)
        PropertyErrored (_, err) -> do
          let cause = if isTimeoutError err then "timed out" else "errored"
          (Nothing, verifierName <+> cause)
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
  let prettyLine (var, value) = pretty var <> ":" <+> pretty value
  vsep (fmap prettyLine assignment)

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

instance (MonadStdIO m) => MonadProgressReporter (JSONReporterT m) where
  reportMultiProperty name checkMultiProperty = JSONReporterT $ do
    let startEvent = MultiPropertyStartEvent name
    outputEvent $ MultiPropertyStart startEvent
    result <- unJSONReporterT checkMultiProperty
    _summary <- getAndClearMultiPropertyState
    outputEvent $ MultiPropertyFinish startEvent
    return result

  reportProperty settings PropertyAddress {..} numberOfQueries checkPropertyFn = JSONReporterT $ do
    let startEvent = PropertyStartEvent propertyName propertyIndices numberOfQueries
    outputEvent $ PropertyStart startEvent
    result <- unJSONReporterT checkPropertyFn
    _ <- getAndClearPropertyState result
    outputEvent $ PropertyFinish startEvent (propertyStatusToPropertySummary settings result)
    return result

  reportQuery (PropertyAddress {..}, queryID) checkQueryFn = JSONReporterT $ do
    let startEvent = QueryStartEvent propertyName propertyIndices queryID
    outputEvent $ QueryStart startEvent
    errorOrResult <- unJSONReporterT checkQueryFn
    case errorOrResult of
      Left {} -> return ()
      Right result -> do
        let endEvent = QueryEndEvent (querySatisified result)
        outputEvent $ QueryFinish startEvent endEvent
    return errorOrResult

instance MonadTrans JSONReporterT where
  lift = JSONReporterT . lift

instance (MonadLogger m) => MonadLogger (JSONReporterT m) where
  enterCompilerPass = lift . enterCompilerPass
  exitCompilerPass = lift exitCompilerPass
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
  | MultiPropertyStart MultiPropertyStartEvent
  | PropertyStart PropertyStartEvent
  | QueryStart QueryStartEvent
  | QueryFinish QueryStartEvent QueryEndEvent
  | PropertyFinish PropertyStartEvent PropertyEndEvent
  | MultiPropertyFinish MultiPropertyStartEvent
  | VerificationFinish
  deriving (Generic)

instance ToJSON ProgressEvent

newtype MultiPropertyStartEvent = MultiPropertyStartEvent
  { propertyName :: Name
  }
  deriving (Generic)

instance ToJSON MultiPropertyStartEvent

data PropertyStartEvent = PropertyStartEvent
  { propertyName :: Name,
    propertyIndices :: TensorIndices,
    numberOfQueries :: Int
  }
  deriving (Generic)

instance ToJSON PropertyStartEvent

data QueryStartEvent = QueryStartEvent
  { propertyName :: Name,
    propertyIndices :: TensorIndices,
    queryID :: QueryID
  }
  deriving (Generic)

instance ToJSON QueryStartEvent

newtype QueryEndEvent = QueryEndEvent
  { satisfied :: Bool
  }
  deriving (Generic)

instance ToJSON QueryEndEvent

data PropertyEndEvent = PropertyEndEvent
  { verified :: Bool,
    erroredQueryID :: Maybe QueryID,
    errorMessage :: Maybe String
  }
  deriving (Generic)

instance ToJSON PropertyEndEvent

propertyStatusToPropertySummary :: VerificationSettings -> PropertyStatus -> PropertyEndEvent
propertyStatusToPropertySummary settings status = case status of
  PropertyCompleted {} -> PropertyEndEvent (isVerified status) Nothing Nothing
  PropertyErrored (queryData, err) -> do
    let address = queryAddress queryData
    let message = verificationErrorMessage $ convertVerificationError (verifier settings) address err
    PropertyEndEvent False (Just $ snd address) (Just $ layoutAsString message)
