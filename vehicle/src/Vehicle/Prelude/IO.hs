module Vehicle.Prelude.IO
  ( specificationFileExtension,
    specificationCacheIndexFileExtension,
    propertyVerificationResultFileExtension,
    propertyVerificationPlanFileExtension,
    vehicleObjectFileExtension,
    vehicleLibraryExtension,
    removeFileIfExists,
    fatalError,
    fatalErrorAsJSON,
    programOutput,
    getVehiclePath,
    ExternalOutputFormat (..),
    MonadStdIO (..),
  )
where

import Control.Exception (catch, throwIO)
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Identity (IdentityT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Writer (WriterT)
import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.List (findIndex, isInfixOf)
import Data.Text (Text)
import Data.Version (Version)
import Prettyprinter (Doc)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Environment (getArgs, getEnvironment, lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPrint, stderr)
import System.IO.Error (isDoesNotExistError)
import System.Info (os)

--------------------------------------------------------------------------------
-- Streams

class (MonadIO m) => MonadStdIO m where
  writeStdout :: Text -> m ()
  writeStderr :: Text -> m ()

  writeStdoutLn :: Text -> m ()
  writeStdoutLn = writeStdout . (<> "\n")

  writeStderrLn :: Text -> m ()
  writeStderrLn = writeStderr . (<> "\n")

{-# SPECIALIZE writeStdout :: Text -> IO () #-}

{-# SPECIALIZE writeStdoutLn :: Text -> IO () #-}

{-# SPECIALIZE writeStderr :: Text -> IO () #-}

{-# SPECIALIZE writeStderrLn :: Text -> IO () #-}

instance (MonadStdIO m) => MonadStdIO (StateT s m) where
  writeStdout :: (MonadStdIO m) => Text -> StateT s m ()
  writeStdout = lift . writeStdout
  writeStderr :: (MonadStdIO m) => Text -> StateT s m ()
  writeStderr = lift . writeStderr

instance (MonadStdIO m) => MonadStdIO (ReaderT s m) where
  writeStdout :: (MonadStdIO m) => Text -> ReaderT s m ()
  writeStdout = lift . writeStdout
  writeStderr :: (MonadStdIO m) => Text -> ReaderT s m ()
  writeStderr = lift . writeStderr

instance (Monoid w, MonadStdIO m) => MonadStdIO (WriterT w m) where
  writeStdout :: (Monoid w, MonadStdIO m) => Text -> WriterT w m ()
  writeStdout = lift . writeStdout
  writeStderr :: (Monoid w, MonadStdIO m) => Text -> WriterT w m ()
  writeStderr = lift . writeStderr

instance (MonadStdIO m) => MonadStdIO (IdentityT m) where
  writeStdout = lift . writeStdout
  writeStderr = lift . writeStderr
  writeStdoutLn = lift . writeStdoutLn
  writeStderrLn = lift . writeStderrLn

instance (MonadStdIO m) => MonadStdIO (ExceptT e m) where
  writeStdout :: (MonadStdIO m) => Text -> ExceptT e m ()
  writeStdout = lift . writeStdout
  writeStderr :: (MonadStdIO m) => Text -> ExceptT e m ()
  writeStderr = lift . writeStderr

--------------------------------------------------------------------------------
-- Files

baseFileExtension :: String
baseFileExtension = ".vcl"

specificationFileExtension :: String
specificationFileExtension = baseFileExtension

specificationCacheIndexFileExtension :: String
specificationCacheIndexFileExtension = baseFileExtension <> "-cache-index"

propertyVerificationPlanFileExtension :: String
propertyVerificationPlanFileExtension = baseFileExtension <> "-plan"

propertyVerificationResultFileExtension :: String
propertyVerificationResultFileExtension = baseFileExtension <> "-result"

vehicleObjectFileExtension :: String
vehicleObjectFileExtension = baseFileExtension <> "o"

vehicleLibraryExtension :: String
vehicleLibraryExtension = baseFileExtension <> "lib"

--------------------------------------------------------------------------------
-- IO operations

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fileName = removeFile fileName `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e

fatalError :: (MonadIO m) => Doc a -> m b
fatalError message = liftIO $ do
  hPrint stderr message
  exitFailure

-- Function to check if the error is a syntax error
isSyntaxError :: String -> Bool
isSyntaxError s = "syntax error" `isInfixOf` s || "syntax error:" `isInfixOf` s

fatalErrorAsJSON :: String -> IO ()
fatalErrorAsJSON msg = do
  args <- getArgs -- Get command line arguments directly from the system
  let (file, origLine, origColumn) = extractLocationInfo msg
  -- Extract filename from args
  let fileName =
        case dropWhile (/= "-s") args of
          ("-s" : fname : _) -> fname -- Find the filename after -s
          _ ->
            case filter (\arg -> ".vcl" `isInfixOf` arg) args of
              (fname : _) -> fname -- Look for arguments ending with .vcl
              _ -> file -- Use the originally extracted filename

  -- Extract specific line and column numbers from syntax error messages
  let (lineNum, colNum) =
        if isSyntaxError msg
          then
            let syntaxLine = matchSyntaxErrorLine msg
                syntaxCol = matchSyntaxErrorColumn msg
             in if syntaxLine > 0 && syntaxCol > 0
                  then (syntaxLine, syntaxCol)
                  else
                    if origLine > 0 && origColumn > 0
                      then (origLine, origColumn)
                      else (1, 1) -- Default values
          else
            if origLine > 0 && origColumn > 0
              then (origLine, origColumn)
              else (1, 1) -- Default values
  let errProv =
        object
          [ "file" .= fileName,
            "range"
              .= object
                [ "start" .= object ["line" .= lineNum, "column" .= colNum],
                  "end" .= object ["line" .= lineNum, "column" .= (colNum + 1)]
                ]
          ]
  BLC.putStrLn $ encode $ object ["error" .= msg, "provenance" .= errProv]
  exitFailure

-- | Extract location information from error messages
-- Tries to match patterns like "Error in file 'filename' at Line X, Columns Y-Z"
-- or "syntax error at line X, column Y before ..." for syntax errors
extractLocationInfo :: String -> (String, Int, Int)
extractLocationInfo msg =
  case matchErrorFile msg of
    Just file ->
      let line = matchErrorLine msg
          column = matchErrorColumn msg
       in (file, line, column)
    Nothing ->
      -- Check if it's a syntax error
      if isSyntaxError msg
        then
          let line = matchSyntaxErrorLine msg
              column = matchSyntaxErrorColumn msg
              -- Extract file path from error context, typically in format error_examples/xx_filename.vcl
              filename = extractSyntaxErrorFilename msg
           in if line > 0 && column > 0
                then (filename, line, column)
                else (filename, 0, 0)
        else
          ("unknown", 0, 0)
  where
    -- Try to extract filename from syntax error message
    extractSyntaxErrorFilename :: String -> String
    extractSyntaxErrorFilename s =
      -- First try to find paths with .vcl extension
      let parts = words s
          possibleFiles = filter (\w -> ".vcl" `isInfixOf` w) parts
       in if not (null possibleFiles)
            then head possibleFiles -- Return first filename found
            else
              -- If not found, try to find error examples directory
              let dirMatches = filter (\w -> "error_examples/" `isInfixOf` w) parts
               in if not (null dirMatches)
                    then head dirMatches -- Return first directory found
                    else "unknown" -- Return "unknown" if nothing found

    -- Extract filename
    matchErrorFile :: String -> Maybe String
    matchErrorFile s =
      case dropWhile (/= '\'') s of
        [] -> Nothing
        _ : rest ->
          case span (/= '\'') rest of
            (file, _) -> Just file

    -- Extract line number
    matchErrorLine :: String -> Int
    matchErrorLine s =
      case dropWhile (/= 'L') s of
        [] -> 0
        'L' : 'i' : 'n' : 'e' : ' ' : rest ->
          case span isDigit (dropWhile (not . isDigit) rest) of
            (digits, _) -> if null digits then 0 else read digits
        _ -> matchErrorLine (drop 1 s)

    -- Extract column number
    matchErrorColumn :: String -> Int
    matchErrorColumn s =
      case dropWhile (/= 'C') s of
        [] -> 0
        'C' : 'o' : 'l' : 'u' : 'm' : 'n' : ' ' : rest ->
          case span isDigit (dropWhile (not . isDigit) rest) of
            (digits, _) -> if null digits then 0 else read digits
        'C' : 'o' : 'l' : 'u' : 'm' : 'n' : 's' : ' ' : rest ->
          case span isDigit (dropWhile (not . isDigit) rest) of
            (digits, _) -> if null digits then 0 else read digits
        _ -> matchErrorColumn (drop 1 s)

-- Extract line number from syntax error message
matchSyntaxErrorLine :: String -> Int
matchSyntaxErrorLine errMsg =
  let result =
        if "syntax error at line " `isInfixOf` errMsg
          then
            let parts = words errMsg
                lineIndex = findIndex (\w -> w == "line") parts
             in case lineIndex of
                  Just idx ->
                    if idx + 1 < length parts
                      then
                        let lineStr = filter (/= ',') $ parts !! (idx + 1)
                         in if all isDigit lineStr
                              then read lineStr
                              else 0
                      else 0
                  Nothing -> 0
          else 0
   in result

-- Extract column number from syntax error message
matchSyntaxErrorColumn :: String -> Int
matchSyntaxErrorColumn errMsg =
  let result =
        if "column " `isInfixOf` errMsg
          then
            let parts = words errMsg
                colIndex = findIndex (\w -> w == "column") parts
             in case colIndex of
                  Just idx ->
                    if idx + 1 < length parts
                      then
                        let colStr = filter (\c -> isDigit c || c == '.') $ parts !! (idx + 1)
                         in if all isDigit colStr
                              then read colStr
                              else 0
                      else 0
                  Nothing -> 0
          else 0
   in result

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

programOutput :: (MonadIO m) => Doc a -> m ()
programOutput message = liftIO $ print message

--------------------------------------------------------------------------------
-- Library utilities

vehiclePathVariable :: String
vehiclePathVariable = "VEHICLE_PATH"

fallbackVehiclePathVariable :: String
fallbackVehiclePathVariable = case os of
  -- Windows
  "mingw32" -> "APPDATA"
  -- All other systems
  _ -> "HOME"

getVehiclePath :: (MonadIO m) => m FilePath
getVehiclePath = do
  vehiclePathVar <- liftIO $ lookupEnv vehiclePathVariable
  vehiclePath <- case vehiclePathVar of
    Just dir -> return dir
    Nothing -> do
      homeDir <- liftIO $ lookupEnv fallbackVehiclePathVariable
      case homeDir of
        Just dir -> return (dir </> ".vehicle")
        Nothing -> do
          env <- liftIO getEnvironment
          error $
            "Could not find home directory via path variable "
              <> fallbackVehiclePathVariable
              <> ". But could find environment "
              <> "variables: "
              <> show env
  liftIO $ createDirectoryIfMissing False vehiclePath
  return vehiclePath

--------------------------------------------------------------------------------
-- Other

data ExternalOutputFormat = ExternalOutputFormat
  { formatName :: forall a. Doc a,
    formatVersion :: Maybe Version,
    commentToken :: forall a. Doc a,
    emptyLines :: Bool
  }
