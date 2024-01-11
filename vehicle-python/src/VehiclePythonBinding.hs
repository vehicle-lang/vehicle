{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module VehiclePythonBinding where

import Data.ByteString (useAsCString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr)
import Vehicle (mainWithArgsAndExitCode)
import Vehicle.Prelude (MonadStdIO (..))

foreign import ccall _unsafe_python_write_stdout :: CString -> IO ()

foreign import ccall _unsafe_python_write_stderr :: CString -> IO ()

instance MonadStdIO IO where
  writeStdout str = encodeUtf8 str `useAsCString` _unsafe_python_write_stdout
  writeStderr str = encodeUtf8 str `useAsCString` _unsafe_python_write_stderr

_unsafe_vehicle_main :: CInt -> Ptr CString -> IO CInt
_unsafe_vehicle_main argc argv = do
  -- Convert from `Ptr CString` to `[String]`
  args <- mapM peekCString =<< peekArray (fromIntegral argc) argv
  -- Call Vehicle.main and return the exit code
  exitCode <- mainWithArgsAndExitCode args
  return (fromIntegral exitCode)

foreign export ccall _unsafe_vehicle_main :: CInt -> Ptr CString -> IO CInt
