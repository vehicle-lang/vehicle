{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wall #-}

module VehiclePythonBinding where

import Data.ByteString (useAsCString)
import Data.Text.Encoding (encodeUtf8)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr)
import Vehicle (mainWithArgsAndExitCode)

foreign import ccall _unsafe_python_write_stdout :: CString -> IO ()

putOut :: Text -> IO ()
putOut text = encodeUtf8 text `useAsCString` _unsafe_python_write_stdout

foreign import ccall _unsafe_python_write_stderr :: CString -> IO ()

putErr :: Text -> IO ()
putErr text = encodeUtf8 text `useAsCString` _unsafe_python_write_stderr

_unsafe_vehicle_main :: CInt -> Ptr CString -> IO CInt
_unsafe_vehicle_main argc argv = do
  -- Convert from `Ptr CString` to `[String]`
  args <- mapM peekCString =<< peekArray (fromIntegral argc) argv
  -- Call Vehicle.main and return the exit code
  exitCode <- mainWithArgsAndExitCode args
  return (fromIntegral exitCode)

foreign export ccall _unsafe_vehicle_main :: CInt -> Ptr CString -> IO CInt
