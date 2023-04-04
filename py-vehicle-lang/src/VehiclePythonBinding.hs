{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wall #-}

module VehiclePythonBinding where

import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr)
import Vehicle (mainWithArgsAndExitCode)

_unsafeVehicleMain :: CInt -> Ptr CString -> IO CInt
_unsafeVehicleMain argc argv = do
  -- Convert from `Ptr CString` to `[String]`
  args <- mapM peekCString =<< peekArray (fromIntegral argc) argv
  -- Call Vehicle.main and return the exit code
  exitCode <- mainWithArgsAndExitCode args
  return (fromIntegral exitCode)

foreign export ccall _unsafeVehicleMain :: CInt -> Ptr CString -> IO CInt
