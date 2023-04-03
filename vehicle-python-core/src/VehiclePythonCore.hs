{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wall #-}

module VehiclePythonCore where

import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Options.Applicative (defaultPrefs, execParserPure, handleParseResult)
import Vehicle (run)
import Vehicle.CommandLine (commandLineOptionsParserInfo)

defaultMain :: [String] -> IO ()
defaultMain args = do
  setLocaleEncoding utf8
  options <- handleParseResult $
    execParserPure defaultPrefs commandLineOptionsParserInfo args
  run options

hs_defaultMain :: CInt -> Ptr CString -> IO ()
hs_defaultMain argc argv =
  peekArray (fromIntegral argc) argv
    >>= mapM peekCString
    >>= defaultMain

foreign export ccall hs_defaultMain :: CInt -> Ptr CString -> IO ()
