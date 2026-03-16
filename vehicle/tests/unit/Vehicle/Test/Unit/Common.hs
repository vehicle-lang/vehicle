{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Test.Unit.Common where

import Control.Monad.Except (ExceptT)
import Debug.Trace (trace)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, testCase)
import Vehicle.Compile.Error (CompileError)
import Vehicle.Compile.Print.Error
  ( formatCompileError,
    logCompileError,
  )
import Vehicle.Prelude
  ( Pretty (pretty),
    developerError,
  )
import Vehicle.Prelude.Logging
import Vehicle.Prelude.Warning (groupWarnings)

--------------------------------------------------------------------------------
-- Test settings monad

unitTestCase :: String -> ExceptT CompileError (SilentLoggerT IO) Assertion -> TestTree
unitTestCase testName errorOrAssertionWithLogs =
  testCase testName (traceLogs errorOrAssertionWithLogs)
  where
    traceLogs :: ExceptT CompileError (SilentLoggerT IO) Assertion -> Assertion
    traceLogs e = do
      let e' = logCompileError e
      (v, warnings) <- runSilentLoggerT e'
      let result =
            if null warnings
              then v
              else trace (showCompileWarnings $ groupWarnings warnings) v
      case result of
        Left x -> developerError $ pretty $ formatCompileError x
        Right y -> y
