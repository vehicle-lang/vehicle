{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Test.Unit.Common where

import Control.Monad.Except (ExceptT)
import Data.Data (Proxy (..))
import Data.Tagged (Tagged (Tagged))
import Debug.Trace (trace)
import Test.Tasty (TestTree, includingOptions)
import Test.Tasty.HUnit (Assertion, testCase)
import Test.Tasty.Ingredients (Ingredient)
import Test.Tasty.Options (IsOption (..), OptionDescription (Option))
import Text.Read (readMaybe)
import Vehicle.Compile.Error (CompileError)
import Vehicle.Compile.Error.Message
  ( MeaningfulError (details),
    logCompileError,
  )
import Vehicle.Prelude
  ( Pretty (pretty),
    developerError,
  )
import Vehicle.Prelude.Logging
import Vehicle.Prelude.Warning (groupWarnings)

vehicleLoggingIngredient :: Ingredient
vehicleLoggingIngredient =
  includingOptions [Option (Proxy :: Proxy LoggingLevel)]

instance IsOption LoggingLevel where
  defaultValue :: LoggingLevel
  defaultValue = defaultLoggingLevel

  parseValue :: String -> Maybe LoggingLevel
  parseValue = readMaybe

  optionName :: Tagged LoggingLevel String
  optionName = Tagged "vehicle-logging"

  optionHelp :: Tagged LoggingLevel String
  optionHelp = Tagged loggingLevelHelp

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
        Left x -> developerError $ pretty $ details x
        Right y -> y
