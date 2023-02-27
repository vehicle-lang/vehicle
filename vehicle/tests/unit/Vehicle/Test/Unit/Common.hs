{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Test.Unit.Common where

import Control.Monad.Except (ExceptT)
import Data.Data (Proxy (..))
import Data.Tagged (Tagged (Tagged))
import Debug.Trace (trace)
import Test.Tasty (TestTree, askOption, includingOptions)
import Test.Tasty.HUnit (Assertion, testCase)
import Test.Tasty.Ingredients (Ingredient)
import Test.Tasty.Options (IsOption (..), OptionDescription (Option))
import Text.Read (readMaybe)
import Vehicle.Compile.Error (CompileError)
import Vehicle.Compile.Error.Message
  ( MeaningfulError (details),
    logCompileError,
  )
import Vehicle.Compile.Prelude
  ( LoggingLevel,
  )
import Vehicle.Prelude
  ( DelayedLoggerT,
    Pretty (pretty),
    defaultLoggingLevel,
    developerError,
    loggingLevelHelp,
    runDelayedLoggerT,
    showMessages,
  )

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

unitTestCase :: String -> ExceptT CompileError (DelayedLoggerT IO) Assertion -> TestTree
unitTestCase testName errorOrAssertionWithLogs =
  askOption $ \logLevel -> testCase testName (traceLogs logLevel errorOrAssertionWithLogs)
  where
    traceLogs :: LoggingLevel -> ExceptT CompileError (DelayedLoggerT IO) Assertion -> Assertion
    traceLogs logLevel e = do
      let e' = logCompileError e
      (v, logs) <- runDelayedLoggerT logLevel e'
      let result = if null logs then v else trace (showMessages logs) v
      case result of
        Left x -> developerError $ pretty $ details x
        Right y -> y
