module Vehicle.Test.CompileMode.Unit.CommandLine
  ( commandLineParserTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Options.Applicative

import Vehicle
import Vehicle.Test.Utils (unitTestCase)
import Vehicle.Check (CheckOptions(..))
import Vehicle.CommandLine (commandLineOptionsParserInfo)
import Vehicle.Prelude

commandLineParserTests :: TestTree
commandLineParserTests = testGroup "CommandLineParser"
  [ parserTest "redirectOutput"
      "vehicle --redirectOutput myLogs/test.txt" $
        Options
        { globalOptions = defaultGlobalOptions
          { outFile      = Just "myLogs/test.txt"
          }
        , modeOptions  = Nothing
        }

  , parserTest "redirectError"
      "vehicle --redirectError myLogs/test.txt" $
        Options
        { globalOptions = defaultGlobalOptions
          { errFile      = Just "myLogs/test.txt"
          }
        , modeOptions  = Nothing
        }

  , parserTest "redirectLogs"
      "vehicle --redirectLogs myLogs/test.txt" $
        Options
        { globalOptions = defaultGlobalOptions
          { logFile      = Just "myLogs/test.txt"
          }
        , modeOptions  = Nothing
        }

  , parserTest "logging"
      "vehicle --logging MinDetail" $
        Options
        { globalOptions = defaultGlobalOptions
          { loggingLevel = MinDetail
          }
        , modeOptions  = Nothing
        }

  , parserTest "checkMode"
    "vehicle check --proofCache mpc.vpcl" $
      Options
      { globalOptions = defaultGlobalOptions
      , modeOptions  = Just $ Check $ CheckOptions
        { proofCache = "mpc.vpcl"
        }
      }
  ]

parserTest :: String -> String -> Options -> TestTree
parserTest name command expected = testCase name $ do
  let args   = tail $ words command
  let result = execParserPure defaultPrefs commandLineOptionsParserInfo args

  case result of
    Failure failure      -> assertFailure (show failure)
    CompletionInvoked cr -> error "should not return CompletionInvoked in test case"

    Success actual       -> do
      let errorMessage = layoutAsString $
            "When parsing:" <> line <> line <>
            indent 2 (pretty command) <> line

      assertEqual errorMessage actual expected