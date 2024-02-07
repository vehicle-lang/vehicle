module Vehicle.Test.Unit.Compile.CommandLine
  ( commandLineParserTests,
  )
where

import Data.Map qualified as Map (fromList)
import Options.Applicative (ParserResult (..), defaultPrefs, execParserPure)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Vehicle.Backend.Prelude (TypingSystem (..))
import Vehicle.CommandLine
  ( GlobalOptions (..),
    ModeOptions (..),
    Options (..),
    commandLineOptionsParserInfo,
    defaultGlobalOptions,
  )
import Vehicle.Prelude
  ( Pretty (pretty),
    developerError,
    indent,
    layoutAsString,
    line,
  )
import Vehicle.Prelude.Logging (LoggingLevel (..))
import Vehicle.TypeCheck (TypeCheckOptions (..))
import Vehicle.Validate (ValidateOptions (..))
import Vehicle.Verify (VerifyOptions (..))
import Vehicle.Verify.Verifier (VerifierID (..))

commandLineParserTests :: TestTree
commandLineParserTests =
  testGroup
    "CommandLineParser"
    [ noModeTests,
      checkModeTests,
      verifyTests,
      validateModeTests
    ]

noModeTests :: TestTree
noModeTests =
  testGroup
    "noMode"
    [ parserTest
        "redirectLogs"
        "vehicle --redirect-logs myLogs/test.txt"
        $ Options
          { globalOptions =
              defaultGlobalOptions
                { logFile = Just "myLogs/test.txt"
                },
            modeOptions = Nothing
          },
      parserTest
        "logging"
        "vehicle --logging MinDetail"
        $ Options
          { globalOptions =
              defaultGlobalOptions
                { loggingLevel = MinDetail
                },
            modeOptions = Nothing
          }
    ]

checkModeTests :: TestTree
checkModeTests =
  testGroup
    "checkMode"
    [ parserTest
        "basic"
        "vehicle check \
        \--specification test/spec.vcl"
        $ Options
          { globalOptions = defaultGlobalOptions,
            modeOptions =
              Just $
                Check $
                  TypeCheckOptions
                    { specification = "test/spec.vcl",
                      typingSystem = Standard
                    }
          }
    ]

validateModeTests :: TestTree
validateModeTests =
  testGroup
    "validateMode"
    [ parserTest
        "basic"
        "vehicle validate --cache local/outputFolder"
        $ Options
          { globalOptions = defaultGlobalOptions,
            modeOptions =
              Just $
                Validate $
                  ValidateOptions
                    { verificationCache = "local/outputFolder"
                    }
          }
    ]

verifyTests :: TestTree
verifyTests =
  testGroup
    "verifyMode"
    [ parserTest
        "basic"
        "vehicle verify \
        \--specification queries \
        \--verifier Marabou \
        \--verifier-location bin/Marabou \
        \--cache local/outputFolder"
        Options
          { globalOptions = defaultGlobalOptions,
            modeOptions =
              Just $
                Verify $
                  VerifyOptions
                    { specification = "queries",
                      properties = mempty,
                      networkLocations = mempty,
                      datasetLocations = mempty,
                      parameterValues = mempty,
                      verifierID = Marabou,
                      verifierLocation = Just "bin/Marabou",
                      verificationCache = Just "local/outputFolder"
                    }
          },
      parserTest
        "preCompileBasic"
        "vehicle verify \
        \--specification test/spec.vcl \
        \--network f:test/myNetwork.onnx \
        \--verifier Marabou"
        Options
          { globalOptions = defaultGlobalOptions,
            modeOptions =
              Just $
                Verify $
                  VerifyOptions
                    { specification = "test/spec.vcl",
                      properties = mempty,
                      networkLocations = Map.fromList [("f", "test/myNetwork.onnx")],
                      datasetLocations = mempty,
                      parameterValues = mempty,
                      verifierID = Marabou,
                      verifierLocation = Nothing,
                      verificationCache = Nothing
                    }
          },
      parserTest
        "preCompileComplex"
        "vehicle verify \
        \--specification test/spec.vcl \
        \--property p1 \
        \--property p2 \
        \--dataset d:test/myDataset.idx \
        \--network f1:test/myNetwork1.onnx \
        \--parameter p:7.3 \
        \--network f2:test/myNetwork2.onnx \
        \--verifier Marabou"
        Options
          { globalOptions = defaultGlobalOptions,
            modeOptions =
              Just $
                Verify $
                  VerifyOptions
                    { specification = "test/spec.vcl",
                      properties = ["p1", "p2"],
                      networkLocations = Map.fromList [("f1", "test/myNetwork1.onnx"), ("f2", "test/myNetwork2.onnx")],
                      datasetLocations = Map.fromList [("d", "test/myDataset.idx")],
                      parameterValues = Map.fromList [("p", "7.3")],
                      verifierID = Marabou,
                      verifierLocation = Nothing,
                      verificationCache = Nothing
                    }
          }
    ]

parserTest :: String -> String -> Options -> TestTree
parserTest name command expected = testCase name $ do
  let args = case words command of
        (_ : as) -> as
        _ -> developerError "Malformed command. Commands must start with 'vehicle'"
  let result = execParserPure defaultPrefs commandLineOptionsParserInfo args

  case result of
    Failure failure -> assertFailure (show failure)
    CompletionInvoked _cr -> error "should not return CompletionInvoked in test case"
    Success actual -> do
      let errorMessage =
            layoutAsString $
              "When parsing:"
                <> line
                <> line
                <> indent 2 (pretty command)
                <> line

      assertEqual errorMessage actual expected
