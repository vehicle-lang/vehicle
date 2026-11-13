module Vehicle.Test.Unit.Compile.CommandLine
  ( commandLineParserTests,
  )
where

import Data.Map qualified as Map (fromList)
import Options.Applicative (ParserResult (..), defaultPrefs, execParserPure)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Vehicle.Backend.Prelude (InteractiveTheoremProverID (..))
import Vehicle.CommandLine
  ( GlobalOptions (..),
    ModeOptions (..),
    Options (..),
    commandLineOptionsParserInfo,
    defaultGlobalOptions,
  )
import Vehicle.Export (ExportOptions (..))
import Vehicle.List (ListOptions (..))
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

commandLineParserTests :: TestTree
commandLineParserTests =
  testGroup
    "CommandLineParser"
    [ noModeTests,
      checkModeTests,
      verifyTests,
      validateModeTests,
      listModeTests,
      exportModeTests
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
        "vehicle --logging MinDetail --no-warnings"
        $ Options
          { globalOptions =
              defaultGlobalOptions
                { loggingLevel = MinDetail,
                  noWarnings = True
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
        "vehicle typecheck \
        \--specification test/spec.vcl"
        $ Options
          { globalOptions = defaultGlobalOptions,
            modeOptions =
              Just $
                TypeCheck $
                  TypeCheckOptions
                    { specification = "test/spec.vcl",
                      secondaryTypeSystem = Nothing,
                      declarationsToCompile = mempty
                    }
          }
    ]

listModeTests :: TestTree
listModeTests =
  testGroup
    "listMode"
    [ parserTest
        "basic"
        "vehicle list \
        \--specification test/spec.vcl"
        $ Options
          { globalOptions = defaultGlobalOptions,
            modeOptions =
              Just $
                List $
                  ListOptions
                    { specification = "test/spec.vcl",
                      networkLocations = mempty,
                      datasetLocations = mempty,
                      parameterValues = mempty
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

exportModeTests :: TestTree
exportModeTests =
  testGroup
    "exportMode"
    [ parserTest
        "cache"
        "vehicle export \
        \--target Agda \
        \--cache local/outputFolder"
        $ Options
          { globalOptions = defaultGlobalOptions,
            modeOptions =
              Just $
                Export $
                  ExportOptions
                    { target = Agda,
                      specification = Nothing,
                      declarationsToCompile = mempty,
                      networkLocations = mempty,
                      datasetLocations = mempty,
                      parameterValues = mempty,
                      output = Nothing,
                      moduleName = Nothing,
                      verificationCache = Just "local/outputFolder",
                      constructiveReals = False
                    }
          },
      parserTest
        "specification"
        "vehicle export \
        \--target Rocq \
        \--specification test/spec.vcl \
        \--declaration property \
        \--network f:test/network.onnx \
        \--dataset d:test/dataset.idx \
        \--parameter p:1 \
        \--output test/spec.v"
        $ Options
          { globalOptions = defaultGlobalOptions,
            modeOptions =
              Just $
                Export $
                  ExportOptions
                    { target = Rocq,
                      specification = Just "test/spec.vcl",
                      declarationsToCompile = ["property"],
                      networkLocations = Map.fromList [("f", "test/network.onnx")],
                      datasetLocations = Map.fromList [("d", "test/dataset.idx")],
                      parameterValues = Map.fromList [("p", "1")],
                      output = Just "test/spec.v",
                      moduleName = Nothing,
                      verificationCache = Nothing,
                      constructiveReals = False
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
        \--solver bin/Marabou \
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
                      solverExecutable = "bin/Marabou",
                      verificationCache = Just "local/outputFolder",
                      solverExtraArgs = Nothing,
                      noSatPrint = False
                    }
          },
      parserTest
        "preCompileBasic"
        "vehicle verify \
        \--specification test/spec.vcl \
        \--network f:test/myNetwork.onnx \
        \--solver Marabou"
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
                      solverExecutable = "Marabou",
                      verificationCache = Nothing,
                      solverExtraArgs = Nothing,
                      noSatPrint = False
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
        \--solver Marabou \
        \--solver-args --verbose=True \
        \--no-sat-print"
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
                      solverExecutable = "Marabou",
                      verificationCache = Nothing,
                      solverExtraArgs = Just "--verbose=True",
                      noSatPrint = True
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
