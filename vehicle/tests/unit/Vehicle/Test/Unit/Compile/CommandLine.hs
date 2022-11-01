module Vehicle.Test.Unit.Compile.CommandLine
  ( commandLineParserTests
  ) where

import Data.Map qualified as Map (fromList)
import Data.Set qualified as Set
import Options.Applicative (ParserResult (CompletionInvoked, Failure, Success),
                            defaultPrefs, execParserPure)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Vehicle (GlobalOptions (errFile, logFile, loggingLevel, outFile),
                ModeOptions (Check, Verify), Options (..), defaultGlobalOptions)
import Vehicle.Check (CheckOptions (..))
import Vehicle.CommandLine (commandLineOptionsParserInfo)
import Vehicle.Prelude (LoggingLevel (MinDetail), Pretty (pretty), indent,
                        layoutAsString, line)
import Vehicle.Verify (VerifyOptions (..))
import Vehicle.Verify.Core (VerifierIdentifier (..))

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

  , parserTest "verifyMode1"
    "vehicle verify \
      \--specification test/spec.vcl \
      \--network f:test/myNetwork.onnx \
      \--verifier Marabou"
    Options
      { globalOptions = defaultGlobalOptions
      , modeOptions  = Just $ Verify $ VerifyOptions
        { specification    = "test/spec.vcl"
        , properties       = mempty
        , networkLocations = Map.fromList [("f", "test/myNetwork.onnx")]
        , datasetLocations = mempty
        , parameterValues  = mempty
        , verifier         = Marabou
        , verifierLocation = Nothing
        , proofCache       = Nothing
        }
      }

  , parserTest "verifyMode2"
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
      { globalOptions = defaultGlobalOptions
      , modeOptions  = Just $ Verify $ VerifyOptions
        { specification    = "test/spec.vcl"
        , properties       = Set.fromList ["p1", "p2"]
        , networkLocations = Map.fromList [("f1", "test/myNetwork1.onnx"),("f2", "test/myNetwork2.onnx")]
        , datasetLocations = Map.fromList [("d", "test/myDataset.idx")]
        , parameterValues  = Map.fromList [("p", "7.3")]
        , verifier         = Marabou
        , verifierLocation = Nothing
        , proofCache       = Nothing
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
