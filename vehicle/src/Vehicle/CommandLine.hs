module Vehicle.CommandLine
  ( commandLineOptionsParserInfo,
  )
where

import Control.Applicative (Alternative (many), (<**>))
import Data.Map (Map)
import Data.Map qualified as Map (fromList)
import Data.Text (Text)
import Data.Text qualified as Text
import Options.Applicative
  ( InfoMod,
    Mod,
    OptionFields,
    Parser,
    ParserInfo,
    auto,
    command,
    fullDesc,
    header,
    help,
    helper,
    hsubparser,
    info,
    long,
    maybeReader,
    metavar,
    option,
    optional,
    progDesc,
    short,
    showDefault,
    strOption,
    switch,
    value,
  )
import Vehicle (GlobalOptions (..), ModeOptions (..), Options (..))
import Vehicle.Backend.Prelude (DifferentiableLogic, ITP, Target, TypingSystem (..))
import Vehicle.Check (CheckOptions (..))
import Vehicle.Compile (CompileOptions (..))
import Vehicle.CompileAndVerify (CompileAndVerifyOptions (..))
import Vehicle.Export (ExportOptions (..))
import Vehicle.Prelude
  ( LoggingLevel,
    defaultLoggingLevel,
    enumerate,
    loggingLevelHelp,
    supportedOptions,
    vehicleSpecificationFileExtension,
    vehicleVerificationPlanFileExtension,
  )
import Vehicle.TypeCheck (TypeCheckOptions (..))
import Vehicle.Verify (VerifierID, VerifyOptions (..))

--------------------------------------------------------------------------------
-- List of all options
--------------------------------------------------------------------------------
{-
--  - a
--  - b
proofCache  = Opt "c" "proofCache"
dataset     = Opt "d" "dataset"
declaration = Opt "e" "declaration"
--  - f
--  - g
--  - h
--  - i
--  - j
--  - k
verifierLocation = Opt "l" "verifierLocation"
moduleName       = Opt "m" "moduleName"
network          = Opt "n" "network"
outputFile       = Opt "o" "outputFile"
parameter        = Opt "p" "parameter"
--  - q
--  - r
specification    = Opt "s" "specification"
target           = Opt "t" "target"
--  - u
verifier         = Opt "v" "verifier"
--  - x
property         = Opt "y" "property"
--  - z
-}

commandLineOptionsParserInfo :: ParserInfo Options
commandLineOptionsParserInfo =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> header "Vehicle - a program for writing and checking neural network specifications"
    )

--------------------------------------------------------------------------------
-- Options common to all modes

optionsParser :: Parser Options
optionsParser =
  Options
    <$> globalOptionsParser
    <*> modeOptionsParser

--------------------------------------------------------------------------------
-- Global

globalOptionsParser :: Parser GlobalOptions
globalOptionsParser =
  GlobalOptions
    <$> showVersionParser
    <*> redirectLogsParser
    <*> loggingLevelParser

--------------------------------------------------------------------------------
-- Modes

modeOptionsParser :: Parser (Maybe ModeOptions)
modeOptionsParser =
  optional $
    hsubparser $
      command "typeCheck" typeCheckParserInfo
        <> command "compile" compileParserInfo
        <> command "verify" verifyParserInfo
        <> command "compileAndVerify" compileAndVerifyParserInfo
        <> command "check" checkParserInfo
        <> command "export" exportParserInfo

--------------------------------------------------------------------------------
-- Type-check mode

typeCheckDescription :: InfoMod ModeOptions
typeCheckDescription =
  progDesc $
    "typeCheck a " <> vehicleSpecificationFileExtension <> " file to an output target"

typeCheckParser :: Parser TypeCheckOptions
typeCheckParser =
  TypeCheckOptions
    <$> specificationParser
    <*> typeSystemParser
    <*> declarationParser

typeCheckParserInfo :: ParserInfo ModeOptions
typeCheckParserInfo = info (TypeCheck <$> typeCheckParser) typeCheckDescription

--------------------------------------------------------------------------------
-- Compile mode

compileDescription :: InfoMod ModeOptions
compileDescription =
  progDesc $
    "Compile a " <> vehicleSpecificationFileExtension <> " file to an output target"

compileParser :: Parser CompileOptions
compileParser =
  CompileOptions
    <$> compileTargetParser
    <*> specificationParser
    <*> declarationParser
    <*> networkParser
    <*> datasetParser
    <*> parameterParser
    <*> outputFileParser
    <*> modulePrefixOption
    <*> compileProofCacheParser

compileParserInfo :: ParserInfo ModeOptions
compileParserInfo = info (Compile <$> compileParser) compileDescription

--------------------------------------------------------------------------------
-- Verify mode

verifyDescription :: InfoMod ModeOptions
verifyDescription =
  progDesc
    "Verify the status of a Vehicle property, and write out the result to a \
    \ proof cache."

verifyParser :: Parser VerifyOptions
verifyParser =
  VerifyOptions
    <$> verificationPlanParser
    <*> verifierParser
    <*> verifierLocationParser
    <*> verifyProofCacheParser

verifyParserInfo :: ParserInfo ModeOptions
verifyParserInfo = info (Verify <$> verifyParser) verifyDescription

--------------------------------------------------------------------------------
-- Compile and verify mode

compileAndVerifyDescription :: InfoMod ModeOptions
compileAndVerifyDescription =
  progDesc
    "Verify the status of a Vehicle property, and write out the result to a \
    \ proof cache."

compileAndVerifyParser :: Parser CompileAndVerifyOptions
compileAndVerifyParser =
  CompileAndVerifyOptions
    <$> specificationParser
    <*> propertyParser
    <*> networkParser
    <*> datasetParser
    <*> parameterParser
    <*> verifierParser
    <*> verifierLocationParser
    <*> verifyProofCacheParser

compileAndVerifyParserInfo :: ParserInfo ModeOptions
compileAndVerifyParserInfo =
  info (CompileAndVerify <$> compileAndVerifyParser) compileAndVerifyDescription

--------------------------------------------------------------------------------
-- Check mode

checkDescription :: InfoMod ModeOptions
checkDescription =
  progDesc
    "Check the verification status of a Vehicle specification."

checkParser :: Parser CheckOptions
checkParser =
  CheckOptions
    <$> checkProofCacheParser

checkParserInfo :: ParserInfo ModeOptions
checkParserInfo = info (Check <$> checkParser) checkDescription

--------------------------------------------------------------------------------
-- Export mode

exportDescription :: InfoMod ModeOptions
exportDescription =
  progDesc
    "Export a Vehicle specification to an interactive theorem prover."

exportParser :: Parser ExportOptions
exportParser =
  ExportOptions
    <$> exportTargetParser
    <*> exportProofCacheParser
    <*> outputFileParser
    <*> modulePrefixOption

exportParserInfo :: ParserInfo ModeOptions
exportParserInfo = info (Export <$> exportParser) exportDescription

--------------------------------------------------------------------------------
-- Some shared option parsers

repeatedParameterHelp :: String
repeatedParameterHelp = "Can be provided multiple times."

allITPs :: [String]
allITPs = map show (enumerate @ITP)

allVerifiers :: [String]
allVerifiers = map show (enumerate @VerifierID)

allLossFunctionDLs :: [String]
allLossFunctionDLs = map show (enumerate @DifferentiableLogic)

allTasks :: [String]
allTasks = allVerifiers <> allITPs <> allLossFunctionDLs

allTypeSystems :: [String]
allTypeSystems = flip map (enumerate @TypingSystem) $ \case
  Standard -> "Standard"
  Linearity -> "Linearity - check whether quantified variables are used linearly in the specification."
  Polarity -> "Polarity - check whether alternating quantifiers are used in the specification."

resourceOption :: Mod OptionFields (Text, String) -> Parser (Map Text String)
resourceOption desc = Map.fromList <$> many (option (maybeReader readNL) desc)
  where
    readNL :: String -> Maybe (Text, String)
    readNL s = case Text.splitOn (Text.pack ":") (Text.pack s) of
      [name, val] -> Just (name, Text.unpack val)
      _ -> Nothing

showVersionParser :: Parser Bool
showVersionParser =
  switch $
    long "version"
      <> short 'V'
      <> help "Show version information."

redirectLogsParser :: Parser (Maybe FilePath)
redirectLogsParser =
  optional $
    strOption $
      long "redirectLogs"
        <> metavar "FILE"
        <> help
          "Redirects logs to the provided file. \
          \ If no argument is provided will default to stdout."

loggingLevelParser :: Parser LoggingLevel
loggingLevelParser =
  option auto $
    long "logging"
      <> value defaultLoggingLevel
      <> showDefault
      <> help loggingLevelHelp

verificationPlanParser :: Parser FilePath
verificationPlanParser =
  strOption $
    long "verificationPlan"
      <> short 'p'
      <> metavar "FILE"
      <> help ("The " <> vehicleVerificationPlanFileExtension <> " file containing the verification plan.")

typeSystemParser :: Parser TypingSystem
typeSystemParser =
  option auto $
    long "typeSystem"
      <> short 't'
      <> help ("Which typing system should be used. " <> supportedOptions allTypeSystems)
      <> value Standard

specificationParser :: Parser FilePath
specificationParser =
  strOption $
    long "specification"
      <> short 's'
      <> metavar "FILE"
      <> help ("The " <> vehicleSpecificationFileExtension <> " file containing the specification.")

networkParser :: Parser (Map Text FilePath)
networkParser =
  resourceOption $
    long "network"
      <> short 'n'
      <> metavar "NAME:FILE"
      <> help
        ( "Provide the implementation of a network declared in the \
          \ specification. Its value should consist of a colon-separated \
          \ pair of the name of the network in the specification and a file \
          \ path. "
            <> repeatedParameterHelp
        )

datasetParser :: Parser (Map Text FilePath)
datasetParser =
  resourceOption $
    long "dataset"
      <> short 'd'
      <> metavar "NAME:FILE"
      <> help
        ( "Provide a dataset declared in the specification. Its value should \
          \ consist of a colon-separated pair of the name of the dataset in \
          \ the specification and a file path. "
            <> repeatedParameterHelp
        )

parameterParser :: Parser (Map Text String)
parameterParser =
  resourceOption $
    long "parameter"
      <> short 'p'
      <> metavar "NAME:VALUE"
      <> help
        ( "Provide a value for a parameter referenced in the specification. Its value \
          \ should consist of a colon-separated pair of the name of the \
          \ parameter in the specification and its value. "
            <> repeatedParameterHelp
        )

modulePrefixOption :: Parser (Maybe String)
modulePrefixOption =
  optional $
    strOption $
      long "moduleName"
        <> short 'm'
        <> metavar "MODULENAME"
        <> help
          "Override the name of the exported ITP module. For example, \
          \compiling with 'Foo.Bar' will result in \
          \the Agda module with the internal name `Foo.Bar.agda`. If not \
          \provided then the name will default to the name of the output file."

outputFileParser :: Parser (Maybe String)
outputFileParser =
  optional $
    strOption $
      long "outputFile"
        <> short 'o'
        <> metavar "FILE"
        <> help "Output location for compiled file(s). Defaults to stdout if not provided."

propertyParser :: Parser [Text]
propertyParser =
  many
    ( strOption $
        long "property"
          <> short 'y'
          <> metavar "NAME"
          <> help
            ( "Property in the specification to verify. "
                <> repeatedParameterHelp
                <> " If none provided then all properties in the specification will be verified."
            )
    )

declarationParser :: Parser [Text]
declarationParser =
  many
    ( strOption $
        long "declaration"
          <> short 'e'
          <> metavar "NAME"
          <> help
            ( "Declarations in the specification to include during compilation. "
                <> repeatedParameterHelp
                <> " If not provided then all declarations in the specification will be compiled."
            )
    )

verifierParser :: Parser VerifierID
verifierParser =
  option auto $
    long "verifier"
      <> short 'v'
      <> metavar "VERIFIER"
      <> help ("Verifier to use. " <> supportedOptions allVerifiers)

verifierLocationParser :: Parser (Maybe FilePath)
verifierLocationParser =
  optional $
    strOption $
      long "verifierLocation"
        <> short 'l'
        <> metavar "FILE"
        <> help
          "Location of the executable for the verifier. \
          \If not provided then Vehicle will search for it in the PATH \
          \environment variable."

exportTargetParser :: Parser ITP
exportTargetParser =
  option auto $
    long "target"
      <> short 't'
      <> metavar "TARGET"
      <> help ("The target to export to. " <> supportedOptions allITPs)

compileTargetParser :: Parser Target
compileTargetParser =
  option auto $
    long "target"
      <> short 't'
      <> metavar "TARGET"
      <> help ("The target that the specification should be compiled to. " <> supportedOptions allTasks)

proofCacheOption :: Mod OptionFields String -> Parser String
proofCacheOption helpField =
  strOption $
    long "proofCache"
      <> short 'c'
      <> metavar "FILE"
      <> helpField

checkProofCacheParser :: Parser FilePath
checkProofCacheParser =
  proofCacheOption $
    help
      "The location of the proof cache \
      \ that can be used to check the verification status \
      \ of the specification. The proof cache can be generated via the \
      \ `vehicle verify` command."

exportProofCacheParser :: Parser FilePath
exportProofCacheParser =
  proofCacheOption $
    help
      "The location of the proof cache \
      \ that can be used to check the verification status \
      \ of the specification. Should be parsed when compiling to an ITP. "

verifyProofCacheParser :: Parser (Maybe FilePath)
verifyProofCacheParser =
  optional $
    proofCacheOption $
      help
        "The location that the proof-cache for the specification will be generated.\
        \ If not provided then no proof-cache will be created."

compileProofCacheParser :: Parser (Maybe FilePath)
compileProofCacheParser = optional exportProofCacheParser
