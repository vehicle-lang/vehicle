module Vehicle.CommandLine
  ( Options (..),
    GlobalOptions (..),
    ModeOptions (..),
    defaultGlobalOptions,
    commandLineOptionsParserInfo,
  )
where

import Control.Applicative (Alternative (many), (<**>))
import Data.List (delete)
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
    ReadM,
    auto,
    command,
    eitherReader,
    fullDesc,
    header,
    help,
    helper,
    hsubparser,
    info,
    internal,
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
import Vehicle.Backend.Prelude (DifferentiableLogicID, ITP, Target (..), TypingSystem (..), findTarget)
import Vehicle.Compile (CompileOptions (..))
import Vehicle.Export (ExportOptions (..))
import Vehicle.Prelude
  ( Doc,
    enumerate,
    indent,
    layoutAsString,
    line,
    specificationFileExtension,
    supportedOptions,
    vsep,
  )
import Vehicle.Prelude.Logging
import Vehicle.TypeCheck (TypeCheckOptions (..))
import Vehicle.Validate (ValidateOptions (..))
import Vehicle.Verify (VerifierID (..), VerifyOptions (..))
import Vehicle.Verify.QueryFormat

--------------------------------------------------------------------------------
-- Options objects
--------------------------------------------------------------------------------

data Options = Options
  { globalOptions :: GlobalOptions,
    modeOptions :: Maybe ModeOptions
  }
  deriving (Eq, Show)

data GlobalOptions = GlobalOptions
  { version :: Bool,
    logFile :: Maybe FilePath,
    loggingLevel :: LoggingLevel
  }
  deriving (Eq, Show)

defaultGlobalOptions :: GlobalOptions
defaultGlobalOptions =
  GlobalOptions
    { version = False,
      logFile = Nothing,
      loggingLevel = defaultLoggingLevel
    }

data ModeOptions
  = Check TypeCheckOptions
  | Compile CompileOptions
  | Verify VerifyOptions
  | Validate ValidateOptions
  | Export ExportOptions
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- List of all options
--------------------------------------------------------------------------------
{-
args        = Opt "a"
--  - b
cache       = Opt "c" "cache"
dataset     = Opt "d" "dataset"
declaration = Opt "e" "declaration"
--  - f
--  - g
--  - h
--  - i
--  - j
--  - k
verifier-location = Opt "l" "verifier-location"
module-name       = Opt "m" "module-name"
network           = Opt "n" "network"
output        = Opt "o" "output"
parameter         = Opt "p" "parameter"
queries           = Opt "q" "queries"
--  - r
specification     = Opt "s" "specification"
target            = Opt "t" "target"
--  - u
verifier          = Opt "v" "verifier"
--  - x
property          = Opt "y" "property"
--  - z
-}

commandLineOptionsParserInfo :: ParserInfo Options
commandLineOptionsParserInfo =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> header "Vehicle - a program for enforcing neural network specifications"
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
      command "check" typeCheckParserInfo
        <> command "compile" compileParserInfo
        <> command "verify" verifyParserInfo
        <> command "validate" validateParserInfo
        <> command "export" exportParserInfo

--------------------------------------------------------------------------------
-- Check mode

typeCheckDescription :: InfoMod ModeOptions
typeCheckDescription =
  progDesc $
    "Type-check a "
      <> specificationFileExtension
      <> " specification file"
      <> "."

typeCheckParser :: Parser TypeCheckOptions
typeCheckParser =
  TypeCheckOptions
    <$> specificationParser
    <*> typeSystemParser

typeCheckParserInfo :: ParserInfo ModeOptions
typeCheckParserInfo = info (Check <$> typeCheckParser) typeCheckDescription

--------------------------------------------------------------------------------
-- Compile mode

compileDescription :: InfoMod ModeOptions
compileDescription =
  progDesc $
    "Compile a " <> specificationFileExtension <> " specification file."

compileParser :: Parser CompileOptions
compileParser =
  CompileOptions
    <$> compileTargetParser
    <*> specificationParser
    <*> declarationParser
    <*> networkParser
    <*> datasetParser
    <*> parameterParser
    <*> outputParser
    <*> modulePrefixOption
    <*> compileCacheParser
    <*> outputAsJSONParser

compileParserInfo :: ParserInfo ModeOptions
compileParserInfo = info (Compile <$> compileParser) compileDescription

--------------------------------------------------------------------------------
-- Verify mode

verifyDescription :: InfoMod ModeOptions
verifyDescription =
  progDesc $
    "Verify whether properties in a "
      <> specificationFileExtension
      <> " specification file are true or false."

verifyParser :: Parser VerifyOptions
verifyParser =
  VerifyOptions
    <$> verifySpecificationParser
    <*> propertyParser
    <*> networkParser
    <*> datasetParser
    <*> parameterParser
    <*> verifyCacheParser
    <*> verifierParser
    <*> verifierLocationParser
    <*> verifierExtraArgsParser

verifyParserInfo :: ParserInfo ModeOptions
verifyParserInfo = info (Verify <$> verifyParser) verifyDescription

--------------------------------------------------------------------------------
-- Check mode

validateDescription :: InfoMod ModeOptions
validateDescription =
  progDesc
    "Validate a verification result to check whether it still holds."

validateParser :: Parser ValidateOptions
validateParser =
  ValidateOptions
    <$> validateCacheParser

validateParserInfo :: ParserInfo ModeOptions
validateParserInfo = info (Validate <$> validateParser) validateDescription

--------------------------------------------------------------------------------
-- Export mode

exportDescription :: InfoMod ModeOptions
exportDescription =
  progDesc $
    "Export a"
      <> specificationFileExtension
      <> " specification file to an interactive theorem prover."

exportParser :: Parser ExportOptions
exportParser =
  ExportOptions
    <$> exportTargetParser
    <*> exportCacheParser
    <*> outputParser
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
allVerifiers = map show (delete TestVerifier (enumerate @VerifierID))

allVerifiersFormats :: [String]
allVerifiersFormats = map show (enumerate @QueryFormatID)

allLossFunctionDLs :: [String]
allLossFunctionDLs = map show (enumerate @DifferentiableLogicID)

allTargets :: [String]
allTargets = allLossFunctionDLs <> allVerifiersFormats <> allITPs <> [show ExplicitVehicle]

allTypeSystems :: [Doc a]
allTypeSystems = flip map (enumerate @TypingSystem) $ \case
  Standard -> "i) Standard - check whether the types written in the specification are consistent."
  Polarity -> "ii) Polarity - check whether alternating quantifiers are used in the specification."
  Linearity -> "iii) Linearity - check whether quantified variables are used linearly in the specification."

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
      <> short 'v'
      <> help "Show version information."

redirectLogsParser :: Parser (Maybe FilePath)
redirectLogsParser =
  optional $
    strOption $
      long "redirect-logs"
        <> internal
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

verifySpecificationParser :: Parser FilePath
verifySpecificationParser =
  strOption $
    long "specification"
      <> short 's'
      <> metavar "FILE"
      <> help
        ( "Either: i) a "
            <> specificationFileExtension
            <> " file containing the specification "
            <> "or ii) a folder containing the queries and verification plan generated by"
            <> "a previous call to `vehicle compile`."
        )

typeSystemParser :: Parser TypingSystem
typeSystemParser =
  option auto $
    long "typeSystem"
      <> short 't'
      <> help
        ( "Which typing system should be used. "
            <> layoutAsString
              ( line
                  <> line
                  <> indent
                    2
                    ( vsep allTypeSystems
                    )
              )
        )
      <> value Standard

specificationParser :: Parser FilePath
specificationParser =
  strOption $
    long "specification"
      <> short 's'
      <> metavar "FILE"
      <> help ("The " <> specificationFileExtension <> " file containing the specification.")

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
      long "module-name"
        <> short 'm'
        <> metavar "MODULENAME"
        <> help
          "Override the name of the exported ITP module. For example, \
          \compiling with 'Foo.Bar' will result in \
          \the Agda module with the internal name `Foo.Bar.agda`. If not \
          \provided then the name will default to the name of the output file."

outputParser :: Parser (Maybe String)
outputParser =
  optional $
    strOption $
      long "output"
        <> short 'o'
        <> metavar "FILE"
        <> help "Output location for compiled file(s). Defaults to stdout if not provided."

outputAsJSONParser :: Parser Bool
outputAsJSONParser =
  switch $
    long "json"
      <> short 'j'
      <> help "Output the program as JSON instead of text."
      <> internal

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
      long "verifier-location"
        <> short 'l'
        <> metavar "FILE"
        <> help
          "Location of the executable for the verifier. \
          \If not provided then Vehicle will search for it in the PATH \
          \environment variable."

verifierExtraArgsParser :: Parser (Maybe String)
verifierExtraArgsParser =
  optional $
    strOption $
      long "verifier-args"
        <> short 'a'
        <> metavar "STRING"
        <> help
          "Extra arguments to pass through to the verifier when verifying each query."

exportTargetParser :: Parser ITP
exportTargetParser =
  option auto $
    long "target"
      <> short 't'
      <> metavar "TARGET"
      <> help ("The target to export to. " <> supportedOptions allITPs)

compileTargetParser :: Parser Target
compileTargetParser =
  option targetReader $
    long "target"
      <> short 't'
      <> metavar "TARGET"
      <> help ("The target that the specification should be compiled to. " <> supportedOptions allTargets <> ".")
  where
    targetReader :: ReadM Target
    targetReader = eitherReader $ \s -> case findTarget s of
      Nothing -> Left ("Could not parse 'target' value '" <> s <> "'")
      Just t -> Right t

cacheOption :: Mod OptionFields String -> Parser String
cacheOption helpField =
  strOption $
    long "cache"
      <> short 'c'
      <> metavar "FILE"
      <> helpField

validateCacheParser :: Parser FilePath
validateCacheParser =
  cacheOption $
    help
      "The location of the verification cache to validate. \
      \ This is the folder generated via the \
      \ `vehicle verify` command."

exportCacheParser :: Parser FilePath
exportCacheParser =
  cacheOption $
    help
      "The location of the verification cache \
      \ that can be used to check the verification status \
      \ of the specification. Should be passed when compiling to an ITP. "

verifyCacheParser :: Parser (Maybe FilePath)
verifyCacheParser =
  optional $
    cacheOption $
      help
        "The location that the verification cache for the specification will be generated.\
        \ If not provided then no cache will be created."

compileCacheParser :: Parser (Maybe FilePath)
compileCacheParser = optional exportCacheParser
