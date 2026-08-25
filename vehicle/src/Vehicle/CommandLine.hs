{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fewer imports" #-}
module Vehicle.CommandLine
  ( Options (..),
    GlobalOptions (..),
    ModeOptions (..),
    defaultGlobalOptions,
    commandLineOptionsParserInfo,
  )
where

import Data.Map (Map)
import Data.Map qualified as Map (fromList)
import Data.Text (Text)
import Data.Text qualified as Text
import Options.Applicative
import Vehicle.Backend.Prelude (BuiltinDifferentiableLogicID, DifferentiableLogicID, InteractiveTheoremProverID, LossFunctionMode (Training), SecondaryTypeSystem (..))
import Vehicle.Compile (CompileOptions (..), ITPOptions (..), LossOptions (..), QueryOptions (..))
import Vehicle.Export (ExportOptions (..))
import Vehicle.List (ListOptions (..))
import Vehicle.Prelude
  ( Doc,
    Pretty (..),
    enumerate,
    indent,
    layoutAsString,
    line,
    lineIndent,
    specificationFileExtension,
    vsep,
    (<+>),
  )
import Vehicle.Prelude.Logging
import Vehicle.TypeCheck (TypeCheckOptions (..))
import Vehicle.Validate (ValidateOptions (..))
import Vehicle.Verify (VerifyOptions (..))
import Vehicle.Verify.QueryFormat

--------------------------------------------------------------------------------
-- Options objects
--------------------------------------------------------------------------------

data Options = Options
  { globalOptions :: GlobalOptions,
    modeOptions :: Maybe ModeOptions
  }
  deriving (Show, Eq)

data GlobalOptions = GlobalOptions
  { version :: Bool,
    logFile :: Maybe FilePath,
    loggingLevel :: LoggingLevel,
    loggingTarget :: Maybe CompilerStack,
    noWarnings :: Bool,
    outputAsJSON :: OutputAsJSON
  }
  deriving (Show, Eq)

defaultGlobalOptions :: GlobalOptions
defaultGlobalOptions =
  GlobalOptions
    { version = False,
      logFile = Nothing,
      loggingLevel = defaultLoggingLevel,
      loggingTarget = Nothing,
      noWarnings = False,
      outputAsJSON = False
    }

data ModeOptions
  = TypeCheck TypeCheckOptions
  | Compile CompileOptions
  | Verify VerifyOptions
  | Validate ValidateOptions
  | Export ExportOptions
  | List ListOptions
  deriving (Show, Eq)

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
solver-location = Opt "l" "solver-location"
module-name       = Opt "m" "module-name"
network           = Opt "n" "network"
output            = Opt "o" "output"
parameter         = Opt "p" "parameter"
queries           = Opt "q" "queries"
--  - r
specification     = Opt "s" "specification"
target            = Opt "t" "target"
--  - u
solver            = Opt "v" "solver"
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
    <*> loggingTargetParser
    <*> noWarningsParser
    <*> outputAsJSONParser

--------------------------------------------------------------------------------
-- Modes

modeOptionsParser :: Parser (Maybe ModeOptions)
modeOptionsParser =
  optional $
    hsubparser $
      command "typecheck" typeCheckParserInfo
        <> command "compile" compileParserInfo
        <> command "verify" verifyParserInfo
        <> command "validate" validateParserInfo
        <> command "export" exportParserInfo
        <> command "list" listParserInfo

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
    <*> declarationParser

typeCheckParserInfo :: ParserInfo ModeOptions
typeCheckParserInfo = info (TypeCheck <$> typeCheckParser) typeCheckDescription

--------------------------------------------------------------------------------
-- List mode

listDescription :: InfoMod ModeOptions
listDescription =
  progDesc $
    "List entities for a "
      <> specificationFileExtension
      <> " specification file"
      <> "."

listParser :: Parser ListOptions
listParser =
  ListOptions
    <$> specificationParser
    <*> networkParser
    <*> datasetParser
    <*> parameterParser

listParserInfo :: ParserInfo ModeOptions
listParserInfo = info (List <$> listParser) listDescription

--------------------------------------------------------------------------------
-- Compile mode

compileDescription :: InfoMod ModeOptions
compileDescription =
  progDesc $
    "Compile a " <> specificationFileExtension <> " specification file."

compileParserInfo :: ParserInfo ModeOptions
compileParserInfo = info (Compile <$> compileParser) compileDescription

compileParser :: Parser CompileOptions
compileParser =
  hsubparser $
    command "loss" compileLossParserInfo
      <> command "queries" compileQueryParserInfo
      <> command "itp" compileITPParserInfo

--------------------------------------------------------------------------------
-- Compile loss mode

compileLossParserInfo :: ParserInfo CompileOptions
compileLossParserInfo = info (LossTarget <$> compileLossParser) compileLossDescription

compileLossDescription :: InfoMod CompileOptions
compileLossDescription =
  progDesc
    "Compile a specification to a loss function."

compileLossParser :: Parser LossOptions
compileLossParser =
  LossOptions
    <$> lossModeParser
    <*> lossLogicParser
    <*> specificationParser
    <*> declarationParser
    <*> outputParser

lossLogicParser :: Parser DifferentiableLogicID
lossLogicParser =
  option auto $
    long "logic"
      <> short 'l'
      <> metavar "LOGIC"
      <> helpDoc (Just ("The differentiable logic to export to." <+> supportedOptions allBuiltinDifferentiableLogics))

lossModeParser :: Parser LossFunctionMode
lossModeParser =
  option auto $
    long "lossMode"
      <> short 'm'
      <> metavar "MODE"
      <> value Training
      <> showDefault
      <> helpDoc (Just ("Sets the loss function for training or search." <+> supportedOptions allLossModes))

--------------------------------------------------------------------------------
-- Compile query mode

compileQueryParserInfo :: ParserInfo CompileOptions
compileQueryParserInfo = info (QueryTarget <$> compileQueryParser) compileQueryDescription

compileQueryDescription :: InfoMod CompileOptions
compileQueryDescription =
  progDesc
    "Compile a specification to VNN-LIB queries for a solver."

compileQueryParser :: Parser QueryOptions
compileQueryParser =
  QueryOptions
    <$> queryFormatParser
    <*> specificationParser
    <*> declarationParser
    <*> networkParser
    <*> datasetParser
    <*> parameterParser
    <*> outputParser
    <*> compileCacheParser

queryFormatParser :: Parser QueryFormatID
queryFormatParser =
  option auto $
    long "format"
      <> short 'f'
      <> metavar "FORMAT"
      <> helpDoc (Just ("The query format to export to." <+> supportedOptions allSolversFormats))

--------------------------------------------------------------------------------
-- Compile ITP mode

compileITPParserInfo :: ParserInfo CompileOptions
compileITPParserInfo = info (ITPTarget <$> compileITPParser) compileITPDescription

compileITPDescription :: InfoMod CompileOptions
compileITPDescription =
  progDesc
    "Compile a specification to interactive theorem prover code."

compileITPParser :: Parser ITPOptions
compileITPParser =
  ITPOptions
    <$> itpParser
    <*> specificationParser
    <*> declarationParser
    <*> networkParser
    <*> datasetParser
    <*> parameterParser
    <*> outputParser
    <*> modulePrefixOption
    <*> compileCacheParser
    <*> compileConstReals

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
    <*> solverParser
    <*> solverExtraArgsParser
    <*> noSatPrintParser

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
    <$> itpParser
    <*> exportCacheParser
    <*> outputParser
    <*> modulePrefixOption
    <*> compileConstReals

exportParserInfo :: ParserInfo ModeOptions
exportParserInfo = info (Export <$> exportParser) exportDescription

--------------------------------------------------------------------------------
-- Some shared option parsers

repeatedParameterHelp :: String
repeatedParameterHelp = "Can be provided multiple times."

allITPs :: [String]
allITPs = map show (enumerate @InteractiveTheoremProverID)

allSolversFormats :: [String]
allSolversFormats = map show (enumerate @QueryFormatID)

allLossModes :: [String]
allLossModes = map show (enumerate @LossFunctionMode)

allBuiltinDifferentiableLogics :: [String]
allBuiltinDifferentiableLogics = map show (enumerate @BuiltinDifferentiableLogicID)

allTypeSystems :: [Doc a]
allTypeSystems = flip map (zip [1 :: Int ..] (enumerate @SecondaryTypeSystem)) $ \(n, t) ->
  pretty n
    <> "." <+> pretty t <+> "-" <+> case t of
      PolarityTypes -> "check whether alternating quantifiers are used in the specification."
      LinearityTypes -> "check whether quantified variables are used linearly in the specification."
      DecidabilityTypes -> "check which booleans are decidable and which are undecidable in the context of Vehicle"

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
        <> help "Redirects logs to the provided file. If no argument is provided will default to stdout."

loggingLevelParser :: Parser LoggingLevel
loggingLevelParser =
  option auto $
    long "logging"
      <> value defaultLoggingLevel
      <> showDefault
      <> helpDoc (Just loggingLevelHelp)

loggingLevelHelp :: Doc a
loggingLevelHelp =
  "Sets the level of detail in the logs if the --log argument has been passed."
    <+> supportedOptions allLoggingLevels
    <> line

loggingTargetParser :: Parser (Maybe CompilerStack)
loggingTargetParser =
  optional $
    option auto $
      long "loggingPass"
        <> helpDoc (Just loggingPassHelp)

loggingPassHelp :: Doc a
loggingPassHelp =
  "Sets which compiler pass logging is enabled for."
    <+> supportedOptions allCompilerPasses

noWarningsParser :: Parser Bool
noWarningsParser = do
  switch $
    long "no-warnings"
      <> help "Suppress the printing of warnings."

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
            <> "or ii) a folder containing the queries and verification plan generated by "
            <> "a previous call to `vehicle compile queries`."
        )

typeSystemParser :: Parser (Maybe SecondaryTypeSystem)
typeSystemParser =
  optional $
    option auto $
      long "typeSystem"
        <> short 't'
        <> metavar "TYPE_SYSTEM"
        <> help
          ( "A secondary type system to be used."
              <> layoutAsString
                ( line
                    <> line
                    <> indent
                      2
                      (vsep allTypeSystems)
                )
          )

specificationParser :: Parser FilePath
specificationParser =
  strOption $
    long "specification"
      <> short 's'
      <> metavar "FILE"
      <> helpDoc (Just ("The " <> pretty specificationFileExtension <> " file containing the specification."))

networkParser :: Parser (Map Text FilePath)
networkParser =
  resourceOption $
    long "network"
      <> short 'n'
      <> metavar "NAME:FILE"
      <> help
        ( "Provide the implementation of a network declared in the "
            <> "specification. Its value should consist of a colon-separated "
            <> "pair of the name of the network in the specification and a file path. "
            <> repeatedParameterHelp
        )

datasetParser :: Parser (Map Text FilePath)
datasetParser =
  resourceOption $
    long "dataset"
      <> short 'd'
      <> metavar "NAME:FILE"
      <> help
        ( "Provide a dataset declared in the specification. Its value should "
            <> "consist of a colon-separated pair of the name of the dataset in "
            <> "the specification and a file path. "
            <> repeatedParameterHelp
        )

parameterParser :: Parser (Map Text String)
parameterParser =
  resourceOption $
    long "parameter"
      <> short 'p'
      <> metavar "NAME:VALUE"
      <> help
        ( "Provide a value for a parameter referenced in the specification. Its value "
            <> "should consist of a colon-separated pair of the name of the "
            <> "parameter in the specification and its value. "
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
          ( "Override the name of the exported ITP module. For example, "
              <> "compiling with 'Foo.Bar' will result in "
              <> "the Agda module with the internal name `Foo.Bar.agda`. If not "
              <> "provided then the name will default to the name of the output file."
          )

outputParser :: Parser (Maybe String)
outputParser =
  optional $
    strOption $
      long "output"
        <> short 'o'
        <> metavar "FILE"
        <> help "Output location for compiled file(s). Defaults to `stdout` if not provided."

outputAsJSONParser :: Parser OutputAsJSON
outputAsJSONParser =
  switch $
    long "json"
      <> short 'j'
      <> help "Output the program as JSON instead of text."

-- Don't advertise JSON capabilities if a release build.
#ifdef releaseBuild
      <> internal
#endif

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

solverParser :: Parser String
solverParser =
  strOption $
    long "solver"
      <> short 'v'
      <> metavar "FILE"
      <> helpDoc (Just "The solver to use. Can be either a path to a solver executable or the name of a solver executable available via the system PATH environment variable.")

solverExtraArgsParser :: Parser (Maybe String)
solverExtraArgsParser =
  optional $
    strOption $
      long "solver-args"
        <> short 'a'
        <> metavar "STRING"
        <> help
          "Extra arguments to pass through to the solver when verifying each query."

noSatPrintParser :: Parser Bool
noSatPrintParser = do
  switch $
    long "no-sat-print"
      <> help "Suppress the printing of witnesses and counter-examples found during verification."

itpParser :: Parser InteractiveTheoremProverID
itpParser =
  option auto $
    long "target"
      <> short 't'
      <> metavar "TARGET"
      <> helpDoc (Just ("The target to export to." <+> supportedOptions allITPs))

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
    help $
      "The location of the verification cache to validate. "
        <> " This is the folder generated via the "
        <> " `vehicle verify` command."

exportCacheParser :: Parser FilePath
exportCacheParser =
  cacheOption $
    help $
      "The location of the verification cache "
        <> "that can be used to check the verification status "
        <> "of the specification. Should be passed when compiling to an ITP. "

verifyCacheParser :: Parser (Maybe FilePath)
verifyCacheParser =
  optional $
    cacheOption $
      help $
        "The location that the verification cache for the specification will be generated. "
          <> "If not provided then no cache will be created."

compileCacheParser :: Parser (Maybe FilePath)
compileCacheParser = optional exportCacheParser

compileConstReals :: Parser Bool
compileConstReals =
  switch $
    long "constructive-reals"
      <> short 'r'
      <> helpDoc (Just "Use constructive reals instead of mathcomp reals for Rocq.")

supportedOptions :: [String] -> Doc a
supportedOptions opts = "Supported options: " <> lineIndent (vsep $ fmap (\v -> "*" <+> pretty v) opts)
