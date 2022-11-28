module Vehicle.CommandLine
 ( commandLineOptionsParserInfo
 ) where

import Control.Applicative (Alternative (many), (<**>))
import Data.Map (Map)
import Data.Map qualified as Map (fromList)
import Data.Set (Set)
import Data.Set qualified as Set (fromList)
import Data.Text (Text)
import Data.Text qualified as Text
import Options.Applicative (InfoMod, Mod, OptionFields, Parser, ParserInfo,
                            auto, command, fullDesc, header, help, helper,
                            hsubparser, info, long, maybeReader, metavar,
                            option, optional, progDesc, short, showDefault,
                            strOption, switch, value)
import Vehicle (GlobalOptions (..), ModeOptions (..), Options (..))
import Vehicle.Backend.Prelude (Backend (..), DifferentiableLogic, ITP)
import Vehicle.Check (CheckOptions (..))
import Vehicle.Compile (CompileOptions (..))
import Vehicle.Export (ExportOptions (..))
import Vehicle.Prelude (LoggingLevel, defaultLoggingLevel, enumerate,
                        loggingLevelHelp, supportedOptions, vehicleFileExtension)
import Vehicle.Verify (VerifierIdentifier, VerifyOptions (..))

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
commandLineOptionsParserInfo = info (optionsParser <**> helper)
   ( fullDesc
  <> header "Vehicle - a program for writing and checking neural network specifications"
   )

--------------------------------------------------------------------------------
-- Options common to all modes

optionsParser :: Parser Options
optionsParser = Options
  <$> globalOptionsParser
  <*> modeOptionsParser

--------------------------------------------------------------------------------
-- Global

globalOptionsParser :: Parser GlobalOptions
globalOptionsParser = GlobalOptions
  <$> showVersionParser
  <*> redirectLogsParser
  <*> loggingLevelParser

--------------------------------------------------------------------------------
-- Modes

modeOptionsParser :: Parser (Maybe ModeOptions)
modeOptionsParser = optional $ hsubparser $
  command "compile" (info (Compile <$> compileParser) compileDescription) <>
  command "verify"  (info (Verify  <$> verifyParser)  verifyDescription)  <>
  command "check"   (info (Check   <$> checkParser)   checkDescription)   <>
  command "export"  (info (Export  <$> exportParser)  exportDescription)

--------------------------------------------------------------------------------
-- Compile mode

compileDescription :: InfoMod ModeOptions
compileDescription = progDesc $
  "Compile a " <> vehicleFileExtension <> " file to an output target"

compileParser :: Parser CompileOptions
compileParser = CompileOptions
  <$> compilationTargetParser
  <*> specificationParser
  <*> declarationParser
  <*> networkParser
  <*> datasetParser
  <*> parameterParser
  <*> outputFileParser
  <*> modulePrefixOption
  <*> compileProofCacheParser

--------------------------------------------------------------------------------
-- Verify mode

verifyDescription :: InfoMod ModeOptions
verifyDescription = progDesc
  "Verify the status of a Vehicle property, and write out the result to a \
  \ proof cache."

verifyParser :: Parser VerifyOptions
verifyParser = VerifyOptions
  <$> specificationParser
  <*> propertyParser
  <*> networkParser
  <*> datasetParser
  <*> parameterParser
  <*> verifierParser
  <*> verifierLocationParser
  <*> verifyProofCacheParser

--------------------------------------------------------------------------------
-- Check mode

checkDescription :: InfoMod ModeOptions
checkDescription = progDesc
  "Check the verification status of a Vehicle specification."

checkParser :: Parser CheckOptions
checkParser = CheckOptions
 <$> checkProofCacheParser

--------------------------------------------------------------------------------
-- Export mode

exportDescription :: InfoMod ModeOptions
exportDescription = progDesc
  "Export a Vehicle specification to an interactive theorem prover."

exportParser :: Parser ExportOptions
exportParser = ExportOptions
  <$> exportTargetParser
  <*> exportProofCacheParser
  <*> outputFileParser
  <*> modulePrefixOption

--------------------------------------------------------------------------------
-- Some shared option parsers

repeatedParameterHelp :: String
repeatedParameterHelp = "Can be provided multiple times."

allITPs :: [String]
allITPs = map show (enumerate @ITP)

allVerifiers :: [String]
allVerifiers = map show (enumerate @VerifierIdentifier)

allLossFunctionDLs :: [String]
allLossFunctionDLs = map show (enumerate @DifferentiableLogic)

allBackends :: [String]
allBackends = allVerifiers <> allITPs <> allLossFunctionDLs

resourceOption :: Mod OptionFields (Text, String) -> Parser (Map Text String)
resourceOption desc = Map.fromList <$> many (option (maybeReader readNL) desc)
  where
  readNL :: String -> Maybe (Text, String)
  readNL s = case Text.splitOn (Text.pack ":") (Text.pack s) of
    [name, val] -> Just (name, Text.unpack val)
    _           -> Nothing

showVersionParser :: Parser Bool
showVersionParser = switch $
  long  "version" <>
  short 'V' <>
  help  "Show version information."

redirectLogsParser :: Parser (Maybe FilePath)
redirectLogsParser = optional $ strOption $
  long    "redirectLogs" <>
  metavar "FILE" <>
  help    "Redirects logs to the provided file. \
         \ If no argument is provided will default to stdout."

loggingLevelParser :: Parser LoggingLevel
loggingLevelParser = option auto $
  long "logging" <>
  value defaultLoggingLevel <>
  showDefault <>
  help loggingLevelHelp

specificationParser :: Parser FilePath
specificationParser = strOption $
  long    "specification" <>
  short   's' <>
  metavar "FILE" <>
  help    ("The " <> vehicleFileExtension <> " file containing the specification.")

networkParser :: Parser (Map Text FilePath)
networkParser = resourceOption $
  long    "network" <>
  short   'n' <>
  metavar "NAME:FILE" <>
  help   ("Provide the implementation of a network declared in the \
        \ specification. Its value should consist of a colon-separated \
        \ pair of the name of the network in the specification and a file \
        \ path. " <> repeatedParameterHelp)

datasetParser :: Parser (Map Text FilePath)
datasetParser = resourceOption $
  long    "dataset" <>
  short   'd' <>
  metavar "NAME:FILE" <>
  help   ("Provide a dataset declared in the specification. Its value should \
         \ consist of a colon-separated pair of the name of the dataset in \
         \ the specification and a file path. " <> repeatedParameterHelp)

parameterParser :: Parser (Map Text String)
parameterParser = resourceOption $
  long    "parameter"  <>
  short   'p'          <>
  metavar "NAME:VALUE" <>
  help   ("Provide a value for a parameter referenced in the specification. Its value \
         \ should consist of a colon-separated pair of the name of the \
         \ parameter in the specification and its value. " <> repeatedParameterHelp)

modulePrefixOption :: Parser (Maybe String)
modulePrefixOption = optional $ strOption $
  long    "moduleName" <>
  short   'm' <>
  metavar "MODULENAME" <>
  help    "Override the name of the exported ITP module. For example, \
          \compiling with 'Foo.Bar' will result in \
          \the Agda module with the internal name `Foo.Bar.agda`. If not \
          \provided then the name will default to the name of the output file."

outputFileParser :: Parser (Maybe String)
outputFileParser = optional $ strOption $
  long    "outputFile" <>
  short   'o' <>
  metavar "FILE" <>
  help    "Output location for compiled file(s). Defaults to stdout if not provided."

propertyParser :: Parser (Set Text)
propertyParser = Set.fromList <$> many (strOption $
  long    "property" <>
  short   'y' <>
  metavar "NAME" <>
  help    ("Property in the specification to verify. " <> repeatedParameterHelp <>
          " If none provided then all properties in the specification will be verified."))

declarationParser :: Parser (Set Text)
declarationParser = Set.fromList <$> many (strOption $
  long    "declaration" <>
  short   'e' <>
  metavar "NAME" <>
  help    ("Declarations in the specification to include during compilation. " <>
          repeatedParameterHelp <>
          " If not provided then all declarations in the specification will be compiled."))

verifierParser :: Parser VerifierIdentifier
verifierParser = option auto $
  long    "verifier" <>
  short   'v' <>
  metavar "VERIFIER" <>
  help    ("Verifier to use. " <> supportedOptions allVerifiers)

verifierLocationParser :: Parser (Maybe FilePath)
verifierLocationParser = optional $ strOption $
  long    "verifierLocation" <>
  short   'l' <>
  metavar "FILE" <>
  help    "Location of the executable for the verifier. \
           \If not provided then Vehicle will search for it in the PATH \
           \environment variable."

exportTargetParser :: Parser ITP
exportTargetParser = option auto $
  long    "target" <>
  short   't'      <>
  metavar "TARGET" <>
  help    ("The target to export to. " <> supportedOptions allITPs)

compilationTargetParser :: Parser Backend
compilationTargetParser = option auto $
  long    "target" <>
  short   't' <>
  metavar "TARGET" <>
  help    ("Compilation target. " <> supportedOptions allBackends)

proofCacheOption :: Mod OptionFields String -> Parser String
proofCacheOption helpField = strOption $
  long    "proofCache" <>
  short   'c' <>
  metavar "FILE" <>
  helpField

checkProofCacheParser :: Parser FilePath
checkProofCacheParser = proofCacheOption $
  help "The location of the proof cache \
      \ that can be used to check the verification status \
      \ of the specification. The proof cache can be generated via the \
      \ `vehicle verify` command."

exportProofCacheParser :: Parser FilePath
exportProofCacheParser = proofCacheOption $
  help "The location of the proof cache \
      \ that can be used to check the verification status \
      \ of the specification. Should be parsed when compiling to an ITP. "

verifyProofCacheParser :: Parser (Maybe FilePath)
verifyProofCacheParser = optional $ proofCacheOption $
  help "The location that the proof-cache for the specification will be generated.\
      \ If not provided then no proof-cache will be created."

compileProofCacheParser :: Parser (Maybe FilePath)
compileProofCacheParser = optional exportProofCacheParser
