module Main where

import GHC.IO.Encoding (utf8, setLocaleEncoding)
import Options.Applicative
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Map (Map)
import Data.Map qualified as Map (fromList)
import Data.Set (Set)
import Data.Set qualified as Set (fromList)

import Vehicle (run, ModeOptions(..), Options(Options))
import Vehicle.Check (CheckOptions(..))
import Vehicle.Compile (CompileOptions(..))
import Vehicle.Verify (VerifyOptions(..))
import Vehicle.Export (ExportOptions(..))

--------------------------------------------------------------------------------
-- Main function

main :: IO ()
main = do
  setLocaleEncoding utf8
  options <- execParser optionsParserInfo
  run options

{-
Short-options guide:
  - a
  - b
  - c -- proof cache
  - d -- dataset file
  - e
  - f
  - g
  - h
  - i
  - j
  - k
  - l -- verifier location
  - m -- Agda module name
  - n -- network file
  - o -- output file
  - p -- parameter value
  - q
  - r
  - s -- specification file
  - t -- compilation target
  - u
  - v -- verifier
  - x
  - y -- property
  - z
-}

--------------------------------------------------------------------------------
-- Options common to all modes

optionsParserInfo :: ParserInfo Options
optionsParserInfo = info (optionsParser <**> helper)
   ( fullDesc
  <> header "Vehicle - a program for writing and checking neural network specifications"
   )

optionsParser :: Parser Options
optionsParser = Options
  <$> switch
      ( long "version"
     <> short 'V'
     <> help "Show version information." )
  <*> option auto
      ( long "output"
     <> help "Redirects output to the provided file. \
             \ If no argument is provided will default to stdout."
     <> showDefault
     <> value Nothing
     <> metavar "FILENAME" )
  <*> option auto
      ( long "error"
     <> help "Redirects error to the provided file. \
             \ If no argument is provided will default to stderr."
     <> showDefault
     <> value Nothing
     <> metavar "FILENAME" )
  <*> option auto
      ( long "log"
     <> help "Enables logging to the provided file. \
             \ If no argument is provided will default to stdout."
     <> showDefault
     <> value Nothing
     <> metavar "FILENAME" )
  <*> option auto
      ( long "loggingLevel"
     <> help "Sets the level of detail in the logs if the --log argument has been passed.  \
             \ Ranges between 1 (minimal detail) to 3 (maximal detail)"
     <> showDefault
     <> value 1
     <> metavar "INT")
  <*> modeParser

modeParser :: Parser ModeOptions
modeParser = hsubparser
    ( command "compile" (info (Compile <$> compileParser) compileDescription)
   <> command "verify"  (info (Verify  <$> verifyParser)  verifyDescription)
   <> command "check"   (info (Check   <$> checkParser)   checkDescription)
   <> command "export"  (info (Export  <$> exportParser)  exportDescription)
    )

--------------------------------------------------------------------------------
-- Some shared option parsers

repeatedParameterHelp :: String
repeatedParameterHelp = "Can be provided multiple times."

resourceOption :: Mod OptionFields (Text, String) -> Parser (Map Text String)
resourceOption desc = Map.fromList <$> many (option (maybeReader readNL) desc)
  where
  readNL :: String -> Maybe (Text, String)
  readNL s = case Text.splitOn (Text.pack ":") (Text.pack s) of
    [name, value] -> Just (name, Text.unpack value)
    _             -> Nothing

specificationOption :: Parser FilePath
specificationOption = strOption
  ( long "specification"
  <> short 's'
  <> help "The .vcl file containing the specification."
  <> metavar "FILE" )

networkOption :: Parser (Map Text FilePath)
networkOption = resourceOption
  ( long "network"
  <> short 'n'
  <> help ("Provide the implementation of a network declared in the \
          \ specification. Its value should consist of a colon-separated \
          \ pair of the name of the network in the specification and a file \
          \ path. " <> repeatedParameterHelp)
  <> metavar "NAME:FILE")

datasetOption :: Parser (Map Text FilePath)
datasetOption = resourceOption
  ( long "dataset"
  <> short 'd'
  <> help ("Provide a dataset declared in the specification. Its value should \
          \ consist of a colon-separated pair of the name of the dataset in \
          \ the specification and a file path. " <> repeatedParameterHelp)
  <> metavar "NAME:FILE")

parameterOption :: Parser (Map Text String)
parameterOption = resourceOption
  ( long "parameter"
  <> short 'p'
  <> help ("Provide a value for a parameter referenced in the specification. Its value \
          \ should consist of a colon-separated pair of the name of the \
          \ parameter in the specification and its value. " <> repeatedParameterHelp)
  <> metavar "NAME:VALUE")

modulePrefixOption :: Parser (Maybe String)
modulePrefixOption = optional (strOption
  ( long "moduleName"
  <> short 'm'
  <> help "Override the name of the exported ITP module. For example, \
          \compiling with 'Foo.Bar' will result in \
          \the Agda module with the internal name `Foo.Bar.agda`. If not \
          \provided then the name will default to the name of the output file."
  <> metavar "MODULENAME" ))

proofCacheOption :: Mod OptionFields String -> Parser String
proofCacheOption helpField = strOption
  ( long "proofCache"
  <> short 'c'
  <> helpField
  <> metavar "FILE"
  )

outputFileOption :: Parser (Maybe String)
outputFileOption = optional $ strOption
  ( long "outputFile"
  <> short 'o'
  <> help "Output location for compiled file(s). Defaults to stdout if not provided."
  <> metavar "FILE"
  )

propertyOption :: Mod OptionFields Text -> Parser (Set Text)
propertyOption extraDesc = Set.fromList <$> many (option auto desc)
  where
    desc :: Mod OptionFields Text
    desc =
         long "property"
      <> short 'y'
      <> extraDesc
      <> metavar "NAME"

--------------------------------------------------------------------------------
-- Compile mode

compileDescription :: InfoMod ModeOptions
compileDescription = progDesc "Compile a .vcl file to an output target"

compileParser :: Parser CompileOptions
compileParser = CompileOptions
  <$> option auto
      ( long "target"
     <> short 't'
     <> help "Compilation target."
     <> metavar "TARGET" )
  <*> specificationOption
  <*> propertyOption
       ( help ("Property to include during compilation. " <> repeatedParameterHelp)
       )
  <*> outputFileOption
  <*> networkOption
  <*> datasetOption
  <*> parameterOption
  <*> modulePrefixOption
  <*> optional (proofCacheOption
       ( help "The location of the proof cache \
          \ that can be used to check the verification status \
          \ of the specification. The proof cache can be generated via the \
          \ `vehicle verify` command."
       ))

--------------------------------------------------------------------------------
-- Verify mode

verifyDescription :: InfoMod ModeOptions
verifyDescription = progDesc
  "Verify the status of a Vehicle property, and write out the result to a \
  \ proof cache."

verifyParser :: Parser VerifyOptions
verifyParser = VerifyOptions
  <$> specificationOption
  <*> propertyOption
       ( help ("Property in the specification to verify. " <> repeatedParameterHelp <>
               " If none provided then all properties in the file will be verified.")
       )
  <*> networkOption
  <*> datasetOption
  <*> parameterOption
  <*> option auto
      ( long "verifier"
     <> short 'v'
     <> help "Verifier to use."
     <> metavar "TARGET" )
  <*> optional (strOption
      ( long "verifierLocation"
     <> short 'l'
     <> help "Location of the executable for the verifier. \
             \If not provided then Vehicle will search for it in the PATH \
             \environment variable."
     <> metavar "FILE" ))
  <*> optional (proofCacheOption
      ( help "Location to create the proof cache for the verified result."
      ))

--------------------------------------------------------------------------------
-- Check mode

checkDescription :: InfoMod ModeOptions
checkDescription = progDesc
  "Check the verification status of a Vehicle specification."

checkParser :: Parser CheckOptions
checkParser = CheckOptions
 <$> proofCacheOption
      ( help "The proof-cache for the specification."
      )

--------------------------------------------------------------------------------
-- Export mode

exportDescription :: InfoMod ModeOptions
exportDescription = progDesc
  "Export a Vehicle specification to an interactive theorem prover."

exportParser :: Parser ExportOptions
exportParser = ExportOptions
  <$> option auto
      ( long "target"
     <> short 't'
     <> help "Compilation target."
     <> metavar "TARGET" )
  <*> proofCacheOption
      ( help "the proof cache containing the verification result"
      )
  <*> outputFileOption
  <*> modulePrefixOption