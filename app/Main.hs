module Main where

import GHC.IO.Encoding (utf8, setLocaleEncoding)
import Options.Applicative
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Map qualified as Map (fromList)

import Vehicle (run, ModeOptions(..), Options(Options))
import Vehicle.Check (CheckOptions(..))
import Vehicle.Compile (CompileOptions(..))
import Vehicle.Verify (VerifyOptions(..))
import Vehicle.Export (ExportOptions(..))
import Data.Map (Map)

--------------------------------------------------------------------------------
-- Main function

main :: IO ()
main = do
  setLocaleEncoding utf8
  options <- execParser optionsParserInfo
  run options

--------------------------------------------------------------------------------
-- Options common to all modes

optionsParserInfo :: ParserInfo Options
optionsParserInfo = info (optionsParser <**> helper)
   ( fullDesc
  <> header "Vehicle - a program for writing and checking neural network specifications" )

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

resourceOption :: Mod OptionFields (Text, String) -> Parser (Map Text String)
resourceOption desc = Map.fromList <$> many (option (maybeReader readNL) desc)
  where
  readNL :: String -> Maybe (Text, String)
  readNL s = case Text.splitOn (Text.pack ":") (Text.pack s) of
    [name, value] -> Just (name, Text.unpack value)
    _             -> Nothing

specOption :: Parser FilePath
specOption = strOption
  ( long "specification"
  <> short 's'
  <> help "The .vcl file containing the specification."
  <> metavar "FILE" )

networkOption :: Parser (Map Text FilePath)
networkOption = resourceOption
  ( long "network"
  <> short 'n'
  <> help "Provide the implementation of a network declared in the \
          \ specification. Its value should consist of a colon-separated \
          \ pair of the name of the network in the specification and a file \
          \ path."
  <> metavar "NAME:FILE")

datasetOption :: Parser (Map Text FilePath)
datasetOption = resourceOption
  ( long "dataset"
  <> short 'd'
  <> help "Provide a dataset declared in the specification. Its value should \
          \ consist of a colon-separated pair of the name of the dataset in \
          \ the specification and a file path."
  <> metavar "NAME:FILE")

parameterOption :: Parser (Map Text String)
parameterOption = resourceOption
  ( long "parameter"
  <> short 'p'
  <> help "Provide a parameter referenced in the specification. Its value \
          \ should consist of a colon-separated pair of the name of the \
          \ parameter in the specification and its value."
  <> metavar "NAME:VALUE")

modulePrefixOption :: Parser (Maybe String)
modulePrefixOption = optional (strOption
  ( long "modulePrefix"
  <> short 'm'
  <> help "Prefix for the name of the exported ITP module. For example, \
          \compiling to 'Baz.agda' with a prefix of `Foo.Bar` will result in \
          \the Agda module with the name `Foo.Bar.Baz`."
  <> metavar "MODULENAME" ))

inputProofCacheOption :: Parser String
inputProofCacheOption = strOption
  ( long "proofCache"
  <> short 'p'
  <> help "The location of the proof cache \
          \ that can be used to check the verification status \
          \ of the specification. The proof cache can be generated via the \
          \ `vehicle verify` command."
  <> metavar "FILE" )

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
  <*> specOption
  <*> optional (strOption
      ( long "outputFile"
     <> short 'o'
     <> help "Output location for compiled file. Defaults to stdout if not provided."
     <> metavar "FILE" ))
  <*> networkOption
  <*> datasetOption
  <*> parameterOption
  <*> modulePrefixOption
  <*> optional inputProofCacheOption

--------------------------------------------------------------------------------
-- Verify mode

verifyDescription :: InfoMod ModeOptions
verifyDescription = progDesc
  "Verify the status of a Vehicle property, and write out the result to a \
  \ proof cache."

verifyParser :: Parser VerifyOptions
verifyParser = VerifyOptions
  <$> specOption
  <*> networkOption
  <*> datasetOption
  <*> parameterOption
  <*> option auto
      ( long "verifier"
     <> short 'v'
     <> help "Verifier to use."
     <> metavar "TARGET" )
  <*> optional (strOption
      ( long "proofCache"
     <> short 'c'
     <> help "Location to export the proof cache file for the Vehicle project."
     <> metavar "FILE" ))

--------------------------------------------------------------------------------
-- Check mode

checkDescription :: InfoMod ModeOptions
checkDescription = progDesc
  "Check the verification status of a Vehicle specification."

checkParser :: Parser CheckOptions
checkParser = CheckOptions
 <$> strOption
      ( long "proofCache"
     <> short 'p'
     <> help "The proof-cache for the specification."
     <> metavar "FILE" )

--------------------------------------------------------------------------------
-- Export mode

exportDescription :: InfoMod ModeOptions
exportDescription = progDesc
  "Export a Vehicle specification to an interactive theorem prover."

exportParser :: Parser ExportOptions
exportParser = ExportOptions
  <$> option auto
      ( long "itp"
     <> short 'i'
     <> help "Compilation target."
     <> metavar "TARGET" )
  <*> inputProofCacheOption
  <*> optional (strOption
      ( long "outputFile"
     <> short 'o'
     <> help "Output location for compiled file. Defaults to stdout if not provided."
     <> metavar "FILE" ))
  <*> modulePrefixOption