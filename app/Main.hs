{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import GHC.IO.Encoding (utf8, setLocaleEncoding)
import Options.Applicative
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Map qualified as Map (fromList)

import Vehicle (run, Command(..), Options(Options))
import Vehicle.Check (CheckOptions(..))
import Vehicle.Compile (CompileOptions(..))
import Vehicle.Verify (VerifyOptions(..))
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
  <*> commandParser

commandParser :: Parser Command
commandParser = hsubparser
    ( command "compile" (info (Compile <$> compileParser) compileDescription)
   <> command "verify"  (info (Verify  <$> verifyParser)  verifyDescription)
   <> command "check"   (info (Check   <$> checkParser)   checkDescription)
    )

--------------------------------------------------------------------------------
-- Some shared option parsers

resourceOptions :: Mod OptionFields (Text, String) -> Parser (Map Text String)
resourceOptions desc = Map.fromList <$> many (option (maybeReader readNL) desc)
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
networkOption = resourceOptions
  ( long "network"
  <> short 'n'
  <> help "Provide the implementation of a network declared in the \
          \ specification. Its value should consist of a colon-separated \
          \ pair of the name of the network in the specification and a file \
          \ path."
  <> metavar "NAME:FILE")

datasetOption :: Parser (Map Text FilePath)
datasetOption = resourceOptions
  ( long "dataset"
  <> short 'd'
  <> help "Provide a dataset declared in the specification. Its value should \
          \ consist of a colon-separated pair of the name of the dataset in \
          \ the specification and a file path."
  <> metavar "NAME:FILE")

parameterOption :: Parser (Map Text Text)
parameterOption = fmap Text.pack <$> resourceOptions
  ( long "parameter"
  <> short 'p'
  <> help "Provide a parameter referenced in the specification. Its value \
          \ should consist of a colon-separated pair of the name of the \
          \ parameter in the specification and its value."
  <> metavar "NAME:VALUE")

--------------------------------------------------------------------------------
-- Compile mode

compileDescription :: InfoMod Command
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
  <*> optional (strOption
      ( long "modulePrefix"
     <> short 'm'
     <> help "Sometimes needed when compiling to ITP code. For example, compiling to \
              \ 'Baz.agda' with a prefix of `Foo.Bar` will result in the Agda module \
              \ with the name `Foo.Bar.Baz`."
     <> metavar "MODULENAME" ))
  <*> optional (strOption
      ( long "proofCache"
     <> short 'p'
     <> help "The location of the proof cache \
              \ that can be used to check the verification status \
              \ of the specification. The proof cache can be generated via the \
              \ `vehicle verify` command."
     <> metavar "FILE" ))

--------------------------------------------------------------------------------
-- Verify mode

verifyDescription :: InfoMod Command
verifyDescription = progDesc ("Verify the status of a Vehicle property," <>
                              "and write out the result to a proof cache.")

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
     <> help "The proof cache file for the Vehicle project."
     <> metavar "FILE" ))

--------------------------------------------------------------------------------
-- Check mode

checkDescription :: InfoMod Command
checkDescription = progDesc "Check the verification status of a Vehicle property."

checkParser :: Parser CheckOptions
checkParser = CheckOptions
 <$> strOption
      ( long "proofCache"
     <> short 'p'
     <> help "The proof cache file for the Vehicle project."
     <> metavar "FILE" )