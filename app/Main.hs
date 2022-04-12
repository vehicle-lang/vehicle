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
  <> header "vehicle - a program for neural network verification" )

optionsParser :: Parser Options
optionsParser = Options
  <$> switch
      ( long "version"
     <> short 'V'
     <> help "Show version information." )
  <*> option auto
      ( long "output"
     <> help "Redirects output to the provided file. If no argument is provided will default to stdout."
     <> showDefault
     <> value Nothing
     <> metavar "FILENAME" )
  <*> option auto
      ( long "error"
     <> help "Redirects error to the provided file. If no argument is provided will default to stderr."
     <> showDefault
     <> value Nothing
     <> metavar "FILENAME" )
  <*> option auto
      ( long "log"
     <> help "Enables logging to the provided file. If no argument is provided will default to stdout."
     <> showDefault
     <> value Nothing
     <> metavar "FILENAME" )
  <*> commandParser

commandParser :: Parser Command
commandParser = hsubparser
    ( command "compile" (info (Compile <$> compileParser) compileDescription)
   <> command "verify"  (info (Verify  <$> verifyParser)  verifyDescription)
   <> command "check"   (info (Check   <$> checkParser)   checkDescription)
    )

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
  <*> strOption
      ( long "inputFile"
     <> short 'i'
     <> help "Input .vcl file."
     <> metavar "FILE" )
  <*> optional (strOption
      ( long "outputFile"
     <> short 'o'
     <> help "Output location for compiled file. Defaults to stdout if not provided."
     <> metavar "FILE" ))
  <*> resourceOptions
      ( long "network"
     <> short 'n'
     <> help "The name (as used in the Vehicle code) and path to a neural network."
     <> metavar "NAME:FILE")
  <*> resourceOptions
      ( long "dataset"
     <> short 'd'
     <> help "The name (as used in the Vehicle code) and path to a dataset."
     <> metavar "NAME:FILE")
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

resourceOptions :: Mod OptionFields (Text, FilePath) -> Parser (Map Text FilePath)
resourceOptions desc = Map.fromList <$> many (option (maybeReader readNL) desc)
  where
    readNL :: String -> Maybe (Text, FilePath)
    readNL s = case Text.splitOn (Text.pack ":") (Text.pack s) of
      [name, filepath] -> Just (name, Text.unpack filepath)
      _                -> Nothing

--------------------------------------------------------------------------------
-- Verify mode

verifyDescription :: InfoMod Command
verifyDescription = progDesc ("Verify the status of a Vehicle property," <>
                              "and write out the result to a proof cache.")

verifyParser :: Parser VerifyOptions
verifyParser = VerifyOptions
  <$> option auto
      ( long "verifier"
     <> short 'v'
     <> help "Verifier to use."
     <> metavar "TARGET" )
  <*> strOption
      ( long "inputFile"
     <> short 'i'
     <> help "Input .vcl file."
     <> metavar "FILE" )
  <*> resourceOptions
      ( long "network"
     <> short 'n'
     <> help "The name (as used in the Vehicle code) and path to a neural network."
     <> metavar "NAME:FILE" )
  <*> resourceOptions
      ( long "dataset"
    <> short 'd'
    <> help "The name (as used in the Vehicle code) and path to a dataset."
    <> metavar "NAME:FILE")
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