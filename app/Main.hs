{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import GHC.IO.Encoding (utf8, setLocaleEncoding)
import Options.Applicative
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Map qualified as Map (fromList)

import Vehicle (run, Command(..), Options(Options))
import Vehicle.NeuralNetwork (NetworkLocations)
import Vehicle.Check (CheckOptions(..))
import Vehicle.Compile (CompileOptions(..))
import Vehicle.Verify (VerifyOptions(..))

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
      ( long "log-file"
     <> help "Enables logging to the provided file. If no argument is provided will output to stdout."
     <> showDefault
     <> value Nothing
     <> metavar "FILENAME" )
  <*> option auto
      ( long "error-file"
     <> help "Redirects error to the provided file. If no argument is provided will output to stderr."
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
  <$> strOption
      ( long "inputFile"
     <> short 'i'
     <> help "Input .vcl file."
     <> metavar "FILE" )
  <*> optional (strOption
      ( long "outputFile"
     <> short 'o'
     <> help "Output location for compiled file. Defaults to stdout if not provided."
     <> metavar "FILE" ))
  <*> option auto
      ( long "target"
     <> short 't'
     <> help "Compilation target."
     <> metavar "TARGET" )
  <*> strOption
      ( long "moduleName"
     <> short 'm'
     <> help "The name of the module."
     <> metavar "MODULENAME" )
  <*> networkOptions
       ( long "network"
      <> short 'n'
      <> help "The name (as used in the Vehicle code) and path to a neural network."
      <> metavar "NAME:FILE" )

networkOptions :: Mod OptionFields (Text, FilePath) -> Parser NetworkLocations
networkOptions desc = Map.fromList <$> many (option (maybeReader readNL) desc)
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
  <$> strOption
      ( long "inputFile"
     <> short 'i'
     <> help "Input .vcl file."
     <> metavar "FILE" )
  <*> option auto
      ( long "verifier"
     <> short 'v'
     <> help "Verifier to use."
     <> metavar "TARGET" )
  <*> networkOptions
      ( long "network"
     <> short 'n'
     <> help "The name (as used in the Vehicle code) and path to a neural network."
     <> metavar "NETWORK" )
  <*> optional (strOption
      ( long "proofCache"
     <> short 'p'
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