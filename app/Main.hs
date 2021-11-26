{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import GHC.IO.Encoding (utf8, setLocaleEncoding)
import Options.Applicative

import Vehicle (run, Command(..), Options(Options))
import Vehicle.Compile (CompileOptions(..))
import Vehicle.Check (CheckOptions(..))

--------------------------------------------------------------------------------
-- Main function

main :: IO ()
main = do
  setLocaleEncoding utf8
  options <- execParser optionsParserInfo
  run options

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
  <*> commandParser

commandParser :: Parser Command
commandParser = hsubparser
    ( command "compile" (info (Compile <$> compileParser) compileDescription)
   <> command "check"   (info (Check   <$> checkParser)   checkDescription)
    )

compileDescription :: InfoMod Command
compileDescription = progDesc "Compile a .vcl file to an output target"

compileParser :: Parser CompileOptions
compileParser = CompileOptions
  <$> option auto
      ( long "inputFile"
     <> short 'i'
     <> help "Input .vcl file."
     <> metavar "FILENAME" )
  <*> optional (option auto
      ( long "outputFile"
     <> short 'o'
     <> help "Output location for compiled file. Defaults to stdout if not provided."
     <> metavar "FILENAME" ))
  <*> option auto
      ( long "target"
     <> short 't'
     <> help "Compilation target."
     <> metavar "TARGET" )
  <*> option auto
      ( long "moduleName"
     <> short 'm'
     <> help "The name of the module."
     <> metavar "MODULENAME" )

checkDescription :: InfoMod Command
checkDescription = progDesc "Check the verification status of a Vehicle property."

checkParser :: Parser CheckOptions
checkParser = CheckOptions
 <$> option auto
      ( long "databaseFile"
     <> short 'd'
     <> help "The database file for the Vehicle project."
     <> metavar "FILENAME" )
 <*> option auto
      ( long "property"
     <> short 'p'
     <> help "The UUID of the Vehicle property."
     <> metavar "UUID" )