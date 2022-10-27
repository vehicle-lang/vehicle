{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad (mapM, forM_, when)
import Data.List (isPrefixOf, isSuffixOf, dropWhile)
import Distribution.Simple (Args, UserHooks (buildHook, preConf), defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.Program (Program, runDbProgram, simpleProgram)
import Distribution.Simple.Setup (BuildFlags (buildVerbosity), ConfigFlags (configVerbosity), fromFlagOrDefault)
import Distribution.Simple.Utils (createDirectoryIfMissingVerbose, moreRecentFile, intercalate, die', notice, safeTail, takeWhileEndLE)
import Distribution.Types.LocalBuildInfo (LocalBuildInfo(LocalBuildInfo, withPrograms))
import Distribution.Types.PackageDescription (PackageDescription(PackageDescription, extraSrcFiles, extraTmpFiles))
import Distribution.Verbosity (Verbosity, normal)
import System.FilePath ((</>), makeRelative, splitDirectories, takeDirectory, takeBaseName)

srcDir :: FilePath
srcDir = "src"

autogenDir :: FilePath
autogenDir = "autogen"

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      {
        preConf = \args configFlags -> do
          makeAutogenDir args configFlags
          preConf simpleUserHooks args configFlags,
        buildHook = \packageDescription localBuildInfo userHooks buildFlags -> do
          preProcessBnfc packageDescription localBuildInfo userHooks buildFlags
          buildHook simpleUserHooks packageDescription localBuildInfo userHooks buildFlags
      }

makeAutogenDir :: Args -> ConfigFlags -> IO ()
makeAutogenDir args configFlags = do
  let verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
  notice verbosity $ "Create directory for generated modules: " ++ autogenDir
  createDirectoryIfMissingVerbose verbosity True autogenDir

preProcessBnfc :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
preProcessBnfc packageDescription localBuildInfo userHooks buildFlags = do
  let verbosity = fromFlagOrDefault normal (buildVerbosity buildFlags)
  let PackageDescription{extraSrcFiles, extraTmpFiles} = packageDescription
  let LocalBuildInfo{withPrograms} = localBuildInfo
  forM_ extraSrcFiles $ \extraSrcFile ->
    when (".cf" `isSuffixOf` extraSrcFile) $ do
      -- Example:
      --   extraSrcFile      = "src/Vehicle/Syntax/External.cf"
      --   outputPrefix      = "Vehicle/Syntax"
      --   namespacePrefix   = "Vehicle.Syntax"
      --   outputDir         = "autogen/Vehicle/Syntax/External"
      let outputPrefix = makeRelative srcDir (takeDirectory extraSrcFile)
      let namespacePrefix = intercalate "." (splitDirectories outputPrefix)
      let outputDir = autogenDir </> outputPrefix </> takeBaseName extraSrcFile
      let targetFiles = filter (outputDir `isPrefixOf`) extraTmpFiles
      shouldCompile <- and <$> mapM (extraSrcFile `moreRecentFile`) targetFiles
      when shouldCompile $ do
          notice verbosity
            $ unlines
            $ concat
            [ ["Compiling " ++ extraSrcFile ++ "to generate:"]
            , map ("- " ++) targetFiles
            ]
          runDbProgram verbosity bnfcProgram withPrograms
            $ concat
            [ [ "-d" ]
            , [ "--haskell" ]
            , [ "--generic" ]
            , [ "--text-token" ]
            , [ "--name-space=" ++ namespacePrefix | not (null namespacePrefix) ]
            , [ "--outputdir=" ++ autogenDir ]
            , [ extraSrcFile ]
            ]

bnfcProgram :: Program
bnfcProgram = simpleProgram "bnfc"
