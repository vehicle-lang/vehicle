{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad (forM_, when)
import Data.List (isSuffixOf, dropWhile)
import Distribution.Simple (Args, UserHooks (buildHook, preConf), defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.Program (Program, runDbProgram, simpleProgram)
import Distribution.Simple.Setup (BuildFlags (buildVerbosity), ConfigFlags (configVerbosity), fromFlagOrDefault)
import Distribution.Simple.Utils (createDirectoryIfMissingVerbose, intercalate, die', notice, safeTail, takeWhileEndLE)
import Distribution.Types.LocalBuildInfo (LocalBuildInfo(LocalBuildInfo, withPrograms))
import Distribution.Types.PackageDescription (PackageDescription(PackageDescription, extraSrcFiles))
import Distribution.Verbosity (Verbosity, normal)
import System.FilePath (makeRelative, splitDirectories, takeDirectory, takeBaseName)

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
  let PackageDescription{extraSrcFiles} = packageDescription
  let LocalBuildInfo{withPrograms} = localBuildInfo
  forM_ extraSrcFiles $ \extraSrcFile ->
    when (".cf" `isSuffixOf` extraSrcFile) $ do
      notice verbosity $ "Compile " ++ extraSrcFile
      let namespace = intercalate "."
                    $ splitDirectories
                    $ makeRelative srcDir
                    $ takeDirectory extraSrcFile
      let args = concat
            [ [ "-d" ]
            , [ "--haskell" ]
            , [ "--generic" ]
            , [ "--text-token" ]
            , [ "--name-space=" ++ namespace | not (null namespace) ]
            , [ "--outputdir=" ++ autogenDir ]
            , [ extraSrcFile ]
            ]
      runDbProgram verbosity bnfcProgram withPrograms args

bnfcProgram :: Program
bnfcProgram = simpleProgram "bnfc"
