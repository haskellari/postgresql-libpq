module Main (main) where

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.PackageDescription
import Distribution.Version

import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Verbosity

import Data.Char (isSpace)
import Data.List (dropWhile,reverse)

import Distribution.Types.UnqualComponentName

flag :: String -> FlagName
flag = mkFlagName

unqualComponentName :: String -> UnqualComponentName
unqualComponentName = mkUnqualComponentName

main :: IO ()
main = defaultMainWithHooks simpleUserHooks {
  confHook = \pkg flags -> do
    if lookup (flag "use-pkg-config")
              (unFlagAssignment (configConfigurationsFlags flags)) == Just True
    then do
      confHook simpleUserHooks pkg flags
    else do
      lbi <- confHook simpleUserHooks pkg flags
      bi <- psqlBuildInfo lbi

      return lbi {
        localPkgDescr = updatePackageDescription
                          (Just bi, [(unqualComponentName "runtests", bi)]) (localPkgDescr lbi)
      }
}

psqlBuildInfo :: LocalBuildInfo -> IO BuildInfo
psqlBuildInfo lbi = do
  (pgconfigProg, _) <- requireProgram verbosity
                         (simpleProgram "pg_config") (withPrograms lbi)
  let pgconfig = getProgramOutput verbosity pgconfigProg

  incDir <- pgconfig ["--includedir"]
  libDir <- pgconfig ["--libdir"]

  return emptyBuildInfo {
    extraLibDirs = [strip libDir],
    includeDirs  = [strip incDir]
  }
  where
    verbosity = normal -- honestly, this is a hack
    strip x = dropWhile isSpace $ reverse $ dropWhile isSpace $ reverse x
