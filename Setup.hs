{-# LANGUAGE CPP #-}

#ifndef MIN_VERSION_Cabal
#define MIN_VERSION_Cabal(x,y,z) 0
#endif

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.PackageDescription
import Distribution.Version

import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Verbosity

import Data.Char (isSpace)
import Data.List (dropWhile,reverse)

#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Types.UnqualComponentName
#endif

flag :: String -> FlagName
#if MIN_VERSION_Cabal(2,0,0)
flag = mkFlagName
#else
flag = FlagName
#endif

#if MIN_VERSION_Cabal(2,0,0)
unqualComponentName :: String -> UnqualComponentName
unqualComponentName = mkUnqualComponentName
#else
unqualComponentName :: String -> String
unqualComponentName = id
#endif

main = defaultMainWithHooks simpleUserHooks {
  confHook = \pkg flags -> do
    if lookup (flag "use-pkg-config")
              (configConfigurationsFlags flags) == Just True
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
