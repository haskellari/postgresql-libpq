#!/usr/bin/env runhaskell

import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Version

import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Verbosity

import Data.Char (isSpace)
import Data.List (dropWhile,reverse)

main = defaultMainWithHooks simpleUserHooks {
  confHook = \pkg flags -> do
    lbi <- confHook simpleUserHooks pkg flags
    bi <- psqlBuildInfo lbi

    return lbi {
      localPkgDescr = updatePackageDescription
                        (Just bi, [("runtests", bi)]) (localPkgDescr lbi)
    }
}

psqlBuildInfo :: LocalBuildInfo -> IO BuildInfo
psqlBuildInfo lbi = do
  (pgconfigProg, _) <- requireProgram verbosity
                         (simpleProgram "pg_config") (withPrograms lbi)
  let pgconfig = rawSystemProgramStdout verbosity pgconfigProg

  incDir <- pgconfig ["--includedir"]
  libDir <- pgconfig ["--libdir"]

  return emptyBuildInfo {
    extraLibDirs = [strip libDir],
    includeDirs  = [strip incDir]
  }
  where
    verbosity = normal -- honestly, this is a hack
    strip x = dropWhile isSpace $ reverse $ dropWhile isSpace $ reverse x
