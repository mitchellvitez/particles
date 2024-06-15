{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_fluidsim (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/mitchellvitez/.cabal/bin"
libdir     = "/Users/mitchellvitez/.cabal/lib/x86_64-osx-ghc-8.10.7/fluidsim-0.1.0.0-inplace-fluidsim"
dynlibdir  = "/Users/mitchellvitez/.cabal/lib/x86_64-osx-ghc-8.10.7"
datadir    = "/Users/mitchellvitez/.cabal/share/x86_64-osx-ghc-8.10.7/fluidsim-0.1.0.0"
libexecdir = "/Users/mitchellvitez/.cabal/libexec/x86_64-osx-ghc-8.10.7/fluidsim-0.1.0.0"
sysconfdir = "/Users/mitchellvitez/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "fluidsim_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "fluidsim_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "fluidsim_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "fluidsim_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "fluidsim_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "fluidsim_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
