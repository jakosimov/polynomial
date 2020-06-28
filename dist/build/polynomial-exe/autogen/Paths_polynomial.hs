{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_polynomial (
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

bindir     = "/home/jakob/.cabal/bin"
libdir     = "/home/jakob/.cabal/lib/x86_64-linux-ghc-8.8.3/polynomial-0.1.0.0-HLeAczkB0K2CWPkdWqKNOk-polynomial-exe"
dynlibdir  = "/home/jakob/.cabal/lib/x86_64-linux-ghc-8.8.3"
datadir    = "/home/jakob/.cabal/share/x86_64-linux-ghc-8.8.3/polynomial-0.1.0.0"
libexecdir = "/home/jakob/.cabal/libexec/x86_64-linux-ghc-8.8.3/polynomial-0.1.0.0"
sysconfdir = "/home/jakob/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "polynomial_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "polynomial_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "polynomial_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "polynomial_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "polynomial_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "polynomial_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)