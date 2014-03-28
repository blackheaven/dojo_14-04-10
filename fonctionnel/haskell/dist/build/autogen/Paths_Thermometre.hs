module Paths_Thermometre (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/black/.cabal/bin"
libdir     = "/home/black/.cabal/lib/x86_64-linux-ghc-7.6.3/Thermometre-0.1.0.0"
datadir    = "/home/black/.cabal/share/x86_64-linux-ghc-7.6.3/Thermometre-0.1.0.0"
libexecdir = "/home/black/.cabal/libexec"
sysconfdir = "/home/black/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Thermometre_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Thermometre_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Thermometre_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Thermometre_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Thermometre_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
