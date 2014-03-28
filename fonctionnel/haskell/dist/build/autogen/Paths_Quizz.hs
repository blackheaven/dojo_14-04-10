module Paths_Quizz (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/black/.cabal/bin"
libdir     = "/home/black/.cabal/lib/Quizz-0.1.0.0/ghc-7.6.3"
datadir    = "/home/black/.cabal/share/Quizz-0.1.0.0"
libexecdir = "/home/black/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "Quizz_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Quizz_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Quizz_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Quizz_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
