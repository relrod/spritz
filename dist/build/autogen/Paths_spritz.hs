module Paths_spritz (
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

bindir     = "/home/ricky/.cabal/bin"
libdir     = "/home/ricky/.cabal/lib/x86_64-linux-ghc-7.8.3/spritz-0.1.0.0"
datadir    = "/home/ricky/.cabal/share/x86_64-linux-ghc-7.8.3/spritz-0.1.0.0"
libexecdir = "/home/ricky/.cabal/libexec"
sysconfdir = "/home/ricky/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "spritz_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "spritz_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "spritz_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "spritz_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "spritz_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
