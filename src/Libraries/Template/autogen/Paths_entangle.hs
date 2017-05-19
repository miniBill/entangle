module Paths_entangle (
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
version = Version {versionBranch = [0,1,1], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/linda/Library/Haskell/bin"
libdir     = "/Users/linda/Library/Haskell/ghc-7.8.3-x86_64/lib/entangle-0.1.1"
datadir    = "/Users/linda/Library/Haskell/share/ghc-7.8.3-x86_64/entangle-0.1.1"
libexecdir = "/Users/linda/Library/Haskell/libexec"
sysconfdir = "/Users/linda/Library/Haskell/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "entangle_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "entangle_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "entangle_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "entangle_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "entangle_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
