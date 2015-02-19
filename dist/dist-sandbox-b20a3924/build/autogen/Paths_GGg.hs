module Paths_GGg (
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
version = Version {versionBranch = [0,1,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/xnil/GGg/.cabal-sandbox/bin"
libdir     = "/home/xnil/GGg/.cabal-sandbox/lib/x86_64-linux-ghc-7.8.4/GGg-0.1.0.1"
datadir    = "/home/xnil/GGg/.cabal-sandbox/share/x86_64-linux-ghc-7.8.4/GGg-0.1.0.1"
libexecdir = "/home/xnil/GGg/.cabal-sandbox/libexec"
sysconfdir = "/home/xnil/GGg/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "GGg_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "GGg_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "GGg_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "GGg_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "GGg_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
