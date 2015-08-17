module Paths_listasFP (
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
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/andrei/.cabal/bin"
libdir     = "/home/andrei/.cabal/lib/listasFP-0.1/ghc-7.6.3"
datadir    = "/home/andrei/.cabal/share/listasFP-0.1"
libexecdir = "/home/andrei/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "listasFP_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "listasFP_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "listasFP_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "listasFP_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
