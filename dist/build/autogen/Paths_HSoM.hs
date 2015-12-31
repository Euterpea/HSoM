module Paths_HSoM (
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
version = Version [1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Owner\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Owner\\AppData\\Roaming\\cabal\\i386-windows-ghc-7.10.3\\HSoM-1.0.0-56c7Fc3XZJk0d4DpEaxcwM"
datadir    = "C:\\Users\\Owner\\AppData\\Roaming\\cabal\\i386-windows-ghc-7.10.3\\HSoM-1.0.0"
libexecdir = "C:\\Users\\Owner\\AppData\\Roaming\\cabal\\HSoM-1.0.0-56c7Fc3XZJk0d4DpEaxcwM"
sysconfdir = "C:\\Users\\Owner\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "HSoM_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "HSoM_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "HSoM_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HSoM_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "HSoM_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
