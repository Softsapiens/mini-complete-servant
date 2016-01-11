module Paths_mini_complete_servant (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/dani/Code/haskell/mini-complete-servant/.stack-work/install/x86_64-osx/lts-4.1/7.10.3/bin"
libdir     = "/Users/dani/Code/haskell/mini-complete-servant/.stack-work/install/x86_64-osx/lts-4.1/7.10.3/lib/x86_64-osx-ghc-7.10.3/mini-complete-servant-0.1.0.0-9v7ExUsDV4e5R9xUwMcoDy"
datadir    = "/Users/dani/Code/haskell/mini-complete-servant/.stack-work/install/x86_64-osx/lts-4.1/7.10.3/share/x86_64-osx-ghc-7.10.3/mini-complete-servant-0.1.0.0"
libexecdir = "/Users/dani/Code/haskell/mini-complete-servant/.stack-work/install/x86_64-osx/lts-4.1/7.10.3/libexec"
sysconfdir = "/Users/dani/Code/haskell/mini-complete-servant/.stack-work/install/x86_64-osx/lts-4.1/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "mini_complete_servant_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "mini_complete_servant_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "mini_complete_servant_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mini_complete_servant_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "mini_complete_servant_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
