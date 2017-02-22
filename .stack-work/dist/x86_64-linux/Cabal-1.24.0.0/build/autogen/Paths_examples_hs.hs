{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_examples_hs (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/pwoh/Dropbox/Thesis/notes/examples-hs/.stack-work/install/x86_64-linux/lts-6.30/7.10.3/bin"
libdir     = "/home/pwoh/Dropbox/Thesis/notes/examples-hs/.stack-work/install/x86_64-linux/lts-6.30/7.10.3/lib/x86_64-linux-ghc-7.10.3/examples-hs-0.1.0.0-4L77jDB8vjdHgiqyCVhmEO"
datadir    = "/home/pwoh/Dropbox/Thesis/notes/examples-hs/.stack-work/install/x86_64-linux/lts-6.30/7.10.3/share/x86_64-linux-ghc-7.10.3/examples-hs-0.1.0.0"
libexecdir = "/home/pwoh/Dropbox/Thesis/notes/examples-hs/.stack-work/install/x86_64-linux/lts-6.30/7.10.3/libexec"
sysconfdir = "/home/pwoh/Dropbox/Thesis/notes/examples-hs/.stack-work/install/x86_64-linux/lts-6.30/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "examples_hs_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "examples_hs_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "examples_hs_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "examples_hs_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "examples_hs_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
