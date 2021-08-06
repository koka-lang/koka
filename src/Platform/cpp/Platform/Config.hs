{-# OPTIONS -cpp #-}
------------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Configuration data
-}
-----------------------------------------------------------------------------
module Platform.Config where

-- by not inlining these we avoid rebuilding too many source files (since the .hi stays unchanged)
{-# NOINLINE version #-}
{-# NOINLINE buildDate #-}
{-# NOINLINE buildTime #-}

programName :: String
#if defined(KOKA_MAIN)
programName = KOKA_MAIN
#else
programName = "koka"
#endif

version :: String
#if defined(KOKA_VERSION)
version = KOKA_VERSION
#else
version = "0"
#endif

compilerBuildVariant :: String
#if defined(KOKA_VARIANT)
compilerBuildVariant = KOKA_VARIANT
#else
compilerBuildVariant = "interpreted"
#endif

compiler :: String
#if defined(KOKA_COMPILER)
compiler = KOKA_COMPILER
#elif __GHCI__
compiler = "ghci"
#elif __GLASGOW_HASKELL__
compiler = "ghc"
#else
compiler = "unknown"
#endif

exeExtension   :: String
pathSep,pathDelimiter :: Char

#if defined(WINDOWS)
-- platform      = "windows"
exeExtension  = ".exe"
dllExtension  = ".dll"
objExtension  = ".obj"
libExtension  = ".lib"
libPrefix     = ""
pathSep       = '\\'
pathDelimiter = ';'
#elif defined(DARWIN) || defined(__APPLE__) || defined(__MACH__) || defined(__MACOSX__)
-- platform      = "macos"
dllExtension  = ".dylib"
objExtension  = ".o"
libExtension  = ".a"
libPrefix     = "lib"
exeExtension  = ""
pathSep       = '/'
pathDelimiter = ':'
#else
-- platform      = "unix"
dllExtension  = ".so"
objExtension  = ".o"
libExtension  = ".a"
libPrefix     = "lib"
exeExtension  = ""
pathSep       = '/'
pathDelimiter = ':'
#endif

sourceExtension :: String
sourceExtension = ".kk"

buildDate :: String
#ifdef DATE
buildDate  = DATE
#else
buildDate  = __DATE__
#endif

buildTime :: String
buildTime  = __TIME__ ++ " " ++ __DATE__
