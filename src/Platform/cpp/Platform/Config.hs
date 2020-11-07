{-# OPTIONS -cpp #-}
------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
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
#ifdef MAIN
programName = MAIN
#else
programName = "koka"
#endif

version :: String
#ifdef VERSION
version = VERSION
#else
version = "?"
#endif

buildVariant :: String
#ifdef VARIANT
buildVariant = VARIANT
#else
buildVariant = "interpreted"
#endif

compiler :: String
#ifdef COMPILER
compiler = COMPILER
#elif __GHCI__
compiler = "ghci"
#elif __GLASGOW_HASKELL__
compiler = "ghc"
#else
compiler = "unknown"
#endif

exeExtension   :: String
pathSep,pathDelimiter :: Char
#ifdef WINDOWS
exeExtension  = ".exe"
dllExtension  = ".dll"
objExtension  = ".obj"
libExtension  = ".lib"
libPrefix     = ""
pathSep       = '\\'
pathDelimiter = ';'
#elif defined(DARWIN) || defined(__MACOSX__) || defined(__MACH__)
dllExtension  = ".dylib"
objExtension  = ".o"
libExtension  = ".a"
libPrefix     = "lib"
exeExtension  = ""
pathSep       = '/'
pathDelimiter = ':'
#else
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
