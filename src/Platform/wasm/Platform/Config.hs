{-# OPTIONS -cpp #-}
------------------------------------------------------------------------------
-- Copyright 2024, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Configuration data for WASM target.
-}
-----------------------------------------------------------------------------
module Platform.Config where

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
compilerBuildVariant = "wasm"
#endif

compiler :: String
compiler = "ghc-wasm"

exeExtension   :: String
dllExtension   :: String
objExtension   :: String
libExtension   :: String
libPrefix      :: String
pathSep        :: Char
pathDelimiter  :: Char

exeExtension  = ".wasm"
dllExtension  = ".wasm"
objExtension  = ".o"
libExtension  = ".a"
libPrefix     = "lib"
pathSep       = '/'
pathDelimiter = ':'

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
