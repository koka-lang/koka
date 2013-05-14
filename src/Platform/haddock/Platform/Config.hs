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

programName :: String
programName = "koka"

version :: String
version = "?"

exeExtension :: String
exeExtension = ""

pathSep :: Char
pathSep = '/'

pathDelimiter :: Char
pathDelimiter = ':'

sourceExtension :: String
sourceExtension = ".kk"

buildDate :: String
buildDate = ""

buildTime :: String
buildTime = ""

buildVariant :: String
buildVariant = "interpreted"

compiler :: String
compiler = "haddock"
