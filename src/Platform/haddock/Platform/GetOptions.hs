------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Module that exports the GetOpt library in a portable way
-}
-----------------------------------------------------------------------------
module Platform.GetOptions( -- * GetOpt
                            getOpt, usageInfo
                          , ArgOrder(..)
                          , OptDescr(..)
                          , ArgDescr(..)
                          ) where

-- | What to do with options following non-options 
data ArgOrder a
  = RequireOrder  -- ^ no option processing after first non-option 
  | Permute       -- ^ freely intersperse options and non-options 
  | ReturnInOrder (String -> a) -- ^ wrap non-options into options

-- | Each OptDescr describes a single option.
-- The arguments to Option are:
--
--  * list of short option characters
--
--  * list of long option strings (without "--")
--
--  * argument descriptor
--
--  * explanation of option for user 
data OptDescr a 
  = Option [Char] [String] (ArgDescr a) String

-- | Describes whether an option takes an argument or not, and if so how the argument is injected into a value of type a.
data ArgDescr a
  = NoArg a
  | ReqArg (String -> a) String
  | OptArg (Maybe String -> a) String

-- | Process the command-line, and return the list of values that matched (and those that didn't). The arguments are:
--
--  *  The order requirements (see 'ArgOrder')
--
--  *  The option descriptions (see 'OptDescr')
--
--  *  The actual command line arguments (presumably got from 'getArgs'). 
--
-- 'getOpt' returns a triple consisting of the option arguments, a list of non-options, and a list of error messages. 
getOpt :: ArgOrder a -> [OptDescr a] -> [String] -> ([a], [String], [String])
getOpt = error "haddock.Platform.GetOpt: undefined"

-- | Return a string describing the usage of a command, derived from the header (first argument) and the options described by the second argument. 
usageInfo :: String -> [OptDescr a] -> String
usageInfo = error "haddock.Platform.usageInfo: undefined"
