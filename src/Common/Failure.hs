-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Internal errors and assertions.
-}
-----------------------------------------------------------------------------
module Common.Failure( failure, assertion, todo, matchFailure
                     , raise, raiseIO, catchIO
                     ) where

import Data.List       ( isPrefixOf )
import Data.Char       ( toLower, isSpace )
import Platform.Runtime( exCatch )
import Debug.Trace( trace, traceStack )
import Platform.Config( buildVariant )
assertion :: String -> Bool -> a -> a
assertion msg test x
  = if test
     then x
     else failure msg

failure :: String -> a
failure msg
  = raise ("*** internal compiler error: " ++ msg)

todo :: String -> a
todo msg
  = failure ("todo: " ++ msg)
  
matchFailure :: String -> a
matchFailure msg
  = failure ("unmatched pattern: " ++ msg)

raise :: String -> a
raise msg
  = if (buildVariant=="debug")
     then traceStack msg (error msg)
     else (error msg)

raiseIO :: String -> IO a
raiseIO msg
  = ioError (userError msg)

catchIO :: IO a -> (String -> IO a) -> IO a
catchIO io handler
  = exCatch io handler
  where
    adjust msg
      | isPrefixOf "user error:" (map toLower msg)  = skipColon msg
      | isPrefixOf "user error (" (map toLower msg) = init $ dropWhile isSpace (tail (dropWhile (/='(') msg)) 
      | isPrefixOf "user error\nReason:" msg        = skipColon msg
      | isPrefixOf "I/O error (user-defined)" msg   = skipColon msg
      | isPrefixOf "IO Error: User error\nReason:" msg = skipColon (tail (dropWhile (/=':') msg))
      | otherwise = msg
      where
        skipColon msg
          = dropWhile (\c -> isSpace c || c == '\n') (tail (dropWhile (/=':') msg))