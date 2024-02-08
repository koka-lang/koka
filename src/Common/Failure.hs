-----------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Internal errors and assertions.
-}
-----------------------------------------------------------------------------
module Common.Failure( failure, assertion, todo, matchFailure
                     , raise, raiseIO, catchIO, HasCallStack
                     ) where


import Data.List       ( isPrefixOf )
import Data.Char       ( toLower, isSpace )
import Platform.Runtime( exCatch )
import Debug.Trace( trace, traceStack )
import Platform.Config( compilerBuildVariant )
import GHC.Stack( HasCallStack )

assertion :: HasCallStack => String -> Bool -> a -> a
assertion msg test x
  = if test
     then x
     else failure msg

failure :: HasCallStack => String -> a
failure msg
  = raise msg

todo :: HasCallStack => String -> a
todo msg
  = failure ("todo: " ++ msg)

matchFailure :: HasCallStack => String -> a
matchFailure msg
  = failure ("unmatched pattern: " ++ msg)

raise :: HasCallStack => String -> a
raise msg
  = if (compilerBuildVariant=="debug")
     then traceStack msg (error msg)
     else (error msg)

raiseIO :: HasCallStack => String -> IO a
raiseIO msg
  = ioError (userError msg)

catchIO :: HasCallStack => IO a -> (String -> IO a) -> IO a
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