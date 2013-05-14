{-# OPTIONS -cpp #-}
------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Module that exports readline functionality
-}
-----------------------------------------------------------------------------
module Platform.ReadLine( withReadLine, readLine, readLineEx, addHistory
                        ) where


#ifdef READLINE
import qualified READLINE as R
#else
import System.IO
#endif

withReadLine :: IO a -> IO a
readLine     :: String -> IO (Maybe String)
readLineEx   :: String -> IO () -> IO (Maybe String)
addHistory   :: String -> IO ()


#ifdef READLINE

withReadLine io
  = do R.initialize 
       R.setCompletionEntryFunction (Just (\input -> return []))
       io

readLine prompt
  = do line <- R.readline prompt
       case reverse line of
         []       -> readLine prompt
         '\\' : t -> do line2 <- readLine prompt
                        return (reverse t ++ "\n" ++ line2)
         _        -> return line

addHistory line
  = R.addHistory line

readLineEx prompt putPrompt
  = readLine prompt

#else

withReadLine io
  = io

readLine prompt
  = readLineEx prompt (do{ putStr prompt; hFlush stdout})

readLineEx prompt putPrompt
  = do s <- readLines
       return (Just s)
  where
    readLines
      = do putPrompt
           line <- getLine
           case reverse line of
             []       -> readLines
             '\\' : t -> do line2 <- readLines
                            return (reverse t ++ "\n" ++ line2)
             _        -> return line

addHistory line
  = return ()

#endif
