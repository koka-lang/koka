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


import IO

withReadLine :: IO a -> IO a
readLine     :: String -> IO (Maybe String)
addHistory   :: String -> IO ()

withReadLine io
  = io

readLine prompt
  = readLineEx prompt (do{ putStr prompt; hFlush stdout})

readLineEx prompt putPrompt
  = do s <- getLines
       return (Just s)
  where
    getLines
      = do putPrompt
           line <- getLine
           case reverse line of
             []       -> getLines
             '\\' : t -> do line <- getLines
                            return (reverse t ++ "\n" ++ line)
             _        -> return line

addHistory line
  = return ()

