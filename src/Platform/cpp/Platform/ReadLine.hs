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


import System.IO

#if (READLINE==2)
import qualified System.Console.Readline as R
#elif (READLINE==1)
-- nothing
#else
import qualified System.Console.Haskeline as R
import qualified System.Console.Haskeline.History as H
import Platform.Runtime( unsafePerformIO )
import Data.IORef
#endif

withReadLine :: FilePath -> IO a -> IO a
readLine     :: String -> IO (Maybe String)
readLineEx   :: String -> IO () -> IO (Maybe String)
addHistory   :: String -> IO ()


#if (READLINE==2) 

withReadLine historyFile io
  = do R.initialize 
       R.setCompletionEntryFunction (Just (\input -> return []))
       io

readLine prompt
  = do line <- readLines 
       return (Just line) 
  where
    readLines :: IO String
    readLines 
      = do mbline <- R.readline prompt
           let line = case mbline of Just s   -> s
                                     Nothing  -> ""
           case reverse line of
             []       -> readLines
             '\\' : t -> do line2 <- readLines
                            return $ (reverse t ++ "\n" ++ line2)
             _        -> return line

addHistory line
  = R.addHistory line

readLineEx prompt putPrompt
  = readLine prompt

#elif (READLINE==1)

withReadLine historyFile io
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

#else
  
vhistory :: IORef H.History
vhistory = unsafePerformIO $ newIORef H.emptyHistory
  
withReadLine historyPath io
  = do let historyFile = if (null historyPath) then "" else (historyPath ++ "/.readline")
       h0 <- if (null historyFile) then return H.emptyHistory else H.readHistory historyFile 
       writeIORef vhistory h0
       x <- io
       h1 <- readIORef vhistory
       if (null historyFile) then return () else H.writeHistory historyFile (H.stifleHistory (Just 20) h1)
       return x       

readLine prompt 
  = readLineEx prompt (do{ putStr prompt; hFlush stdout})

readLineEx prompt putPrompt
  = do putPrompt 
       h0 <- readIORef vhistory
       (mbline,h1) <- R.runInputT R.defaultSettings{R.autoAddHistory = False } $
                      do R.putHistory h0
                         line <- readLines
                         h1 <- R.getHistory
                         return (Just line, h1)
       writeIORef vhistory h1
       return mbline                         
  where
    readLines :: R.InputT IO String
    readLines 
      = do input <- R.getInputLine ""
           case input of
             Just line -> case reverse line of
                            []       -> readLines
                            '\\' : t -> do line2 <- readLines
                                           return (reverse t ++ "\n" ++ line2)
                            _        -> return line
             _ -> return ""

addHistory line
  = do h <- readIORef vhistory
       writeIORef vhistory (H.addHistory line h)
  
#endif
