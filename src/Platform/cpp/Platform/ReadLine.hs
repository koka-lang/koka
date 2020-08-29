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
import qualified System.Console.Haskeline.Completion as C
import Platform.Runtime( unsafePerformIO )
import Data.IORef
#endif

withReadLine :: FilePath -> IO a -> IO a
readLine     :: [FilePath] -> String -> IO (Maybe String)
readLineEx   :: [FilePath] -> String -> IO () -> IO (Maybe String)
addHistory   :: String -> IO ()


#if (READLINE==2) 

withReadLine historyFile io
  = do R.initialize 
       R.setCompletionEntryFunction (Just (\input -> return []))
       io

readLine roots prompt
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

readLineEx roots prompt putPrompt
  = readLine roots prompt

#elif (READLINE==1)

withReadLine historyFile io
  = io

readLine roots prompt
  = readLineEx roots prompt (do{ putStr prompt; hFlush stdout})

readLineEx roots prompt putPrompt
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
  = do let historyFile = if (null historyPath) then "" else (historyPath ++ "/.koka-history")
       h0 <- if (null historyFile) then return H.emptyHistory else H.readHistory historyFile 
       writeIORef vhistory h0
       x <- io
       h1 <- readIORef vhistory
       if (null historyFile) then return () else H.writeHistory historyFile (H.stifleHistory (Just 20) h1)
       return x       

readLine roots prompt 
  = readLineEx roots prompt (do{ putStr prompt; hFlush stdout})

readLineEx roots prompt putPrompt
  = do putPrompt 
       h0 <- readIORef vhistory
       (mbline,h1) <- R.runInputT (R.setComplete (completeModuleName roots) 
                                   (R.defaultSettings{R.autoAddHistory = False })) $
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
  
  
completeModuleName :: [FilePath] -> C.CompletionFunc IO
completeModuleName roots
  = C.completeQuotedWord (Just '\\') "\"'" (listModules roots) $ 
    C.completeWord (Just '\\') ("\"\'" ++ C.filenameWordBreakChars) (listModules roots)
    
listModules :: [FilePath] -> String -> IO [C.Completion]
listModules roots pre
  = do cs  <- C.listFiles pre
       css <- mapM (\root -> do cs <- C.listFiles (root ++ "/" ++ pre)
                                return [c{ C.replacement = drop (length root + 1) (C.replacement c) } | c <- cs]
                   ) roots  
       let norm s  = map (\c -> if (c=='\\') then '/' else c) s                 
       return [c{ C.replacement = norm (C.replacement c)} | 
               c <- (cs ++ concat css), 
               not (C.isFinished c) {-dir-} || take 3 (reverse (C.replacement c)) == "kk." ]
    
#endif
