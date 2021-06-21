{-# OPTIONS -cpp #-}
{-# LANGUAGE ScopedTypeVariables #-}
------------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Module that exports readline functionality
-}
-----------------------------------------------------------------------------
module Platform.ReadLine( withReadLine, readLine, readLineEx, addHistory
                        ) where


import System.IO
import Control.Exception

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
readLine     :: [FilePath] -> [String] -> String -> IO (Maybe String)
readLineEx   :: [FilePath] -> [String] -> String -> IO () -> IO (Maybe String)
addHistory   :: String -> IO ()


continueLine :: Monad m => Maybe String -> m (Maybe String) -> m (Maybe String)
continueLine mbline readLines = do
  case mbline of
    Just line ->
      case reverse line of
        []       -> readLines
        '\\' : t -> do line2 <- readLines
                       return $ ((reverse t ++) . ("\n" ++)) <$> line2
        _        -> return $ Just line
    Nothing   -> return Nothing


#if (READLINE==2)

withReadLine historyFile io
  = do R.initialize
       R.setCompletionEntryFunction (Just (\input -> return []))
       io

readLine roots prompt
  = do line <- readLines
       return line
  where
    readLines :: IO (Maybe String)
    readLines
      = do mbline <- R.readline prompt
           continueLine mbline readLines

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
       return s
  where
    readLines
      = do putPrompt
           line <- catch (Just <$> getLine) (\(e :: SomeException) -> pure Nothing)
           continueLine line readLines

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
       if (null historyFile) then return () else H.writeHistory historyFile (H.stifleHistory (Just 64) h1)
       return x

readLine roots identifiers prompt
  = readLineEx roots identifiers prompt (do{ putStr prompt; hFlush stdout})

readLineEx roots identifiers prompt putPrompt
  = do putPrompt
       h0 <- readIORef vhistory
       (mbline,h1) <- R.runInputT (R.setComplete (completeLine roots identifiers)
                                   (R.defaultSettings{R.autoAddHistory = False })) $
                      do R.putHistory h0
                         line <- readLines 0
                         h1 <- R.getHistory
                         return (line, h1)
       writeIORef vhistory h1
       return mbline
  where
    readLines :: Int -> R.InputT IO (Maybe String)
    readLines count
      = do input <- R.getInputLine ""
           continueLine input (readLines (count+1))
    
addHistory line
  = do h <- readIORef vhistory
       writeIORef vhistory (H.addHistoryRemovingAllDupes line h)

completeLine :: [FilePath] -> [String] -> C.CompletionFunc IO
completeLine roots identifiers (rprev,prefix) | take 2 (reverse rprev) `elem` [":l",":f",":e"]
  = (C.completeQuotedWord (Just '\\') "\"'" (listModules roots) $
     C.completeWord (Just '\\') ("\"\'" ++ C.filenameWordBreakChars) (listModules roots)) (rprev,prefix)
completeLine roots identifiers (rprev,prefix) 
  = -- return (reverse prefix ++ rprev,[])
    (C.completeWord Nothing " \t.()[]{}" (listNames identifiers)) (rprev,prefix)


listNames :: [String] -> String -> IO [C.Completion]
listNames names prefix
  = return $ [C.Completion name name False | name <- names, name `startsWith` prefix]
    
listModules :: [FilePath] -> String -> IO [C.Completion]
listModules roots prefix 
  = do cs  <- C.listFiles prefix
       css <- mapM (\root -> do cs <- C.listFiles (root ++ "/" ++ prefix)
                                return [c{ C.replacement = drop (length root + 1) (C.replacement c) } | c <- cs]
                   ) roots
       let norm s  = map (\c -> if (c=='\\') then '/' else c) s
       return [c{ C.replacement = norm (C.replacement c)} |
               c <- (cs ++ concat css),
               not (C.isFinished c) {-dir-} || ((C.replacement c) `endsWith` ".kk") ]

startsWith, endsWith :: String -> String -> Bool
startsWith s pre
  = take (length pre) s == pre

endsWith s post
  = startsWith (reverse s) (reverse post)

#endif
