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


import Data.Char( isSpace, isAlphaNum )
import System.IO
import Control.Exception
import Common.ColorScheme

#if (READLINE==1)
-- readline
import qualified System.Console.Readline as R
#elif (READLINE==2)
-- getline
#elif (READLINE==3)
-- haskeline
import qualified System.Console.Haskeline as R
import qualified System.Console.Haskeline.History as H
import qualified System.Console.Haskeline.Completion as C
import Platform.Runtime( unsafePerformIO )
import Data.IORef
#else 
-- 0: repline
import System.Console.Isocline as Isocline
import Syntax.Highlight( highlightInput )
#endif

withReadLine :: FilePath -> IO a -> IO a
-- readLine     :: ColorScheme -> [FilePath] -> [String] -> [(String,String)] -> String -> IO (Maybe String)
readLineEx   :: ColorScheme -> [FilePath] -> [String] -> [(String,String)] -> String -> IO () -> IO (Maybe String)
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


-----------------------------------------------------------------------
-- GNU Readline
-----------------------------------------------------------------------

#if (READLINE==1)

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

#elif (READLINE==2)
-----------------------------------------------------------------------
-- No readline
-----------------------------------------------------------------------

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

#elif (READLINE==3)
-----------------------------------------------------------------------
-- Haskeline
-----------------------------------------------------------------------

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
  = do if (null prompt) then putPrompt else return ()
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
      = do input <- R.getInputLine prompt
           continueLine input (readLines (count+1))
    
addHistory line
  = do h <- readIORef vhistory
       writeIORef vhistory (H.addHistoryRemovingAllDupes line h)

completeLine :: [FilePath] -> [String] -> C.CompletionFunc IO
completeLine roots identifiers (rprev,prefix) | take 2 (dropWhile isSpace (reverse rprev)) `elem` [":l",":f",":e"]
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


#else  
-- 0
-----------------------------------------------------------------------
-- Isocline
-----------------------------------------------------------------------

addHistory entry
  = historyAdd entry

withReadLine historyPath io
  = do let historyFile = if (null historyPath) then "" else (historyPath ++ "/.koka-history")
       setHistory historyFile 200
       enableAutoTab True
       io

readLine cscheme roots identifiers options prompt
  = readLineEx cscheme roots identifiers options prompt (do{ putStr prompt; hFlush stdout})

readLineEx cscheme roots identifiers options prompt putPrompt
  = do setPromptMarker prompt ""
       Isocline.readlineExMaybe "" (Just (completer roots identifiers options)) (Just (highlighter cscheme))

    
completer :: [FilePath] -> [String] -> [(String,String)] -> CompletionEnv -> String -> IO ()
completer roots identifiers options cenv input
  = let inputx = dropWhile isSpace input
    in  if (take 2 inputx `elem` [":l",":f",":e"])
          then completeModules roots cenv input
        else if (take 4 inputx `elem` [":set"]) 
          then completeOptions options cenv input
          else completeIdentifiers identifiers cenv input

completeModules ::  [FilePath] -> CompletionEnv -> String ->IO ()
completeModules roots cenv input 
  = completeFileName cenv input (Just '/') (".":roots) [".kk"]

completeOptions :: [(String,String)] -> CompletionEnv -> String -> IO ()
completeOptions options cenv input
  = completeWord cenv input Nothing (completeOptionFlags options)

completeOptionFlags :: [(String,String)] -> String -> [Completion]
completeOptionFlags flags input
  = completionsFor input (map fst flags)  -- todo: integrate help

completeIdentifiers ::  [String] -> CompletionEnv -> String -> IO ()
completeIdentifiers names cenv input 
  = completeWord cenv input (Just isIdChar) (completeNames names) 
  where
    isIdChar :: Char -> Bool
    isIdChar c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '-' || c == '_' || (c >= '0' && c <= '9')

completeNames ::  [String] -> String -> [Completion]
completeNames names input
  = completionsFor input names

highlighter :: ColorScheme -> String -> Fmt
highlighter cscheme input
  = case (span isSpace input) of
      (prews,':':rest)  
        -> -- command
           case (span (\c -> isAlphaNum c || c `elem` "!?") rest) of  -- todo: add highlighting on command options
             (cmd,args) -> style (styleColor (colorCommand cscheme)) (prews ++ ":" ++ cmd) ++
                           style (styleColor (colorSource cscheme)) args
      _ -> -- expression
           highlightInput cscheme input  
  where
    styleColor :: Color -> Style
    styleColor color
      = case color of
          ColorDefault -> ""
          _            -> "ansi-color=" ++ show (fromEnum color)

{-
startsWith, endsWith :: String -> String -> Bool
startsWith s pre
  = take (length pre) s == pre

endsWith s post
  = startsWith (reverse s) (reverse post)
-}

#endif
