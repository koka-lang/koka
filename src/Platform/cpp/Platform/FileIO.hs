-----------------------------------------------------------------------------
-- Copyright 2024, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Platform-specific file I/O primitives.
    The cpp variant delegates to System.Directory, System.Process, etc.

    This module provides LOW-LEVEL primitives only. Higher-level functions
    (copyTextFile, searchPaths, etc.) live in Common.File and call these.
-}
-----------------------------------------------------------------------------
module Platform.FileIO(
    -- * File existence and metadata
    doesFileExist
  , doesDirectoryExist
  , createDirectoryIfMissing
  , getFileSize
    -- * File reading and writing
  , readTextFile
  , writeTextFile
  , writeStringToFile
  , readBinaryContents
  , writeBinaryContents
  , copyBinaryContents
  , removeFileIfExists
    -- * Paths and environment
  , getCwd
  , realPath
  , getEnvVar
  , getEnvPaths
  , getProgramPath
    -- * Home/temp directories
  , getHomeDirectory
  , getTemporaryDirectory
    -- * Process execution
  , runSystem, runSystemRaw, runCmd, runCmdRead, runCmdEnv
  ) where

import System.IO
import System.Directory( doesFileExist, doesDirectoryExist, createDirectoryIfMissing
                       , getCurrentDirectory, canonicalizePath, removeFile
                       , getFileSize
                       , getHomeDirectory, getTemporaryDirectory )
import System.Process   ( system, rawSystem, createProcess, CreateProcess(..)
                        , proc, StdStream(..), waitForProcess )
import System.Exit      ( ExitCode(..) )
import System.Environment ( getEnvironment, getExecutablePath )
import Data.Char( toLower, isSpace )

import Common.Failure( raiseIO, catchIO )
import Platform.Config( pathSep, pathDelimiter, exeExtension )
import qualified Platform.Runtime as B ( exCatch )

-- NOTE: The following utility functions are duplicated from Common.File
-- to avoid circular imports (Common.File imports Platform.FileIO).
-- Keep in sync with Common.File: normalize, normalizeWith, onWindows, startsWith.

onWindows :: Bool
onWindows = (exeExtension == ".exe")

startsWith :: String -> String -> Bool
startsWith s  [] = True
startsWith [] _  = False
startsWith (c:cs) (p:ps) = if (p==c) then startsWith cs ps else False

normalize :: FilePath -> FilePath
normalize path
  = case normalizeWith '/' path of
      (c:':':'/':rest) -> [toLower c] ++ ":/" ++ rest
      npath            -> npath

normalizeWith :: Char -> FilePath -> FilePath
normalizeWith newSep path
  = norm "" path
  where
    norm acc "" = reverse acc
    norm acc ('\\':c:cs) | isSpace c = norm (c:'\\':acc) cs
    norm acc (c:cs) | c == '\\' || c == '/' = norm (newSep:acc) cs
    norm acc (c:cs) = norm (c:acc) cs

-- ── File reading/writing ─────────────────────────────────────────────────────

readTextFile :: FilePath -> IO (Maybe String)
readTextFile fpath
  = B.exCatch (do content <- readFile fpath
                  return (if null content then Just content else (seq (last content) $ Just content)))
              (\exn -> -- trace ("reading file " ++ fpath ++ " exception: " ++ exn)
                   return Nothing)

writeTextFile :: FilePath -> String -> IO ()
writeTextFile = Prelude.writeFile

writeStringToFile :: FilePath -> String -> IO ()
writeStringToFile fpath content
  = do h <- openFile fpath WriteMode
       hPutStr h content `B.exCatch` (\_ -> return ())
       hClose h

readBinaryContents :: FilePath -> IO String
readBinaryContents fpath
  = withBinaryFile fpath ReadMode hGetContents

writeBinaryContents :: FilePath -> String -> IO ()
writeBinaryContents fpath content
  = withBinaryFile fpath WriteMode (\h -> hPutStr h content)

-- | Copy binary file contents using nested handles to avoid lazy IO issues
copyBinaryContents :: FilePath -> FilePath -> IO ()
copyBinaryContents src dest
  = withBinaryFile src ReadMode $ \hsrc ->
      withBinaryFile dest WriteMode $ \hdest ->
        do content <- hGetContents hsrc
           hPutStr hdest content

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fname
  = B.exCatch (removeFile fname) (\_ -> return ())

-- ── Paths and environment ────────────────────────────────────────────────────

getCwd :: IO FilePath
getCwd
   = realPath "."

realPath :: FilePath -> IO FilePath
realPath fpath
  = do fullpath <- if onWindows && fpath `startsWith` "//"
                     then return fpath              -- on windows leave network paths alone as Haskell's `normalise` removes double `//` :-(
                     else canonicalizePath fpath
       return (normalize fullpath)

getEnvVar :: String -> IO String
getEnvVar name
  = do env <- getEnvironment
       case lookup (map toLower name) (map (\(k,v) -> (map toLower k, v)) env) of
         Just val -> return val
         Nothing  -> return ""

getEnvPaths :: String -> IO [FilePath]
getEnvPaths name
  = do{ xs <- getEnvVar name
      ; return (undelimPaths xs)
      }
  `catchIO` \_ -> return []

undelimPaths :: String -> [FilePath]
undelimPaths xs
  = filter (not . null) (undelim [] "" xs)
  where
    -- initial spaces
    undelim ps "" (c:cs)  | isSpace c
      = undelim ps "" cs
    -- directory on windows (e.g. C:)
    undelim ps "" (c:':':cs)
      = undelim ps (':':c:[]) cs
    -- normal
    undelim ps p xs
      = case xs of
          []     -> if (null p)
                     then reverse ps
                     else reverse (reverse p:ps)
          (c:cs) | c == ';' || c == pathDelimiter -> undelim (reverse p:ps) "" cs
                 | otherwise                      -> undelim ps (c:p) cs

getProgramPath :: IO FilePath
getProgramPath = getExecutablePath

-- ── Process execution ────────────────────────────────────────────────────────

runSystemRaw :: String -> IO ()
runSystemRaw command
  = do -- putStrLn ("system: " ++ command)
       exitCode <- system command
       case exitCode of
         ExitFailure i -> raiseIO ("raw command failed:\n " ++ command )
         ExitSuccess   -> return ()

runSystem :: String -> IO ()
runSystem command0
  = do -- putStrLn ("system: " ++ command)
       let command = normalizeWith pathSep command0
       exitCode <- system command
       case exitCode of
         ExitFailure i -> raiseIO ("command failed:\n " ++ command )
         ExitSuccess   -> return ()

runCmd :: String -> [String] -> IO ()
runCmd cmd args
  = do -- putStrLn ("run command: " ++ cmd ++ ", args: " ++ show args)
       exitCode <- rawSystem cmd args
       case exitCode of
          ExitFailure i -> raiseIO ("command failed (exit code " ++ show i ++ ")") -- \n  " ++ concat (intersperse " " (cmd:args)))
          ExitSuccess   -> return ()

runCmdRead :: [(String,String)] -> String -> [String] -> IO (String,String)
runCmdRead extraEnv cmd args
  = do mbEnv <- buildEnv extraEnv
       (_, Just hout, Just herr, process) <- createProcess (proc cmd args){ env = mbEnv, std_out = CreatePipe, std_err = CreatePipe }
       exitCode <- waitForProcess process
       case exitCode of
          ExitFailure i -> do -- hClose hout
                              raiseIO ("command failed (exit code " ++ show i ++ ")") -- \n  " ++ concat (intersperse " " (cmd:args)))
          ExitSuccess   -> do out <- hGetContents hout
                              err <- hGetContents herr
                              -- hClose hout
                              return (out,err)


runCmdEnv :: [(String,String)] -> String -> [String] -> IO ()
runCmdEnv extraEnv cmd args
  = do mbEnv <- buildEnv extraEnv
       (_, _, _, process) <- createProcess (proc cmd args){ env = mbEnv }
       exitCode <- waitForProcess process
       case exitCode of
          ExitFailure i -> do -- hClose hout
                              raiseIO ("command failed (exit code " ++ show i ++ ")") -- \n  " ++ concat (intersperse " " (cmd:args)))
          ExitSuccess   -> return ()

buildEnv :: [(String,String)] -> IO (Maybe [(String,String)])
buildEnv extraEnv
  = if null extraEnv then return Nothing
      else do oldEnv <- getEnvironment
              let newKeys = map fst extraEnv
              return (Just (extraEnv ++ filter (\(k,_) -> not (k `elem` newKeys)) oldEnv))
