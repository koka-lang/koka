-----------------------------------------------------------------------------
-- Copyright 2024, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Platform-specific file I/O primitives for WASM (WASI).
    WASI provides POSIX-like filesystem APIs, so most operations work.
    Process execution is not available in WASI.
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
                       , getFileSize )
import System.Exit( ExitCode(..) )
import Data.Char( toLower )

import Common.Failure( raiseIO, catchIO )
import Platform.Config( pathDelimiter )
import qualified Platform.Runtime as B ( exCatch )

-- ── File reading/writing (same as cpp — WASI provides these) ─────────────────

readTextFile :: FilePath -> IO (Maybe String)
readTextFile fpath
  = B.exCatch (do content <- Prelude.readFile fpath
                  return (if null content then Just content else (seq (last content) $ Just content)))
              (\_ -> return Nothing)

writeTextFile :: FilePath -> String -> IO ()
writeTextFile = Prelude.writeFile

writeStringToFile :: FilePath -> String -> IO ()
writeStringToFile = Prelude.writeFile

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

-- ── Paths and environment (WASI provides limited versions) ───────────────────

getCwd :: IO FilePath
getCwd = canonicalizePath "."

realPath :: FilePath -> IO FilePath
realPath fpath
  = do fullpath <- canonicalizePath fpath
       return (normalizePath fullpath)
  where
    normalizePath [] = []
    normalizePath ('\\':cs) = '/' : normalizePath cs
    normalizePath (c:cs) = c : normalizePath cs

-- WASI may or may not support environment variables depending on the runtime
getEnvVar :: String -> IO String
getEnvVar _ = return ""

getEnvPaths :: String -> IO [FilePath]
getEnvPaths _ = return []

getProgramPath :: IO FilePath
getProgramPath = return "/koka"

-- ── Home/temp directories (stubbed for WASI) ─────────────────────────────────

getHomeDirectory :: IO FilePath
getHomeDirectory = return "/"

getTemporaryDirectory :: IO FilePath
getTemporaryDirectory = return "/tmp"

-- ── Process execution (NOT available in WASI) ────────────────────────────────

runSystemRaw :: String -> IO ()
runSystemRaw _ = raiseIO "command execution not available in WASM/WASI"

runSystem :: String -> IO ()
runSystem _ = raiseIO "command execution not available in WASM/WASI"

runCmd :: String -> [String] -> IO ()
runCmd _ _ = raiseIO "command execution not available in WASM/WASI"

runCmdRead :: [(String,String)] -> String -> [String] -> IO (String,String)
runCmdRead _ _ _ = raiseIO "command execution not available in WASM/WASI"

runCmdReadExit :: [(String,String)] -> String -> [String] -> IO (ExitCode,String,String)
runCmdRead _ _ _ = raiseIO "command execution not available in WASM/WASI"

runCmdEnv :: [(String,String)] -> String -> [String] -> IO ()
runCmdEnv _ _ _ = raiseIO "command execution not available in WASM/WASI"
