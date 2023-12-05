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
module Common.File(
                  -- * System
                    getEnvPaths, getEnvVar
                  , searchPaths, searchPathsSuffixes, searchPathsEx
                  , searchProgram
                  , runSystem, runSystemRaw, runCmd, runCmdRead, runCmdEnv
                  , getProgramPath

                  -- * Strings
                  , startsWith, endsWith, splitOn, trim

                  -- * File names
                  , FileName
                  , basename, notdir, notext, joinPath, joinPaths, extname, dirname, noexts
                  , splitPath, undelimPaths
                  , isPathSep, isPathDelimiter
                  , findMaximalPrefix
                  , isAbsolute
                  , commonPathPrefix
                  , normalizeWith, normalize
                  , isLiteralDoc
                  , ensureExt

                  -- * Files
                  , FileTime, fileTime0, maxFileTime, maxFileTimes
                  , fileTimeCompare, getFileTime
                  , getFileTimeOrCurrent, getCurrentTime
                  , readTextFile, writeTextFile
                  , copyTextFile, copyTextIfNewer, copyTextIfNewerWith, copyTextFileWith
                  , copyBinaryFile, copyBinaryIfNewer
                  , removeFileIfExists
                  , realPath
                  , doesFileExistAndNotEmpty
                  ) where

import Data.List        ( intersperse )
import Data.Char        ( toLower, isSpace )
import Platform.Config  ( pathSep, pathDelimiter, sourceExtension, exeExtension )
import qualified Platform.Runtime as B ( {- copyBinaryFile, -} exCatch )
import Common.Failure   ( raiseIO, catchIO )

import System.IO
import System.Process   ( system, rawSystem, createProcess, CreateProcess(..), proc, StdStream(..), waitForProcess )
import System.Exit      ( ExitCode(..) )
import System.Environment ( getEnvironment, getExecutablePath )
import System.Directory ( doesFileExist, doesDirectoryExist
                        {- , copyFile, copyFileWithMetadata -}
                        , getCurrentDirectory, getDirectoryContents
                        , createDirectoryIfMissing, canonicalizePath, removeFile )

import Lib.Trace
import Platform.Filetime

startsWith, endsWith :: String -> String -> Bool
startsWith s  [] = True
startsWith [] _  = False
startsWith (c:cs) (p:ps) = if (p==c) then startsWith cs ps else False

endsWith s post
  = startsWith (reverse s) (reverse post)

splitOn pred xs
  = normalize [] xs
  where
    normalize acc [] = reverse acc
    normalize acc xs
      = case (span (not . pred) xs) of
          (pre,post) -> normalize (pre:acc) (dropWhile pred post)


trim s
  = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace $ s

isLiteralDoc :: FileName -> Bool
isLiteralDoc fname
  = endsWith fname (sourceExtension ++ ".md") ||
    endsWith fname (sourceExtension ++ ".mdk")

{--------------------------------------------------------------------------
  File names
--------------------------------------------------------------------------}
-- | File name
type FileName = FilePath

-- | Remove the extension and directory part
basename :: FileName -> FileName
basename fname
  = case dropWhile (/='.') (reverse (notdir fname)) of
      '.':rbase -> reverse rbase
      _         -> notdir fname


-- | Get the file extension
extname :: FileName -> FileName
extname fname
  = let (pre,post) = span (/='.') (reverse (notdir fname))
    in if null post
        then ""
        else ("." ++ reverse pre)

ensureExt :: FileName -> String -> FileName
ensureExt fname ext
  = if (extname fname == ext) then fname else fname ++ ext

-- | Return the directory prefix (including last separator if present)
dirname :: FileName -> FileName
dirname fname
  = joinPaths (init (splitPath fname))

-- | Remove the directory prefix
notdir :: FileName -> FileName
notdir fname
  = last (splitPath fname)


notext :: FileName -> FileName
notext fname
  = reverse (drop (length (extname fname)) (reverse fname))

noexts :: FileName -> FileName
noexts fname
  = joinPaths [dirname fname, takeWhile (/='.') (notdir fname)]

-- | Split a (semi-)colon separated list of directories into a directory list
undelimPaths :: String -> [FilePath]
undelimPaths xs
  = filter (not . null) (normalize [] "" xs)
  where
    -- initial spaces
    normalize ps "" (c:cs)  | isSpace c
      = normalize ps "" cs
    -- directory on windows
    normalize ps "" (c:':':cs)
      = normalize ps (':':c:[]) cs
    -- normal
    normalize ps p xs
      = case xs of
          []     -> if (null p)
                     then reverse ps
                     else reverse (reverse p:ps)
          (c:cs) | isPathDelimiter c -> normalize (reverse p:ps) "" cs
                 | otherwise         -> normalize ps (c:p) cs

-- | Split a path into its directory parts
splitPath :: FilePath -> [FilePath]
splitPath fdir
  = let fs = filter (not . null) $ splitOn isPathSep fdir
    in case fdir of
         '/':_ -> "/":fs
         _ -> if (null fs) then [""] else fs


joinPath :: FilePath -> FilePath -> FilePath
joinPath p1 p2
  = joinPaths [p1,p2]

-- | Join a list of paths into one path
joinPaths :: [FilePath] -> FilePath
joinPaths dirs
  = concat
  $ filterPathSepDup
  $ intersperse ['/']
  $ normalize
  $ filter (not . null)
  $ concatMap splitPath dirs
  where
    normalize []            = []
    normalize (p:".":ps)    = normalize (p:ps)
    normalize (p:"..":ps)   | p == "."  = normalize ("..":ps)
                            | p == ".." = p : normalize ("..":ps)
                            | otherwise = normalize ps
    normalize (p:ps)        = p : normalize ps


    filterPathSepDup (p:q:rest)  | endsWith p "/" && q == "/"  = filterPathSepDup (p : rest)
    filterPathSepDup (p:ps)      = p : filterPathSepDup ps
    filterPathSepDup []          = []

-- | Normalize path separators
normalize :: FilePath -> FilePath
normalize path
  = normalizeWith '/' path

-- | Normalize path separators with a specified path separator
normalizeWith :: Char -> FilePath -> FilePath
normalizeWith newSep path
  = norm "" path
  where
    norm acc "" = reverse acc
    norm acc (c:cs)
      = if (isPathSep c)
         then norm (newSep:acc) (dropWhile isPathSep cs)
         else norm (c:acc) cs


-- | Is this a file separator.
isPathSep :: Char -> Bool
isPathSep c
  = (c == '/' || c == '\\')

-- | Is this a path delimiter? (@;@ (and @:@ too on unix)
isPathDelimiter :: Char -> Bool
isPathDelimiter c
  = (c == ';' || c == pathDelimiter)



{--------------------------------------------------------------------------
  system
--------------------------------------------------------------------------}

runSystemRaw :: String -> IO ()
runSystemRaw command
  = do -- putStrLn ("system: " ++ command)
       exitCode <- system command
       case exitCode of
         ExitFailure i -> raiseIO ("command failed:\n " ++ command )
         ExitSuccess   -> return ()

runSystem :: String -> IO ()
runSystem command
  = do -- putStrLn ("system: " ++ command)
       exitCode <- runSystemEx command
       case exitCode of
         ExitFailure i -> raiseIO ("command failed:\n " ++ command )
         ExitSuccess   -> return ()

runSystemEx command
  = system (normalizeWith pathSep command)

runCmd :: String -> [String] -> IO ()
runCmd cmd args
  = do exitCode <- rawSystem cmd args
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

-- | Compare two file modification times (uses 0 for non-existing files)
fileTimeCompare :: FilePath -> FilePath -> IO Ordering
fileTimeCompare fname1 fname2
  = do time1 <- getFileTime fname1
       time2 <- getFileTime fname2
       return (compare time1 time2)


maxFileTime :: FileTime -> FileTime -> FileTime
maxFileTime t1 t2
  = if (t1 >= t2) then t1 else t2

maxFileTimes :: [FileTime] -> FileTime
maxFileTimes times
  = foldr maxFileTime fileTime0 times

doesFileExistAndNotEmpty :: FilePath -> IO Bool
doesFileExistAndNotEmpty fpath
  = do mbContent <- readTextFile fpath
       case mbContent of
         Nothing      -> return False
         Just content -> return (not (null content))


readTextFile :: FilePath -> IO (Maybe String)
readTextFile fpath
  = B.exCatch (do content <- readFile fpath
                  return (if null content then Just content else (seq (last content) $ Just content)))
              (\exn -> -- trace ("reading file " ++ fpath ++ " exception: " ++ exn) 
                   return Nothing)

writeTextFile :: FilePath -> String -> IO ()
writeTextFile fpath content
  = writeFile fpath content

copyTextFile :: FilePath -> FilePath -> IO ()
copyTextFile src dest
  = copyTextFileWith src dest id
  {-
  = if (src == dest)
     then return ()
     else catchIO (do createDirectoryIfMissing True (dirname dest)
                      copyFileWithMetadata src dest)  -- do not use as the source may come from a (readonly) admin permission and should got to user permission
            (error ("could not copy file " ++ show src ++ " to " ++ show dest)) -}

copyTextFileWith :: FilePath -> FilePath -> (String -> String) -> IO ()
copyTextFileWith src dest transform
  = if (src == dest)
     then return ()
     else catchIO (do createDirectoryIfMissing True (dirname dest)
                      ftime   <- getFileTime src
                      content <- readFile src
                      writeFile dest (transform content)
                      setFileTime dest ftime)
            (error ("could not copy file " ++ show src ++ " to " ++ show dest))

copyBinaryFile :: FilePath -> FilePath -> IO ()
copyBinaryFile src dest
  = if (src == dest)
     then return ()
     else catchIO (
            -- do not use as the source may come from a (readonly) admin permission and should got to user permission
            -- B.copyBinaryFile src dest) (\_ -> error ("could not copy file " ++ show src ++ " to " ++ show dest))
             do createDirectoryIfMissing True (dirname dest)
                ftime <- getFileTime src
                withBinaryFile src ReadMode $ \hsrc ->
                  withBinaryFile dest WriteMode $ \hdest ->
                    do content <- hGetContents hsrc
                       hPutStr hdest content
                setFileTime dest ftime)
            (error ("could not copy file " ++ show src ++ " to " ++ show dest))

copyBinaryIfNewer :: Bool -> FilePath -> FilePath -> IO ()
copyBinaryIfNewer always srcName outName
  = do ord <- if (srcName == outName) then return EQ
              else if always then return GT
              else fileTimeCompare srcName outName
       if (ord == GT)
        then do copyBinaryFile srcName outName
        else do -- putStrLn $ "no copy for: " ++ srcName ++ " to " ++ outName
                return ()

copyTextIfNewer :: Bool -> FilePath -> FilePath -> IO ()
copyTextIfNewer always srcName outName
  = do ord <- if (srcName == outName) then return EQ
              else if always then return GT
              else fileTimeCompare srcName outName
       if (ord == GT)
        then do copyTextFile srcName outName
        else do return ()

copyTextIfNewerWith :: Bool -> FilePath -> FilePath -> (String -> String) -> IO ()
copyTextIfNewerWith always srcName outName transform
  = do ord <- if (srcName == outName) then return EQ
              else if always then return GT
              else fileTimeCompare srcName outName
       if (ord == GT)
        then do copyTextFileWith srcName outName transform
        else do return ()

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fname
  = B.exCatch (removeFile fname)
              (\exn -> return ())

getProgramPath :: IO FilePath
getProgramPath = getExecutablePath

commonPathPrefix :: FilePath -> FilePath -> FilePath
commonPathPrefix s1 s2
  = joinPaths $ map fst $ takeWhile (\(c,d) -> c == d) $ zip (splitPath s1) (splitPath s2)


-- | Is a path absolute?
isAbsolute :: FilePath -> Bool
isAbsolute fpath
  = case fpath of
      (_:':':c:_) -> isPathSep c
      ('/':_)     -> True
      _           -> False

-- | Find a maximal prefix given a string and list of prefixes. Returns the prefix and its length.
findMaximalPrefix :: [String] -> String -> Maybe (Int,String)
findMaximalPrefix xs s
  = findMaximal (\x -> if startsWith s x then Just (length x) else Nothing) xs

findMaximal :: (a -> Maybe Int) -> [a] -> Maybe (Int,a)
findMaximal f xs
  = normalize Nothing xs
  where
    normalize res []     = res
    normalize res (x:xs) = case (f x) of
                        Just n  -> case res of
                                     Just (m,y)  | m >= n -> normalize res xs
                                     _           -> normalize (Just (n,x)) xs
                        Nothing -> normalize res xs

---------------------------------------------------------------
-- file searching
----------------------------------------------------------------

searchPaths :: [FilePath] -> [String] -> String -> IO (Maybe (FilePath))
searchPaths path exts name
  = searchPathsSuffixes path exts [] name

searchPathsSuffixes :: [FilePath] -> [String] -> [String] -> String -> IO (Maybe (FilePath))
searchPathsSuffixes path exts suffixes name
  = fmap (fmap (\(root,name) -> joinPath root name)) (searchPathsEx path (filter (not.null) exts) suffixes name)


searchPathsEx :: [FilePath] -> [String] -> [String] -> String -> IO (Maybe (FilePath,FilePath))
searchPathsEx path exts suffixes name
  = search (concatMap (\dir -> map (\n -> (dir,n)) nameext) ("":path))
  where
    search [] = return Nothing  -- notfound envname nameext path
    search ((dir,fname):xs)
      = do{ let fullName = joinPath dir fname
          -- ; trace ("search: " ++ fullName) $ return ()
          ; exist <- doesFileExist fullName
          ; if exist
             then return (Just (dir,fname))
             else search xs
          }

    nameext
      = concatMap (\fname -> fname : map (fname++) exts) $
        map (\suffix -> (notext nname) ++ suffix ++ (extname nname)) ("" : suffixes)

    nname
      = joinPaths $ dropWhile (==".") $ splitPath name


getEnvPaths :: String -> IO [FilePath]
getEnvPaths name
  = do{ xs <- getEnvVar name
      ; return (undelimPaths xs)
      }
  `catchIO` \err -> return []

getEnvVar :: String -> IO String
getEnvVar name
  = do env <- getEnvironment
       case lookup (map toLower name) (map (\(k,v) -> (map toLower k,v)) env) of
         Just val -> return val
         Nothing  -> return ""

realPath :: FilePath -> IO FilePath
realPath fpath
  = canonicalizePath fpath


searchProgram :: FilePath -> IO (Maybe FilePath)
searchProgram ""
  = return Nothing
searchProgram fname | isAbsolute fname || fname `startsWith` "."
  = do exist <- doesFileExist fname
       if exist
         then return (Just fname)
         else return Nothing
searchProgram fname
  = do paths  <- getEnvPaths "PATH"
       searchPaths paths [exeExtension] fname


{-
splitPath :: String -> [String]
splitPath xs
  = normalize [] "" xs
  where
    normalize ps "" (c:':':cs)
      = normalize ps (':':c:[]) cs

    normalize ps p xs
      = case xs of
          []             -> if (null p)
                             then reverse ps
                             else reverse (reverse p:ps)
          (';':cs)       -> normalize (reverse p:ps) "" cs
          (':':cs)       -> normalize (reverse p:ps) "" cs
          (c:cs)         -> normalize ps (c:p) cs
-}
