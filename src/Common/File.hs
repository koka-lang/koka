-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Internal errors and assertions.
-}
-----------------------------------------------------------------------------
module Common.File(
                  -- * System
                    getEnvPaths, getEnvVar
                  , searchPaths, searchPathsEx
                  , runSystem, runSystemRaw
                  , getProgramPath, getInstallDir

                  -- * Strings
                  , startsWith, endsWith, splitOn

                  -- * File names
                  , FileName
                  , basename, notdir, notext, joinPath, joinPaths, extname, dirname, noexts
                  , splitPath, undelimPaths
                  , isPathSep, isPathDelimiter
                  , findMaximalPrefix
                  , isAbsolute
                  , commonPathPrefix
                  , normalizeWith
                  , isLiteralDoc

                  -- * Files
                  , FileTime, fileTime0, maxFileTime, maxFileTimes
                  , fileTimeCompare, getFileTime
                  , getFileTimeOrCurrent, getCurrentTime
                  , copyTextFile, copyTextIfNewer, copyTextIfNewerWith, copyTextFileWith
                  , copyBinaryFile, copyBinaryIfNewer
                  ) where

import Data.List        ( intersperse )
import Data.Char        ( toLower, isSpace )
import Platform.Config  ( pathSep, pathDelimiter, sourceExtension )
import qualified Platform.Runtime as B ( copyBinaryFile )
import Common.Failure   ( raiseIO, catchIO )

import System.Process   ( system )
import System.Exit      ( ExitCode(..) )
import System.Environment ( getEnvironment, getProgName )
import System.Directory ( doesFileExist, doesDirectoryExist
                        , copyFile
                        , getCurrentDirectory, getDirectoryContents
                        , createDirectoryIfMissing, canonicalizePath )

import qualified Platform.Console as C (getProgramPath)
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
      _         -> fname


-- | Get the file extension
extname :: FileName -> FileName
extname fname
  = let (pre,post) = span (/='.') (reverse (notdir fname))
    in if null post
        then ""
        else ("." ++ reverse pre)

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
  $ intersperse [pathSep]
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
  = normalizeWith pathSep path

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
  = system (normalize command)

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

copyTextFile :: FilePath -> FilePath -> IO ()
copyTextFile src dest
  = if (src == dest)
     then return ()
     else catchIO (do createDirectoryIfMissing True (dirname dest)
                      copyFile src dest)
            (error ("could not copy file " ++ show src ++ " to " ++ show dest))

copyTextFileWith :: FilePath -> FilePath -> (String -> String) -> IO ()
copyTextFileWith src dest transform
  = if (src == dest)
     then return ()
     else catchIO (do createDirectoryIfMissing True (dirname dest)
                      content <- readFile src
                      writeFile dest (transform content))
            (error ("could not copy file " ++ show src ++ " to " ++ show dest))

copyBinaryFile :: FilePath -> FilePath -> IO ()
copyBinaryFile src dest
  = if (src == dest)
     then return ()
     else catchIO (B.copyBinaryFile src dest) (\_ -> error ("could not copy file " ++ show src ++ " to " ++ show dest))

copyBinaryIfNewer :: Bool -> FilePath -> FilePath -> IO ()
copyBinaryIfNewer always srcName outName
  = do ord <- if always then return GT else fileTimeCompare srcName outName
       if (ord == GT)
        then do copyBinaryFile srcName outName
        else do -- putStrLn $ "no copy for: " ++ srcName ++ " to " ++ outName
                return ()

copyTextIfNewer :: Bool -> FilePath -> FilePath -> IO ()
copyTextIfNewer always srcName outName
  = do ord <- if always then return GT else fileTimeCompare srcName outName
       if (ord == GT)
        then do copyTextFile srcName outName
        else do return ()

copyTextIfNewerWith :: Bool -> FilePath -> FilePath -> (String -> String) -> IO ()
copyTextIfNewerWith always srcName outName transform
  = do ord <- if always then return GT else fileTimeCompare srcName outName
       if (ord == GT)
        then do copyTextFileWith srcName outName transform
        else do return ()


getInstallDir :: IO FilePath
getInstallDir
  = do p <- getProgramPath
       let d  = dirname p
           ds = splitPath d
           result = case reverse ds of
                      ("bin":es)   -> joinPaths (reverse es)
                      (_:"out":es) -> joinPaths (reverse es)
                      _            -> d
       -- trace ("install-dir: " ++ result ++ ": " ++ show ds) $
       return result



getProgramPath :: IO FilePath
getProgramPath
  = do p <- C.getProgramPath  -- works on windows
       if (not (null p))
        then return p
        else do name <- getProgName
                if (null name)
                 then return "main"
                 else if (any isPathSep name)
                  then return name
                  else do paths <- getEnvPaths "PATH"
                          mbp   <- searchPaths paths [] name  -- search along the PATH
                          case mbp of
                            Just fname -> return fname
                            Nothing    -> return name


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
  = fmap (fmap (\(root,name) -> joinPath root name)) (searchPathsEx path exts name)

searchPathsEx :: [FilePath] -> [String] -> String -> IO (Maybe (FilePath,FilePath))
searchPathsEx path exts name
  = search (concatMap (\dir -> map (\n -> (dir,n)) nameext) ("":path))
  where
    search [] = return Nothing  -- notfound envname nameext path
    search ((dir,fname):xs)
      = do{ let fullName = joinPath dir fname
          ; exist <- doesFileExist fullName
          ; if exist
             then return (Just (dir,fname))
             else search xs
          }

    nameext
      = (nname : map (nname++) exts)

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
